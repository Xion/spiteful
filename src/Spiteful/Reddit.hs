{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spiteful.Reddit where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Network.HTTP.Client.TLS as TLS
import Pipes
import Reddit hiding (Options)
import Reddit.Types.Listing
import Reddit.Types.Post

import Spiteful.Logging
import Spiteful.Options
import Spiteful.Util


newRedditOptions :: MonadIO m => Options -> m RedditOptions
newRedditOptions opts = do
  -- Reuse HTTP connection manager between Reddit calls.
  httpManager <- liftIO TLS.getGlobalManager
  let redditOpts = toRedditOptions opts
  return redditOpts { connectionManager = Just httpManager}


type EitherR = Either (APIError RedditError)


type PostProducer m = Producer Post m (EitherR ())

-- TODO: remember the IDs of posts already fetched so we can not yield
-- duplicates and stop/restart from the beginning of the listing if duplicates
-- are encountered
fetchPosts :: MonadIO m => Options -> PostProducer m
fetchPosts opts@Options{..} = do
  logAt Debug $ "Fetching " <> tshow listingType <> " posts from " <>
    maybe "the entire Reddit" (("subreddit: " <>) . tshow) optSubreddit
  redditOpts <- newRedditOptions opts
  doFetch redditOpts Nothing
  where
  listingType = fromMaybe New optListing

  doFetch :: MonadIO m => RedditOptions -> Maybe PostID -> PostProducer m
  doFetch rOpts after = do
    result <- liftIO $ runResumeRedditWith rOpts $ do
      let postOpts = def { pagination = After <$> after, limit = optBatchSize }
      Listing _ after' posts <- getPosts' postOpts listingType optSubreddit
      logFmt Debug "Received {} posts from the {} feed (after = {})"
                    (length posts, tshow listingType, tshow after)
      return (posts, after')
    case result of
      Left (err, _) -> return $ Left err
      Right (posts, after') -> do
        mapM_ yield posts
        if null posts then return $ Right ()
        else do
          -- Sleep for 1 second, as per Reddit API recommendation,
          -- or for longer if we exhausted the listing
          -- (as it likely means starting from the beginning of it,
          --  i.e. completely new posts)
          delaySecs <- case after' of
            Just _ -> do
              logAt Trace "Sleeping for a second before continuing with fetch"
              return 1
            Nothing -> do
              logFmt Debug
                "Exhausted the {} listing, starting over in {} seconds"
                (tshow listingType, restartDelaySecs)
              return restartDelaySecs
          liftIO $ threadDelay (delaySecs * 1000000)
          doFetch rOpts after'
    where
    -- | How long to wait when we've exhausted the listing before
    -- retrying the fetch anew from the beginning.
    restartDelaySecs = case listingType of
      New -> 5
      Hot -> 60
      Rising -> 30
      Controversial -> 60
      Top -> 5 * 60  -- /top shouldn't really change a lot


upvote :: MonadIO m => Options -> Post -> m (EitherR ())
upvote opts Post{..} = do
  let PostID pid = postID
      R subr = subreddit
  case optCredentials opts of
    Nothing -> do
      logFmt Warn
        "Cannot upvote post #{} (\"{}\" in /r/{}) due to lack of credentials"
        (pid, title, subr)
      return $ Left $ APIError CredentialsError
    _ -> do
      logFmt Info "Upvoting post #{} (\"{}\" in /r/{}) [{}] -> [{}]"
        (pid, title, subr, score, score + 1)
      redditOpts <- newRedditOptions opts
      result <- liftIO $ runRedditWith redditOpts $
        upvotePost postID
      case result of
        Left err -> logFmt Warn "Failed to upvote post #{}: {}" (pid, tshow err)
        Right _ -> logFmt Debug "Successfully upvoted post #{} in /r/{}"
                                (pid, subr)
      return result
