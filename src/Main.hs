{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Either.Combinators (whenLeft)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import Reddit hiding (Options)
import Reddit.Types.Listing
import Reddit.Types.Post
import System.IO

import Spiteful.Options
import Spiteful.Logging
import Spiteful.Util


main :: IO ()
main = do
  opts <- execParser commandLine

  logAt Info $ "spiteful-bot v" <> botVersion
  forM_ [stdout, stdin] $ \handle -> do
    hSetBuffering handle NoBuffering
    hSetEncoding handle utf8

  logAt Debug $ "Command line options: " <> (tshow opts)

  TLS.setGlobalManager =<< TLS.newTlsManager

  result <- runEffect $ fetchPosts opts
                      >-> P.filter isDontUpvotePost
                      >-> P.mapM_ (upvote opts)
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching posts: " <> tshow err

upvote :: MonadIO m => Options -> Post -> m ()
upvote opts Post{..} = do
  let PostID pid = postID
      R subr = subreddit
  case optCredentials opts of
    Nothing -> do
      logFmt Warn
        "Cannot upvote post #{} (\"{}\" in /r/{}) due to lack of credentials"
        (pid, title, subr)
    _ -> do
      logFmt Info "Upvoting post #{} (\"{}\" in /r/{}) [{}] -> [{}]"
        (pid, title, subr, score, score + 1)
      result <- liftIO $ runRedditWith (toRedditOptions opts) $
        upvotePost postID
      whenLeft result $ \err ->
        logFmt Warn "Failed to upvote post #{}: {}" (pid, tshow err)



-- Reddit stuff
-- TODO: extract this to a separate module

type PostProducer m = Producer Post m (Either (APIError RedditError) ())

-- TODO: remember the IDs of posts already fetched so we can not yield
-- duplicates and stop/restart from the beginning of the listing if duplicates
-- are encountered
fetchPosts :: MonadIO m => Options -> PostProducer m
fetchPosts opts@Options{..} = do
  -- Reuse HTTP connection manager between Reddit calls.
  let redditOpts = toRedditOptions opts
  httpManager <- liftIO $ HTTP.newManager TLS.tlsManagerSettings
  let redditOpts' = redditOpts { connectionManager = Just httpManager}

  logAt Debug $ "Fetching " <> tshow listingType <> " posts from " <>
    maybe "the entire Reddit" (("subreddit: " <>) . tshow) optSubreddit
  doFetch httpManager redditOpts' Nothing
  where
  listingType = fromMaybe New optListing

  doFetch :: MonadIO m
          => HTTP.Manager -> RedditOptions -> Maybe PostID -> PostProducer m
  doFetch manager rOpts after = do
    result <- liftIO $ runRedditWith rOpts $ do
      let postOpts = def { pagination = After <$> after, limit = optBatchSize }
      Listing _ after' posts <- getPosts' postOpts listingType optSubreddit
      logFmt Debug "Received {} posts from the {} feed (after = {})"
                   (length posts, tshow listingType, tshow after)
      return (posts, after')
    case result of
      Left err -> return $ Left err
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
          doFetch manager rOpts after'
    where
    -- | How long to wait when we've exhausted the listing before
    -- retrying the fetch anew from the beginning.
    restartDelaySecs = case listingType of
      New -> 5
      Hot -> 60
      Rising -> 30
      Controversial -> 60
      Top -> 5 * 60  -- /top shouldn't really change a lot

isDontUpvotePost :: Post -> Bool
isDontUpvotePost Post{..} = any (`Text.isInfixOf` title') phrases
  where
  title' = toPlain title
  phrases = map toPlain [ "don't upvote"
                        , "dont upvote"
                        , "no upvote"
                        , "not upvote"
                        ]
  toPlain = Text.toCaseFold . stripAccents . collapseWhitespace
  collapseWhitespace = Text.unwords . Text.words
