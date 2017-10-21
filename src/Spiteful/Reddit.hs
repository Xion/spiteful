{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spiteful.Reddit where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid ((<>))
import qualified Network.HTTP.Client.TLS as TLS
import Pipes
import Reddit hiding (Options, upvotePost)
import qualified Reddit as R
import Reddit.Types.Comment
import Reddit.Types.Listing
import Reddit.Types.Post (Post(..), PostID)

import Spiteful.Logging
import Spiteful.Options
import Spiteful.Util


type EitherR = Either (APIError RedditError)


-- Posts

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
      let fOpts = newFetchOptions opts after
      Listing _ after' posts <- getPosts' fOpts listingType optSubreddit
      logFmt Debug "Received {} posts from the {} feed (after = {})"
                    (length posts, tshow listingType, tshow after)
      return (posts, after')
    case result of
      Left (err, _) -> return $ Left err
      Right (posts, after') -> do
        mapM_ yield posts
        let isEmpty = null posts && isNothing after -- not after'
        if isEmpty then return $ Right ()
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


upvotePost :: MonadIO m => Options -> Post -> m (EitherR ())
upvotePost opts Post{..} = do
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
        R.upvotePost postID
      case result of
        Left err -> logFmt Warn "Failed to upvote post #{}: {}" (pid, tshow err)
        Right _ -> logFmt Debug "Successfully upvoted post #{} in /r/{}"
                                (pid, subr)
      return result


-- Comments

type CommentProducer m = Producer Comment m (EitherR ())

fetchNewComments :: MonadIO m => Options -> CommentProducer m
fetchNewComments opts@Options{..} = do
  logAt Debug $ "Fetching new comments from " <>
    maybe "the entire Reddit" (("subreddit: " <>) . tshow) optSubreddit
  redditOpts <- newRedditOptions opts
  doFetch redditOpts Nothing
  where
  doFetch :: MonadIO m => RedditOptions -> Maybe CommentID -> CommentProducer m
  doFetch rOpts after = do
    result <- liftIO $ runResumeRedditWith rOpts $ do
      let fOpts = newFetchOptions opts after
      Listing _ after' comments <- getNewComments' fOpts optSubreddit
      logFmt Debug "Received {} new comments (after = {})"
                    (length comments, tshow after)
      return (comments, after')
    case result of
      Left (err, _) -> return $ Left err
      Right (comments, after') -> do
        mapM_ yield comments
        let isEmpty = null comments && isNothing after -- not after'
        if isEmpty then return $ Right ()
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
              logAt Debug $ "Exhausted the comment listing, starting over in "
                            <> tshow restartDelaySecs <> " seconds"
              return restartDelaySecs
          liftIO $ threadDelay (delaySecs * 1000000)
          doFetch rOpts after'

  restartDelaySecs = 5

fetchCommentReplies :: MonadIO m => Options -> Comment -> CommentProducer m
fetchCommentReplies opts@Options{..} Comment{..} = do
  let Listing _ after commentRefs = replies
  if null commentRefs && isNothing after then return $ Right ()
  else do
    logFmt Trace "Looking up replies to comment #{} from /r/{}" (cid, subr)
    redditOpts <- newRedditOptions opts

    -- TODO: follow the possible "after" in the Listing below;
    -- it's not very necessary for the purpose this whole function is used yet
    let Listing _ _ commentReplies = replies
    resolveCommentRefs redditOpts commentReplies
  where
  CommentID cid = commentID
  R subr = subreddit

  resolveCommentRefs :: MonadIO m
                     => RedditOptions -> [CommentReference]
                     -> CommentProducer m
  resolveCommentRefs rOpts commentRefs = do
    -- CommentReference may hide multiple unresolved CommentIDs;
    -- gather them all in a single list
    let justSingularRefs = concatMap treeSubComments commentRefs
        unresolved = [ cid | Reference _ [CommentID cid] <- justSingularRefs ]

    -- Resolve those references, getting a HashMap from IDs to Comments
    result <- liftIO $ runResumeRedditWith rOpts $ do
      let chunked = chunksOf singleCommentGetLimit unresolved
      resolved <- (concat <$>) . forM chunked $ \cids -> do
        -- TODO: handle the possible (but unlikely) `after` in the response
        Listing _ _ comments <- getCommentsInfo (map CommentID cids)
        return comments
      return $ HM.fromList (zip unresolved resolved)

    -- Use it to translate all CommentReferences to Comments
    case result of
      Left (err, _) -> return $ Left err
      Right refMap -> do
        let resolveRef = \case
              Actual c -> Just c
              Reference _ [CommentID cid] -> HM.lookup cid refMap
              _ -> Nothing
        forM_ (mapMaybe resolveRef commentRefs) $
          yield
        return $ Right ()
    where
    singleCommentGetLimit = 100 -- enforced by Reddit API


upvoteComment :: MonadIO m => Options -> Comment -> m (EitherR ())
upvoteComment opts Comment{..} = do
  let CommentID cid = commentID
      R subr = subreddit
  case optCredentials opts of
    Nothing -> do
      logFmt Warn
        "Cannot upvote comment #{} in /r/{} due to lack of credentials"
        (cid, subr)
      return $ Left $ APIError CredentialsError
    _ -> do
      logFmt Info "Upvoting comment #{} in /r/{} [{}] -> [{}]"
        (cid, subr, showScore score, showScore $ (+1) <$> score)
      redditOpts <- newRedditOptions opts
      result <- liftIO $ runRedditWith redditOpts $
        R.upvoteComment commentID
      case result of
        Left err ->
          logFmt Warn "Failed to upvote comment #{}: {}" (cid, tshow err)
        Right _ -> logFmt Debug "Successfully upvoted comment #{} in /r/{}"
                                (cid, subr)
      return result
  where
  -- Current score may be absent in the Comment record
  showScore = fromMaybe "?" . (tshow <$>)


-- Utilities

newRedditOptions :: MonadIO m => Options -> m RedditOptions
newRedditOptions opts = do
  -- Reuse HTTP connection manager between Reddit calls.
  httpManager <- liftIO TLS.getGlobalManager
  let redditOpts = toRedditOptions opts
  return redditOpts { connectionManager = Just httpManager}

newFetchOptions :: Options -> Maybe a -> R.Options a
newFetchOptions Options{..} after =
  R.Options { pagination = After <$> after, limit = optBatchSize }
