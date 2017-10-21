{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (throwTo)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either.Combinators (isRight, whenLeft)
import Data.Maybe (isJust, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import Reddit.Types.Comment
import Reddit.Types.Post
import Reddit.Types.Subreddit (SubredditName(..))
import Reddit.Types.User (Username(..))
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Signal (installHandler, sigINT)

import Spiteful.Options
import Spiteful.Logging
import Spiteful.Reddit
import Spiteful.Util


main :: IO ()
main = do
  forM_ [stdout, stdin] $ \handle -> do
    hSetBuffering handle NoBuffering
    hSetEncoding handle utf8

  opts@Options{..} <- parseArgs
  setLogLevel $ toEnum $ fromEnum defaultLogLevel - optVerbosity

  tid <- myThreadId
  installHandler sigINT $ \_ -> do
    -- TODO: some way of printing statistics w/o closing the program
    printStatistics
    throwTo tid ExitSuccess

  logAt Info $ "spiteful-bot v" <> botVersion
  logAt Debug $ "Command line options: " <> tshow opts

  TLS.setGlobalManager =<< TLS.newTlsManager

  -- TODO: pick what to run based on command line arguments
  mapConcurrently_ ($ opts) [ monitorDontUpvotePosts
                            , monitorDAEComments
                            ]


parseArgs :: IO Options
parseArgs = do
  opts <- execParser commandLine

  -- Handle possible "-" value being passed to --user or --password
  case optCredentials opts of
    Just ("-", "-") -> do
      username <- prompt EchoOn userPrompt
      password <- prompt EchoOff passPrompt
      return opts { optCredentials = Just (username, password) }
    Just (u, "-") -> do
      password <- prompt EchoOff passPrompt
      return opts { optCredentials = Just (u, password) }
    Just ("-", p) -> do
      username <- prompt EchoOn userPrompt
      return opts { optCredentials = Just (username, p) }
    _ -> return opts
  where
  userPrompt = "Reddit username"
  passPrompt = "Reddit password"


-- Upvoting "don't upvote" posts

monitorDontUpvotePosts :: Options -> IO ()
monitorDontUpvotePosts opts = do
  result <- runEffect $
    fetchPosts opts >-> countAs postsSeen
    >-> P.filter isDontUpvotePost >-> countAs dontUpvotePostsSeen
    >-> P.filter (not . hasBeenVotedOn)
    >-> P.mapM (upvotePost opts) >-> P.filter isRight >-> countAs postsUpvoted
    >-> P.drain -- drop upvote outcomes but retain a possible fetch error
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching posts: " <> tshow err

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

hasBeenVotedOn :: Post -> Bool
hasBeenVotedOn = isJust . liked


postsSeen :: MVar Int
postsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE postsSeen #-}

dontUpvotePostsSeen :: MVar Int
dontUpvotePostsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE dontUpvotePostsSeen #-}

postsUpvoted :: MVar Int
postsUpvoted = unsafePerformIO $ newMVar 0
{-# NOINLINE postsUpvoted #-}


-- Replying "no" to "does anyone else" comments

monitorDAEComments :: Options -> IO ()
monitorDAEComments opts = do
  result <- runEffect $
    fetchNewComments opts >-> countAs commentsSeen
    >-> P.filter isDAEComment >-> countAs daeCommentsSeen
    >-> P.filterM ((not <$>) . hasCommentBeenRepliedTo opts)
    >-> P.mapM (replyToDAEComment opts) >-> P.filter isRight
                                        >-> countAs commentsRepliedTo
    >-> P.drain
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching comments: " <> tshow err

isDAEComment :: Comment -> Bool
isDAEComment Comment{..} = False -- TODO

hasCommentBeenRepliedTo :: Options -> Comment -> IO Bool
hasCommentBeenRepliedTo opts _ | isNothing (optCredentials opts) = return False
hasCommentBeenRepliedTo opts comment@Comment{..} = do
  (hasReply, fetchResult) <- any' isOurReply $ fetchCommentReplies opts comment
  case fetchResult of
    Left err -> do
      let CommentID cid = commentID
          R subr = subreddit
      logFmt Warn "Failed to obtain all replies to comment #{} in /r/{}: {}"
        (cid, subr, tshow err)
      return True -- let's be conservative here
    Right _ -> return hasReply
  where
  -- Like Pipes.Prelude.any but preserves the pipe's original return value.
  any' :: Monad m => (a -> Bool) -> Producer a m r -> m (Bool, r)
  any' pred pipe = P.fold' (||) False id $ pipe >-> P.map pred

  isOurReply Comment{..} = let Username commenter = author
                               Just (username, _) = optCredentials opts
                           in commenter == username

replyToDAEComment :: MonadIO m => Options -> Comment -> m (EitherR ())
replyToDAEComment opts Comment{..} = do
  let CommentID cid = commentID
      R subr = subreddit
  logFmt Info "Would have replied to comment #{} on /r/{}" (cid, subr)
  return $ Right () -- TODO


commentsSeen :: MVar Int
commentsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE commentsSeen #-}

daeCommentsSeen :: MVar Int
daeCommentsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE daeCommentsSeen #-}

commentsRepliedTo :: MVar Int
commentsRepliedTo = unsafePerformIO $ newMVar 0
{-# NOINLINE commentsRepliedTo #-}


-- Runtime statistics

-- | Pipe that counts all passing elements and stores the total in given MVar.
countAs :: (MonadIO m, Num n) => MVar n -> Pipe a a m r
countAs mvar = P.chain $ \_ -> liftIO $ modifyMVar_ mvar (return . (+1))

printStatistics :: IO ()
printStatistics = do
  postsSeen' <- readMVar postsSeen
  dontUpvotePostsSeen' <- readMVar dontUpvotePostsSeen
  postsUpvoted' <- readMVar postsUpvoted
  commentsSeen' <- readMVar commentsSeen
  daeCommentsSeen' <- readMVar daeCommentsSeen
  commentsRepliedTo' <- readMVar commentsRepliedTo
  mapM_ Text.putStrLn
    [ sep
    , "Total posts seen: " <> tshow postsSeen'
    , "'dont upvote' posts seen: " <> tshow dontUpvotePostsSeen'
    , "Posts upvoted: " <> tshow postsUpvoted'
    , sep
    , "Total comments seen: " <> tshow commentsSeen'
    , "DAE comments seen: " <> tshow daeCommentsSeen'
    , "Comments replied to: " <> tshow commentsRepliedTo'
    , sep
    ]
  where
  sep = Text.replicate 20 "-"
