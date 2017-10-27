{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (throwTo)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either.Combinators (isRight, whenLeft)
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import Reddit.Actions.Thing (reply)
import Reddit.Types.Comment
import Reddit.Types.Post
import Reddit.Types.Subreddit (SubredditName(..))
import Reddit.Types.User (Username(..))
import Safe (headMay)
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import System.Signal (installHandler, sigINT)
import Text.Regex

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

  let features = fromMaybe defaultFeatures optFeatures

  tid <- myThreadId
  installHandler sigINT $ \_ -> do
    -- TODO: some way of printing statistics w/o closing the program
    printStatistics features
    throwTo tid ExitSuccess

  logAt Info $ "spiteful-bot v" <> botVersion
  logAt Debug $ "Command line options: " <> tshow opts

  TLS.setGlobalManager =<< TLS.newTlsManager

  logAt Info $ "Features: " <> csv features
  let workerFuncs = resolveFeatures features
  mapConcurrently_ ($ opts) workerFuncs


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

resolveFeatures :: Features -> [Options -> IO ()]
resolveFeatures = map func . HS.toList
  where
  func FeatureDontUpvote = monitorDontUpvotePosts
  func FeatureDAE = monitorDAEComments


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
  title' = deburr title
  phrases = map deburr [ "don't upvote"
                       , "dont upvote"
                       , "no upvote"
                       , "not upvote"
                       ]

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
    >-> P.mapFoldable findDAEPhrase >-> countAs daeCommentsSeen
    >-> P.filterM ((not <$>) . hasCommentBeenRepliedTo opts . fst)
    >-> P.mapM (uncurry $ replyToDAEComment opts)
                >-> P.filter isRight
                >-> countAs commentsRepliedTo
    >-> P.drain
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching comments: " <> tshow err

findDAEPhrase :: Comment -> Maybe (Comment, Text)
findDAEPhrase comment = (comment,) <$> headMay
    [ Text.pack $ phrase
    | Just (_, phrase, _, _) <- map (flip matchRegexAll commentString) phrases
    ]
  where
  commentString = Text.unpack $ deburr $ body comment
  phrases = map (mkRegex' . (sentenceSepRe ++)) $
      [ "does ae" , "is ae" , "has ae"
      , "dae " ++ anyoneElse -- yes, some dimwits write it like this
      , "does " ++ anyoneElse
      , "is " ++ anyoneElse
      , "has " ++ anyoneElse
      ] <> ["dae"]
  sentenceSepRe = "^\\s*|.\\s+"  -- start of text/line or full stop
  anyoneElse = "any\\s?one else[?]*"

  mkRegex' r = mkRegexWithOpts r singleLine caseSensitive
    where
    singleLine = True  -- ^ and $ match beginning/end of the line
    caseSensitive = False

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

replyToDAEComment :: MonadIO m => Options -> Comment -> Text -> m (EitherR ())
replyToDAEComment opts Comment{..} daePhrase = do
  let CommentID cid = commentID
      R subr = subreddit
  replyText <- sample phrases
  let fullReplyText = "> " <> daePhrase <> "\n\n" <> replyText
  logFmt Info "Replying to comment #{} on /r/{} with \"{}\""
              (cid, subr, fullReplyText)
  result <- runReddit opts $ reply commentID fullReplyText
  case result of
    Left err -> return $ Left err
    Right (CommentID replyID) -> do
      logFmt Debug "Successfuly replied with #{} to comment #{} on /r/{}"
                   (replyID, cid, subr)
      return $ Right ()
  where
  phrases = ["No.", "Nope.", "Not really."]

  sample :: MonadIO m => [a] -> m a
  sample xs = do
    idx <- liftIO $ randomRIO (0, length xs - 1)
    return $ xs !! idx


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

printStatistics :: Features -> IO ()
printStatistics features = do
  lines <- (concat <$>) . mapM (((sep:) <$>) . stats) $ HS.toList features
  mapM_ Text.putStrLn $ lines <> [sep]
  where
  stats FeatureDontUpvote = do
    postsSeen' <- readMVar postsSeen
    dontUpvotePostsSeen' <- readMVar dontUpvotePostsSeen
    postsUpvoted' <- readMVar postsUpvoted
    return [ "Total posts seen: " <> tshow postsSeen'
           , "'dont upvote' posts seen: " <> tshow dontUpvotePostsSeen'
           , "Posts upvoted: " <> tshow postsUpvoted'
           ]
  stats FeatureDAE = do
    commentsSeen' <- readMVar commentsSeen
    daeCommentsSeen' <- readMVar daeCommentsSeen
    commentsRepliedTo' <- readMVar commentsRepliedTo
    return [ "Total comments seen: " <> tshow commentsSeen'
           , "DAE comments seen: " <> tshow daeCommentsSeen'
           , "Comments replied to: " <> tshow commentsRepliedTo'
           ]

  sep = Text.replicate 20 "-"
