{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (throwTo)
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Either.Combinators (isRight, whenLeft)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import Reddit hiding (Options)
import Reddit.Types.Listing
import Reddit.Types.Post
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Signal (installHandler, sigINT)

import Spiteful.Options
import Spiteful.Logging
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

  result <- runEffect $
    fetchPosts opts >-> countAs postsSeen
    >-> P.filter isDontUpvotePost >-> countAs dontUpvotePostsSeen
    >-> P.filter hasNotBeenVotedOn
    >-> P.mapM (upvote opts) >-> P.filter isRight >-> countAs postsUpvoted
    >-> P.drain -- drop upvote outcomes but retain a possible fetch error
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching posts: " <> tshow err


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

hasNotBeenVotedOn :: Post -> Bool
hasNotBeenVotedOn = isNothing . liked


-- Runtime statistics

-- | Pipe that counts all passing elements and stores the total in given MVar.
countAs :: (MonadIO m, Num n) => MVar n -> Pipe a a m r
countAs mvar = P.chain $ \_ -> liftIO $ modifyMVar_ mvar (return . (+1))

postsSeen :: MVar Int
postsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE postsSeen #-}

dontUpvotePostsSeen :: MVar Int
dontUpvotePostsSeen = unsafePerformIO $ newMVar 0
{-# NOINLINE dontUpvotePostsSeen #-}

postsUpvoted :: MVar Int
postsUpvoted = unsafePerformIO $ newMVar 0
{-# NOINLINE postsUpvoted #-}

printStatistics :: IO ()
printStatistics = do
  seen <- readMVar postsSeen
  dontUpvoteSeen <- readMVar dontUpvotePostsSeen
  upvoted <- readMVar postsUpvoted
  mapM_ Text.putStrLn [ sep
                      , "Total posts seen: " <> tshow seen
                      , "'dont upvote' posts seen: " <> tshow dontUpvoteSeen
                      , "Posts upvoted: " <> tshow upvoted
                      , sep
                      ]
  where
  sep = Text.replicate 20 "-"


-- Reddit stuff
-- TODO: extract this to a separate module

type RedditResult = Either (APIError RedditError) ()
type PostProducer m = Producer Post m RedditResult

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

upvote :: MonadIO m => Options -> Post -> m RedditResult
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

newRedditOptions :: MonadIO m => Options -> m RedditOptions
newRedditOptions opts = do
  -- Reuse HTTP connection manager between Reddit calls.
  httpManager <- liftIO TLS.getGlobalManager
  let redditOpts = toRedditOptions opts
  return redditOpts { connectionManager = Just httpManager}
