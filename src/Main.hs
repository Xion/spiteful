{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar
import Control.Exception (throwTo)
import Control.Monad
import Control.Monad.IO.Class
import Data.Either.Combinators (isRight, whenLeft)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import Reddit.Types.Post
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
  monitorDontUpvotePosts opts


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
    >-> P.filter hasNotBeenVotedOn
    >-> P.mapM (upvote opts) >-> P.filter isRight >-> countAs postsUpvoted
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
