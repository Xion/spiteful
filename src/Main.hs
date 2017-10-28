module Main where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async
import Control.Exception (throwTo)
import Control.Monad
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import System.Exit
import System.IO
import System.Signal (installHandler, sigINT)

import Spiteful.Features (Feature(..), defaultFeatures)
import Spiteful.Features.DAE
import Spiteful.Features.DontUpvote
import Spiteful.Features.UpvoteIf
import Spiteful.Logging
import Spiteful.Metrics
import Spiteful.Options
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
  func FeatureUpvoteIf = monitorUpvoteIfPosts
  func FeatureDAE = monitorDAEComments
