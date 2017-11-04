module Main where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async
import Control.Exception (throwTo)
import Control.Monad
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import qualified System.Console.Terminal.Size as Term
import System.Exit
import System.IO
import System.Signal (installHandler, sigINT)

import Spiteful.Features hiding (describe)
import qualified Spiteful.Features as F
import Spiteful.Features.DAE
import Spiteful.Features.DontUpvote
import Spiteful.Features.IfThisGetsUpvotes
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

  command <- execParser commandLine
  case command of
    PrintVersion -> printVersion
    ShowFeatures -> showFeatures
    RunBot opts -> run opts


printVersion :: IO ()
printVersion = Text.putStrLn $ "spiteful-bot " <> botVersion


showFeatures :: IO ()
showFeatures =
  printDefinitionList $ zip (map tshow features) (map F.describe features)
  where
  features = [minBound..maxBound] :: [Feature]

  printDefinitionList :: [(Text, Text)] -> IO ()
  printDefinitionList list = do
    let indent = 4
    maxWidth <- fromMaybe 80 <$> Term.width <$$> Term.size
    forM_ list $ \(term, definition) -> do
      Text.putStrLn $ Text.take maxWidth term
      Text.putStrLn ""
      mapM_ Text.putStrLn $
        map (Text.replicate indent " " <>) $
          -- TODO: break on words
          Text.chunksOf (maxWidth - indent) definition
      Text.putStrLn ""


run :: Options -> IO ()
run opts = do
  opts'@Options{..} <- fixCredentials opts
  setupLogging optVerbosity

  let features = fromMaybe defaultFeatures optFeatures

  tid <- myThreadId
  installHandler sigINT $ \_ -> do
    -- TODO: some way of printing statistics w/o closing the program
    printStatistics features
    throwTo tid ExitSuccess

  logAt Info $ "spiteful-bot v" <> botVersion
  logAt Debug $ "Command line options: " <> tshow opts'

  TLS.setGlobalManager =<< TLS.newTlsManager

  logAt Info $ "Features: " <> csv features
  let workerFuncs = resolveFeatures features
  mapConcurrently_ ($ opts) workerFuncs

fixCredentials :: Options -> IO Options
fixCredentials opts =
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


setupLogging :: Int -> IO ()
setupLogging verbosity = do
  let level = defaultLevel - verbosity
      minLevel = fromEnum (minBound :: LogLevel)
      maxLevel = fromEnum (maxBound :: LogLevel)
      defaultLevel = fromEnum defaultLogLevel

  when (not $ level >= minLevel && level <= maxLevel) $ do
    let flag :: Text
        maxFlagCount :: Int
        (flag, maxFlagCount) = if verbosity > 0
                               then ("-v", defaultLevel - minLevel)
                               else ("-q", maxLevel - defaultLevel)
    logFmt Error "The {} flag has been passed too many times ({} > {})"
                 (flag, verbosity, maxFlagCount)
    exitWith $ ExitFailure 2

  setLogLevel $ toEnum level


resolveFeatures :: Features -> [Options -> IO ()]
resolveFeatures = map func . HS.toList
  where
  func FeatureDontUpvote = monitorDontUpvotePosts
  func FeatureUpvoteIf = monitorUpvoteIfPosts
  func FeatureIfThisGetsUpvotes = monitorIfThisGetsUpvotesPosts
  func FeatureDAE = monitorDAEComments
