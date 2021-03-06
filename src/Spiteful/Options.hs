{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Spiteful.Options
  ( botVersion
  , Command(..)
  , commandLine
  , Options(..)
  , toRedditOptions
  ) where

import Control.Applicative ((<$>), liftA2)
import Data.Default
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Version (showVersion)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Options.Applicative
import Reddit hiding (Options)
import Reddit.Types.Listing
import Reddit.Types.Reddit (mainBaseURL)
import Safe (readMay)

import Paths_spiteful (version)
import Spiteful.Features
import Spiteful.Util


defaultUserAgent :: Text
defaultUserAgent = "spiteful-bot " <> botVersion

botVersion :: Text
botVersion = Text.pack $ showVersion version


-- | Options as parsed from the command line.
-- Fields that have Nothing should be defaulted on upon use.
data Options = Options
  { optVerbosity :: Int  -- ^ How many times the -v flag was passed
  , optDebugPort :: Maybe Word16
  , optBaseURL :: Maybe Text  -- ^ Alt. base URL for Reddit API (for testing)
  , optFeatures :: Maybe Features  -- ^ What features should be enabled
  , optCredentials :: Maybe (Text, Text)
  , optSubreddit :: Maybe SubredditName
  , optListing :: Maybe ListingType
  , optUserAgent :: Maybe Text
  , optBatchSize :: Maybe Int
  } deriving (Generic, Show)
-- TODO: this is actually raw options parsed from command line, hence Maybes;
-- introduce another variant of this record type where the default are filled in

instance Default Options

toRedditOptions :: Options -> RedditOptions
toRedditOptions Options{..} = RedditOptions
  { rateLimitingEnabled = True
  , connectionManager = Nothing
  , loginMethod = maybe Anonymous (uncurry Credentials) optCredentials
  , customUserAgent = Just $ Text.encodeUtf8 $ fromMaybe defaultUserAgent optUserAgent
  }


-- Parsing command line options

data Command = RunBot Options | ShowFeatures | PrintVersion
               deriving (Show)

instance Default Command where
  def = RunBot def

commandLine :: ParserInfo Command
commandLine = info (command <**> helper)
  ( fullDesc
  <> progDesc "Run the Spiteful reddit bot"
  <> header "Spiteful reddit bot"
  <> failureCode 2
  )
  where
  command = RunBot <$> runBot
            <|> ShowFeatures <$ showFeatures
            <|> PrintVersion <$ printVersion

printVersion :: Parser ()
printVersion = flag' ()
    ( long "version" <> short 'V'
    <> help "Print the program version and quit"
    <> hidden
    )

showFeatures :: Parser ()
showFeatures = flag' ()
    ( long "show-features"
    <> help "Show a short description of supported bot features"
    <> hidden
    )

runBot :: Parser Options
runBot = do
  optVerbosity <- verbosity
  optDebugPort <- debugPort
  optBaseURL <- optional $ option str
      ( long "baseUrl" <> metavar "URL"
      <> internal  -- won't show up even when --help is passed
      <> help (Text.unpack $ "Alternate base URL for Reddit API "
                             <> "(default is" <> mainBaseURL <> ")")
      )

  optCredentials <- credentials
  optUserAgent <- optional $ option str
      ( long "user-agent" <> short 'A'
      <> metavar "USER-AGENT"
      <> help "Custom value for the User-Agent header sent with all requests"
      <> hidden
      )

  optFeatures <- features
  optListing <- listing
  optBatchSize <- optional $ option auto
      ( long "batch" <> short 'b'
      <> metavar "SIZE"
      <> help "How many Reddit posts or comments to fetch in a single request"
      <> hidden
      )

  optSubreddit <- subreddit
  return Options{..}

verbosity :: Parser Int
verbosity = fromMaybe 0 <$> optional (verbose <|> quiet)
  where
  verbose = length <$> many (flag' ()
      (long "verbose" <> short 'v'
      <> help "Increase the verbosity of logging output"))
  quiet = length <$> many (flag' ()
      (long "quiet" <> short 'q'
      <> help "Decrease the verbosity of logging output"))

debugPort :: Parser (Maybe Word16)
debugPort = optional $ option auto
    ( long "debug-port" <> metavar "PORT"
    <> help "Port number for the optional debug HTTP server"
    <> hidden
    )

credentials :: Parser (Maybe (Text, Text))
credentials = do
  username <- optional $ option str
      ( long "user" <> short 'u' <> metavar "USERNAME"
      <> help ("Reddit username to use for the bot. "
              <> "Pass `-` to read it from stdin.")
      )
  password <- optional $ option str
      ( long "password" <> short 'p' <> metavar "PASSWORD"
      <> help ("Password to the bot's Reddit account. "
              <> "Pass `-` to read it from stdin.")
      )
  return $ liftA2 (,) username password

subreddit :: Parser (Maybe SubredditName)
subreddit = do
  subr <- optional $ argument str
      ( metavar "SUBREDDIT"
      <> help ("Subreddit to limit the bot to. "
                  <> "By default, it watches the entire Reddit")
      )
  return $ R . stripSlashR <$> subr

features :: Parser (Maybe Features)
features = optional $ option features'
    ( long "features" <> short 'f' <> metavar "FEATURE[, FEATURE, [...]]"
    <> help (Text.unpack $ fmt (
        "Either \"all\", or a comma-separated list of features to enable. "
        <> "Choices include: [{}]. Default: [{}]")
        (csv ([minBound..maxBound] :: [Feature]), csv defaultFeatures))
    )
  where
  features' :: ReadM Features
  features' = maybeReader $ \val ->
    let v = Text.pack val
    in HS.fromList <$> if Text.strip v == "all" then Just allFeatures
                       else sequence . map (readMay . Text.unpack . Text.strip)
                            . Text.splitOn "," . Text.toLower $ v
  allFeatures = [minBound..maxBound] :: [Feature]

listing :: Parser (Maybe ListingType)
listing = optional $ option listing'
    ( long "watch" <> short 'w' <> metavar "WHAT"
    <> help (Text.unpack $ "Which listing of Reddit posts to watch: "
                            <> intercalateWithLast " or " ", " listings)
    )
  where
  listing' :: ReadM ListingType
  listing' =
      maybeReader $ readMay . Text.unpack . capitalize . Text.strip . Text.pack

  listings = map (Text.toLower . tshow) ([minBound..maxBound] :: [ListingType])

deriving instance Generic ListingType
deriving instance Bounded ListingType
deriving instance Enum ListingType


-- Utility functions

stripSlashR :: Text -> Text
stripSlashR s =
  foldl' (.) id (map stripPrefix prefixes) $ Text.toLower s
  where
  stripPrefix p s = fromMaybe s $ Text.stripPrefix p s
  prefixes = [ "/r/", "r/", "/" ]

intercalateWithLast :: Text -> Text -> [Text] -> Text
intercalateWithLast lastSep sep xs = case length xs of
  0 -> ""
  1 -> head xs
  2 -> let x:y:[] = xs in x <> lastSep <> y
  _ -> let y = last xs
           x = last (init xs)
           rest = init . init $ xs
       in Text.intercalate sep rest <> sep <> x <> lastSep <> y
