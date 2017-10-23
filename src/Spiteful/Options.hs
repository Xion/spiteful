{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spiteful.Options
  ( botVersion
  , commandLine
  , Options(..)
  , toRedditOptions
  , Feature(..)
  , Features
  , defaultFeatures
  ) where

import Control.Applicative ((<$>), liftA2)
import Data.Default
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Options.Applicative
import Reddit hiding (Options)
import Reddit.Types.Listing
import Reddit.Types.Reddit (mainBaseURL)
import Safe (readMay)

import Paths_spiteful (version)
import Spiteful.Util


defaultUserAgent :: Text
defaultUserAgent = "spiteful-bot " <> botVersion

botVersion :: Text
botVersion = Text.pack $ showVersion version


-- | Options as parsed from the command line.
-- Fields that have Nothing should be defaulted on upon use.
data Options = Options
  { optVerbosity :: Int  -- ^ How many times the -v flag was passed
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


-- | Bot feature that can be turned on or off.
data Feature = FeatureDontUpvote | FeatureDAE
               deriving (Bounded, Enum, Eq, Generic, Ord, Read)

instance Hashable Feature

instance Show Feature where
  show FeatureDontUpvote = "dont-upvote"
  show FeatureDAE = "dae"

type Features = HashSet Feature

-- | Default for when no --feature flag has been passed.
defaultFeatures :: Features
defaultFeatures = HS.fromList [FeatureDontUpvote]


-- Parsing command line options

commandLine :: ParserInfo Options
commandLine = info (options <**> helper)
  ( fullDesc
  <> progDesc "Run the Spiteful reddit bot"
  <> header "Spiteful reddit bot"
  <> failureCode 2
  )

options :: Parser Options
options = do
  verbosity' <- verbosity
  baseUrl <- optional $ option str
      ( long "baseUrl" <> metavar "URL"
      <> internal  -- won't show up even when --help is passed
      <> help (Text.unpack $ "Alternate base URL for Reddit API "
                             <> "(default is" <> mainBaseURL <> ")")
      )

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
  userAgent <- optional $ option str
      ( long "user-agent" <> short 'A'
      <> metavar "USER-AGENT"
      <> help "Custom value for the User-Agent header sent with all requests"
      <> hidden
      )

  features' <- optional $ option features
      ( long "features" <> short 'f' <> metavar "FEATURE[, FEATURE, [...]]"
      <> help (Text.unpack $ fmt (
          "Comma-separated list of features to enable. "
          <> "Choices include: [{}]. Default: [{}]")
          (csv $ ([minBound..maxBound] :: [Feature]), csv defaultFeatures)))
  listingType <- optional $ option listing
      ( long "watch" <> short 'w' <> metavar "WHAT"
      <> help (Text.unpack $ "Which listing of Reddit posts to watch: "
                              <> intercalateWithLast " or " ", " listings)
      )
  batchSize <- optional $ option auto
      ( long "batch" <> short 'b'
      <> metavar "SIZE"
      <> help "How many Reddit posts to fetch in a single request"
      <> hidden
      )

  subreddit <- optional $ argument str
      ( metavar "SUBREDDIT"
      <> help "Subreddit to limit the bot to. By default, it watches the entire Reddit"
      )
  return def { optVerbosity = verbosity'
             , optBaseURL = baseUrl
             , optFeatures = features'
             , optCredentials = liftA2 (,) username password
             , optSubreddit = R . stripSlashR <$> subreddit
             , optListing = listingType
             , optUserAgent = userAgent
             , optBatchSize = batchSize
             }
  where
  -- TODO: make passing too many --v/--q a parse error
  verbosity :: Parser Int
  verbosity = fromMaybe 0 <$> optional (verbose <|> quiet)
    where
    verbose = length <$> many (flag' ()
        (long "verbose" <> short 'v'
        <> help "Increase the verbosity of logging output"))
    quiet = length <$> many (flag' ()
        (long "quiet" <> short 'q'
        <> help "Decrease the verbosity of logging output"))

  features :: ReadM Features
  features = maybeReader $ (HS.fromList <$>)
    . sequence . map (readMay . Text.unpack . Text.strip)
    . Text.splitOn "," . Text.toLower . Text.pack

  listing :: ReadM ListingType
  listing =
      maybeReader $ readMay . Text.unpack . capitalize . Text.strip . Text.pack

  listings = map (Text.toLower . tshow) [New, Hot, Rising, Controversial]

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
