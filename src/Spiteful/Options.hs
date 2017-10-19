{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spiteful.Options
  ( botVersion
  , commandLine
  , Options(..)
  , toRedditOptions
  ) where

import Control.Applicative ((<$>), liftA2)
import Data.Default
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
  , optCredentials :: Maybe (Text, Text)
  , optSubreddit :: Maybe SubredditName
  , optListing :: Maybe ListingType
  , optUserAgent :: Maybe Text
  , optBatchSize :: Maybe Int
  } deriving (Generic, Show)

instance Default Options

toRedditOptions :: Options -> RedditOptions
toRedditOptions Options{..} = RedditOptions
  { rateLimitingEnabled = True
  , connectionManager = Nothing
  , loginMethod = maybe Anonymous (uncurry Credentials) optCredentials
  , customUserAgent = Just $ Text.encodeUtf8 $ fromMaybe defaultUserAgent optUserAgent
  }


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
  listingType <- optional $ option listing
      ( long "watch" <> short 'w' <> metavar "WHAT"
      <> help (Text.unpack $ "Which Reddit listing to watch: "
                              <> Text.intercalate ", " listings)
      )
  userAgent <- optional $ option str
      ( long "user-agent" <> short 'A'
      <> metavar "USER-AGENT"
      <> help "Custom value for the User-Agent header sent with all requests"
      <> hidden
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
             , optCredentials = liftA2 (,) username password
             , optSubreddit = R . stripSlashR <$> subreddit
             , optListing = listingType
             , optUserAgent = userAgent
             , optBatchSize = batchSize
             }
  where
  listing :: ReadM ListingType
  listing =
      maybeReader $ readMay . Text.unpack . capitalize . Text.strip . Text.pack

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

  listings = map (Text.toLower . tshow) [New, Hot, Rising, Controversial]

  stripSlashR s =
    foldl' (.) id (map stripPrefix prefixes) $ Text.toLower s
    where
    stripPrefix p s = fromMaybe s $ Text.stripPrefix p s
    prefixes = [ "/r/", "r/", "/" ]
