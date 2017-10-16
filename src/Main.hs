{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<$>), liftA2)
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Char as Char
import Data.Default
import Data.Either.Combinators (whenLeft)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Format
import Data.Text.Format.Params
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import Reddit hiding (Options)
import Reddit.Types.Listing
import Reddit.Types.Post
import Safe (readMay)
import System.IO

import Paths_spiteful (version)


defaultUserAgent :: Text
defaultUserAgent = "spiteful-bot " <> botVersion

botVersion :: Text
botVersion = Text.pack $ showVersion version


main :: IO ()
main = do
  opts <- execParser commandLine

  logAt Info $ "spiteful-bot v" <> botVersion
  forM_ [stdout, stdin] $ \handle -> do
    hSetBuffering handle NoBuffering
    hSetEncoding handle utf8

  logAt Debug $ "Command line options: " <> (tshow opts)

  TLS.setGlobalManager =<< TLS.newTlsManager

  result <- runEffect $ fetchPosts opts
                      >-> P.filter isDontUpvotePost
                      >-> P.mapM_ (upvote opts)
  whenLeft result $ \err ->
    logAt Error $ "Error when fetching posts: " <> tshow err

upvote :: MonadIO m => Options -> Post -> m ()
upvote opts Post{..} = do
  let PostID pid = postID
      R subr = subreddit
  case optCredentials opts of
    Nothing -> do
      logFmt Warn
        "Cannot upvote post #{} (\"{}\" in /r/{}) due to lack of credentials"
        (pid, title, subr)
    _ -> do
      logFmt Info "Upvoting post #{} (\"{}\" in /r/{}) [{}] -> [{}]"
        (pid, title, subr, score, score + 1)
      result <- liftIO $ runRedditWith (toRedditOptions opts) $
        upvotePost postID
      whenLeft result $ \err ->
        logFmt Warn "Failed to upvote post #{}: {}" (pid, tshow err)


-- Program options & command line

data Options = Options
  { optCredentials :: Maybe (Text, Text)
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

commandLine :: ParserInfo Options
commandLine = info (options <**> helper)
    ( fullDesc
    <> progDesc "Run the Spiteful reddit bot"
    <> header "Spiteful reddit bot"
    <> failureCode 2
    )

options :: Parser Options
options = do
  username <- optional $ option str
    ( long "user" <> short 'u' <> metavar "USERNAME"
    <> help "Reddit username to use for the bot"
    )
  password <- optional $ option str
    ( long "password" <> short 'p' <> metavar "PASSWORD"
    <> help "Password to the bot's Reddit account"
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
  return def { optCredentials = liftA2 (,) username password
             , optSubreddit = R . stripSlashR <$> subreddit
             , optListing = listingType
             , optUserAgent = userAgent
             , optBatchSize = batchSize
             }
  where
  listing :: ReadM ListingType
  listing =
    maybeReader $ readMay . Text.unpack . capitalize . Text.strip . Text.pack

  listings = map (Text.toLower . tshow) [New, Hot, Rising, Controversial]

  stripSlashR s =
    foldl' (.) id (map stripPrefix prefixes) $ Text.toLower s
    where
    stripPrefix p s = fromMaybe s $ Text.stripPrefix p s
    prefixes = [ "/r/", "r/", "/" ]


-- Reddit stuff

type PostProducer m = Producer Post m (Either (APIError RedditError) ())

fetchPosts :: MonadIO m => Options -> PostProducer m
fetchPosts opts@Options{..} = do
  -- Reuse HTTP connection manager between Reddit calls.
  let redditOpts = toRedditOptions opts
  httpManager <- liftIO $ HTTP.newManager TLS.tlsManagerSettings
  let redditOpts' = redditOpts { connectionManager = Just httpManager}

  logAt Debug $ "Fetching " <> tshow listingType <> " posts from " <>
    maybe "the entire Reddit" (("subreddit: " <>) . tshow) optSubreddit
  doFetch httpManager redditOpts' Nothing
  where
  listingType = fromMaybe New optListing

  doFetch :: MonadIO m
          => HTTP.Manager -> RedditOptions -> Maybe PostID -> PostProducer m
  doFetch manager rOpts after = do
    result <- liftIO $ runRedditWith rOpts $ do
      let postOpts = def { pagination = After <$> after, limit = optBatchSize }
      Listing _ after' posts <- getPosts' postOpts listingType optSubreddit
      logFmt Debug "Received {} posts from the {} feed (after = {})"
                   (length posts, tshow listingType, tshow after)
      return (posts, after')
    case result of
      Left err -> return $ Left err
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
          doFetch manager rOpts after'
    where
    -- | How long to wait when we've exhausted the listing before
    -- retrying the fetch anew from the beginning.
    restartDelaySecs = case listingType of
      New -> 5
      Hot -> 60
      Rising -> 30
      Controversial -> 60
      Top -> 5 * 60  -- /top shouldn't really change a lot

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


-- Simple logging

data LogLevel = Trace | Debug | Info | Warn | Error
                deriving (Eq, Show)

logFmt :: (MonadIO m, Params ps) => LogLevel -> Format -> ps -> m ()
logFmt level fmt params = logAt level $ toStrict $ format fmt params

logAt :: MonadIO m => LogLevel -> Text -> m ()
logAt level msg = liftIO $ Text.hPutStrLn stderr $
  "[" <> Text.toUpper (tshow level) <> "] " <> msg


-- Utilities

tshow :: Show a => a -> Text
tshow = Text.pack . show

capitalize :: Text -> Text
capitalize "" = ""
capitalize s | Text.length s == 1 = Text.toUpper s
capitalize s = Char.toUpper (Text.head s) `Text.cons` Text.tail s

stripAccents :: Text -> Text
stripAccents = Text.filter (not . property Diacritic) . normalize NFD
