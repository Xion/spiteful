{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Either.Combinators (whenLeft)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Version (showVersion)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Format
import Data.Text.Format.Params
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import GHC.Generics (Generic)
import System.IO

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Pipes
import qualified Pipes.Prelude as P
import Reddit hiding (Options)
import Reddit.Types.Listing
import Reddit.Types.Post

import Paths_spiteful (version)


defaultUserAgent :: Text
defaultUserAgent = "spiteful-bot " <> Text.pack (showVersion version)


data Options = Options
  { optCredentials :: Maybe (Text, Text)
  , optSubreddit :: Maybe SubredditName
  , optUserAgent :: Maybe Text
  } deriving (Generic, Show)

instance Default Options

toRedditOptions :: Options -> RedditOptions
toRedditOptions Options{..} = RedditOptions
  { rateLimitingEnabled = True
  , connectionManager = Nothing
  , loginMethod = maybe Anonymous (uncurry Credentials) optCredentials
  , customUserAgent = Just $ Text.encodeUtf8 $ fromMaybe defaultUserAgent optUserAgent
  }


main :: IO ()
main = do
  forM_ [stdout, stdin] $ \handle -> do
    hSetBuffering handle NoBuffering
    hSetEncoding handle utf8

  TLS.setGlobalManager =<< TLS.newTlsManager

  -- TODO: read options from the command line
  result <- runEffect $ fetchPosts def New
                      >-> P.filter isDontUpvotePost
                      >-> P.map formatPost
                      >-> P.print
  whenLeft result $ \err -> Text.hPutStrLn stderr (tshow err)

formatPost :: Post -> Text
formatPost post = "[" <> tshow (score post) <> "] " <> title post
                  <> " (" <> subreddit' post <> ")"
  where
  subreddit' post = let R name = subreddit post in "/r/" <> name


-- Fetching posts

type PostProducer m = Producer Post m (Either (APIError RedditError) ())

fetchPosts :: MonadIO m => Options -> ListingType -> PostProducer m
fetchPosts options listingType = do
  -- Reuse HTTP connection manager between Reddit calls.
  let redditOpts = toRedditOptions options
  httpManager <- liftIO $ HTTP.newManager TLS.tlsManagerSettings
  let redditOpts' = redditOpts { connectionManager = Just httpManager}

  doFetch httpManager redditOpts' listingType Nothing
  where
  doFetch :: MonadIO m
          => HTTP.Manager -> RedditOptions -> ListingType -> Maybe PostID
          -> PostProducer m
  doFetch manager rOpts lt after = do
    result <- liftIO $ runRedditWith rOpts $ do
      let postOpts = def { pagination = After <$> after, limit = Just 50 }
      Listing _ after' posts <- getPosts' postOpts lt (optSubreddit options)
      logAt Debug "Received {} posts from the {} feed (after = {})"
                  (length posts, tshow lt, tshow after)
      return (posts, after')
    case result of
      Left err -> return $ Left err
      Right (posts, after') -> do
        mapM_ yield posts
        if not . null $ posts then do
          liftIO $ threadDelay 1000000 -- 1 sec, as per API docs recommendation
          doFetch manager rOpts lt after'
        else return $ Right ()

isDontUpvotePost :: Post -> Bool
isDontUpvotePost post = any (`Text.isInfixOf` title') phrases
  where
  title' = toPlain $ title post
  phrases = map toPlain [ "don't upvote", "no upvote", "not upvote" ]
  toPlain = Text.toCaseFold . stripAccents -- TODO: collapse whitespace


-- Simple logging

data LogLevel = Debug | Info | Warn | Error
                deriving (Eq, Show)

logAt :: (MonadIO m, Params ps) => LogLevel -> Format -> ps -> m ()
logAt level fmt params =
  let message = toStrict $ format fmt params
  in liftIO $ Text.hPutStrLn stderr $
    "[" <> Text.toUpper (tshow level) <> "] " <> message


-- Utilities

tshow :: Show a => a -> Text
tshow = Text.pack . show

stripAccents :: Text -> Text
stripAccents = Text.filter (not . property Diacritic) . normalize NFD
