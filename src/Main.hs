{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

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
import qualified Data.Text.IO as Text
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

  -- TODO: read options from file
  result <- runEffect $
    fetchNewPosts def >-> P.map formatPost >-> P.print
  whenLeft result $ \err -> Text.hPutStrLn stderr (tshow err)

formatPost :: Post -> Text
formatPost post = "[" <> tshow (score post) <> "] " <> title post
                  <> " (" <> subreddit' post <> ")"
  where
  subreddit' post = let R name = subreddit post in "/r/" <> name


type PostProducer m = Producer Post m (Either (APIError RedditError) ())

fetchNewPosts :: MonadIO m => Options -> PostProducer m
fetchNewPosts options = do
  -- Reuse HTTP connection manager between Reddit calls.
  let redditOpts = toRedditOptions options
  httpManager <- liftIO $ HTTP.newManager TLS.tlsManagerSettings
  let redditOpts' = redditOpts { connectionManager = Just httpManager}

  doFetch httpManager redditOpts' New
  where
  doFetch :: MonadIO m
          => Manager -> RedditOptions -> ListingType -> PostProducer m
  doFetch manager rOpts lt = do
    result <- liftIO $ runRedditWith rOpts $ do
      -- TODO: fetch more pages of posts
      Listing _ _ posts <- getPosts' def lt (optSubreddit options)
      return posts
    case result of
      Left err -> return $ Left err
      Right posts -> do
        mapM_ yield posts
        doFetch manager rOpts lt


tshow :: Show a => a -> Text
tshow = Text.pack . show
