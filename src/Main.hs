{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import System.IO

import Reddit hiding (Options)
import Reddit.Types.Listing
import Reddit.Types.Post


-- TODO: include actual version number here
defaultUserAgent :: Text
defaultUserAgent = "spiteful-bot 0.1"


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
  mapM_ setupIO [stdout, stdin]
  -- TODO: read options from file
  let redditOptions = toRedditOptions def
  void $ runRedditWith redditOptions $ do
    Listing _ _ posts <- getPosts' def New Nothing
    forM_ posts $ \post -> do
      liftIO $ Text.putStrLn $ formatPost post
  where
  setupIO :: Handle -> IO ()
  setupIO handle = do
    hSetBuffering handle NoBuffering
    hSetEncoding handle utf8

formatPost :: Post -> Text
formatPost post = "[" <> tshow (score post) <> "] " <> title post
                  <> " (" <> subreddit' post <> ")"
  where
  subreddit' post = let R name = subreddit post in "/r/" <> name

tshow :: Show a => a -> Text
tshow = Text.pack . show
