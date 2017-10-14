{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO

import Reddit
import Reddit.Types.Listing
import Reddit.Types.Post


main :: IO ()
main = do
  mapM_ setupIO [stdout, stdin]
  void $ runRedditAnon $ do
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
