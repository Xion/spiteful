{-# LANGUAGE OverloadedStrings #-}

module Spiteful.Util where

import Control.Exception (bracket_)
import Control.Monad
import Data.Char as Char
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import System.IO


tshow :: Show a => a -> Text
tshow = Text.pack . show

capitalize :: Text -> Text
capitalize "" = ""
capitalize s | Text.length s == 1 = Text.toUpper s
capitalize s = Char.toUpper (Text.head s) `Text.cons` Text.tail s

stripAccents :: Text -> Text
stripAccents = Text.filter (not . property Diacritic) . normalize NFD


data Echo = EchoOn | EchoOff deriving (Eq)

-- | Ask for a line of text, optionally with terminal echo disabled.
-- Based on https://stackoverflow.com/a/4064482.
prompt :: Echo -> Text -> IO Text
prompt echo p = do
  Text.putStr $ Text.stripEnd p <> ": "
  hFlush stdout
  inp <- withEcho (echo == EchoOn) Text.getLine
  when (echo == EchoOff) $ putChar '\n'
  return inp
  where
  withEcho :: Bool -> IO a -> IO a
  withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
