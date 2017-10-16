{-# LANGUAGE OverloadedStrings #-}

module Spiteful.Util where

import Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize


tshow :: Show a => a -> Text
tshow = Text.pack . show

capitalize :: Text -> Text
capitalize "" = ""
capitalize s | Text.length s == 1 = Text.toUpper s
capitalize s = Char.toUpper (Text.head s) `Text.cons` Text.tail s

stripAccents :: Text -> Text
stripAccents = Text.filter (not . property Diacritic) . normalize NFD
