module Spiteful.Util where

import Control.Exception (bracket_)
import Control.Monad
import Data.Char as Char
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Format (Format, format)
import Data.Text.Format.Params (Params)
import Data.Text.ICU.Char
import Data.Text.ICU.Normalize
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as Text (toStrict)
import System.IO
import Text.LineBreak


-- | Apply a function to a value in double-nested functors (e.g. IO (Maybe a)).
(<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<$$>) = fmap . fmap
infixr 8 <$$>

tshow :: Show a => a -> Text
tshow = Text.pack . show

fmt :: Params ps => Format -> ps -> Text
fmt f ps = Text.toStrict $ format f ps

capitalize :: Text -> Text
capitalize "" = ""
capitalize s = Char.toUpper (Text.head s) `Text.cons` Text.tail s

csv :: (Show a, Foldable t) => t a -> Text
csv = Text.intercalate ", " . map tshow . toList

-- | Remove as many distinguishing features from Text as possible.
deburr :: Text -> Text
deburr = Text.toCaseFold . stripAccents . collapseWhitespace

stripAccents :: Text -> Text
stripAccents = Text.filter (not . property Diacritic) . normalize NFD

collapseWhitespace :: Text -> Text
collapseWhitespace = Text.unwords . Text.words

-- | Break text into lines of at most given length.
breakLines :: Int -> Text -> [Text]
breakLines maxCol = map Text.pack . breakStringLn bf . Text.unpack
  where
  bf = BreakFormat maxCol tabWidth '\0' Nothing
  tabWidth = 4


data Echo = EchoOn | EchoOff deriving (Bounded, Enum, Eq)

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
