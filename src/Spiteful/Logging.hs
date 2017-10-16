{-# LANGUAGE OverloadedStrings #-}

module Spiteful.Logging
 ( logAt
 , logFmt
 , LogLevel(..)
 ) where

import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Format (Format, format)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)
import System.IO (stderr)

import Spiteful.Util (tshow)


data LogLevel = Trace | Debug | Info | Warn | Error
                deriving (Eq, Show)


logFmt :: (MonadIO m, Params ps) => LogLevel -> Format -> ps -> m ()
logFmt level fmt ps = logAt level $ toStrict $ format fmt ps

logAt :: MonadIO m => LogLevel -> Text -> m ()
logAt level msg = liftIO $ Text.hPutStrLn stderr $
    "[" <> Text.toUpper (tshow level) <> "] " <> msg
