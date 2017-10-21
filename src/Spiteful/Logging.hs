{-# LANGUAGE OverloadedStrings #-}

module Spiteful.Logging
 ( defaultLogLevel
 , getLogLevel
 , setLogLevel
 , logAt
 , logFmt
 , LogLevel(..)
 ) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Format (Format, format)
import Data.Text.Format.Params (Params)
import Data.Text.Lazy (toStrict)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)

import Spiteful.Util (tshow)


data LogLevel = Trace | Debug | Info | Warn | Error
                deriving (Bounded, Enum, Eq, Ord, Show)


logFmt :: (MonadIO m, Params ps) => LogLevel -> Format -> ps -> m ()
logFmt level fmt ps = logAt level $ toStrict $ format fmt ps

logAt :: MonadIO m => LogLevel -> Text -> m ()
logAt level msg = do
  currLevel <- getLogLevel
  when (level >= currLevel) $ liftIO $ do
    tid <- Text.filter isDigit . tshow <$> myThreadId
    withMVar stderrLock $ \() ->
      Text.hPutStrLn stderr $ mconcat
        [ "[", Text.toUpper (tshow level), "#", tid, "] ", msg ]

-- | Lock on stderr to ensure only one log statement is printed at a time.
stderrLock :: MVar ()
stderrLock = unsafePerformIO $ newMVar ()
{-# NOINLINE stderrLock #-}


defaultLogLevel :: LogLevel
defaultLogLevel = Info

currentLogLevel :: MVar LogLevel
currentLogLevel = unsafePerformIO $ newMVar defaultLogLevel
{-# NOINLINE currentLogLevel #-}

getLogLevel :: MonadIO m => m LogLevel
getLogLevel = liftIO $ readMVar currentLogLevel

setLogLevel :: MonadIO m => LogLevel -> m ()
setLogLevel l = liftIO $ void $ swapMVar currentLogLevel =<< evaluate l
