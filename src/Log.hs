module Log
  ( initLogger
  , logInfo
  , logWarn
  , logError
  ) where

import Data.IORef
import Data.Semigroup

import System.IO.Unsafe
import System.Log.FastLogger

{-# NOINLINE logger #-}
logger :: IORef (TimedFastLogger, IO ())
logger = unsafePerformIO $ newIORef (const (return ()), return ())

-- | Call once (and at most once) to enable logging.
initLogger :: IO ()
initLogger = do
  let spec = FileLogSpec
        { log_file = "/tmp/haskell-download-manager-0.1.0.0.log"
        , log_file_size = 8 * 1024 * 1024
        , log_backup_number = 2
        }
  t <- newTimeCache simpleTimeFormat'
  tl <- newTimedFastLogger t (LogFile spec 4092)
  writeIORef logger tl

logInfo :: String -> IO ()
logInfo = log' . (toLogStr " [Info] " <>) . toLogStr

logWarn :: String -> IO ()
logWarn = log' . (toLogStr " [Warn] " <>) . toLogStr

logError :: String -> IO ()
logError = log' . (toLogStr " [Error] " <>) . toLogStr

log' :: ToLogStr s => s -> IO ()
log' s = do
  l <- fst <$> readIORef logger

  l (\t -> toLogStr t <> toLogStr s <> toLogStr ("\n" :: String))
