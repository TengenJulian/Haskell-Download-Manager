{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Lib
    (
      module Lib
    , module Lib.Thread
    , module Lib.Thread.DownloadThread
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.ByteString.Char8 (readInt)
import           Network.HTTP.Simple
import           Data.Maybe
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import qualified Control.Concurrent as CC
import Control.Monad (forM_)
import qualified Data.ByteString as S (hPut, length)
import           Data.Conduit
import           Data.Conduit.Combinators (takeExactly)
import Control.Monad.IO.Class
import System.IO (withFile, openFile, IOMode (..))
import Network.HTTP.Conduit

import System.Log.FastLogger

import Lib.Thread
import Lib.Thread.DownloadThread

someFunc :: IO ()
someFunc = putStrLn "someFunc"

test_main = do
  logger <- newStdoutLoggerSet 8192

  let loop n t = do
        -- CC.threadDelay (1 * 1000000)
        pushLogStrLn logger (toLogStr ("Hello from " ++ show n ++ "!"))
        putStr "~"
        shouldStop <- threadShouldStop t
        if shouldStop then return n else loop n t

  threads <- mapM (runThread . loop) [1..5::Int]

  -- CC.threadDelay (2 * 1000000)

  forM_ threads stopThread
  putStrLn "stopped threads"
  mapM waitThread threads >>= print
