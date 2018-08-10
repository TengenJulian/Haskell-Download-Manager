module Lib.Log
  (
    module Lib.Log
  ) where

import System.IO
import System.GlobalLock

info :: String -> IO ()
info s = lock (putStrLn s >> hFlush stdout)
