module Lib.Log
  (
    module Lib.Log
  ) where

import System.IO

info :: String -> IO ()
info s = putStrLn s >> hFlush stdout
