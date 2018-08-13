module Lib.Util where

import System.IO (withFile, IOMode (..))
import System.IO.Error
import System.Directory
import System.FilePath
import qualified System.Clock as CL

import Control.Monad.Catch (catch)

-- | Checks if FilePath is an existing directory and can be read/written.
-- Otherwise an error message is returned.
checkDir :: FilePath -> IO (Maybe String)
checkDir dir = do
  let handler e
        | isDoesNotExistError e = return . Just $ "Directory \'" ++ dir ++ "\' does not exist"
        | isPermissionError e   = return . Just $ "Couldn't get permissions for \'" ++ dir ++ "\'"
        | otherwise             = return . Just $ "Unknown error occured"

      action = do
        p <- getPermissions dir
        b <- doesDirectoryExist dir
        if not (writable p && readable p)
          then return . Just $ "Directory \'" ++ dir ++ "\' does not have read and write permissions"
          else if not b
          then return . Just $ "\'" ++ dir ++ "\' is not a directory or does not exist"
          else return Nothing

  action `catch` handler

-- | Creates a an empty file, with a filename that is similar to 'path'
createUnusedFile :: FilePath -> IO FilePath
createUnusedFile path = do
  let (path', ext) = splitExtension path

      loop i = do
        let p = path' ++ "-" ++ show i ++ ext
        b <- doesFileExist path
        b' <- doesFileExist p

        if not b
          then return path
          else if b'
               then loop (i + 1)
               else return p

  newPath <- loop (1 :: Int)
  withFile newPath WriteMode $ \_ -> return ()

  return newPath

toSecs :: CL.TimeSpec -> Double
toSecs t = fromIntegral (CL.sec t) + fromIntegral (CL.nsec t) / (1000000000 :: Double)
