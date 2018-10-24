module Download where

import Control.Concurrent.STM.TVar

import Data.List (intercalate)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

import Network.HTTP.Conduit (parseRequest, Request (..))

import System.FilePath

import Thread

data Download = Download
  { dlUrl :: String
  , dlDir :: String
  , dlFileName :: String
  , dlNumWorkThreads :: Int
  , dlInfo :: TVar DownloadInfo
  , dlCachedInfo :: DownloadInfo
  , dlCachedStatus :: ThreadStatus
  }

instance Show Download where
  show dl = "Download {" ++ intercalate ", "
    [
    "dlUrl = " ++ dlUrl dl
    , "dlDir = " ++ dlDir dl
    , "dlFileName = " ++ dlFileName dl
    , "dlNumWorkThreads = " ++ show (dlNumWorkThreads dl)
    , "dlCachedInfo = " ++ show (dlCachedInfo dl)
    ] ++ "}"

data DownloadInfo = DownloadInfo
  { dlSpeed :: Double
  , dlEta :: (Double, Double)
  , dlBytesDownloaded :: Int
  , dlSize :: Maybe Int
  , dlThread :: Maybe (Thread ())
  }
  deriving (Show)

dlPath :: Download -> FilePath
dlPath dl = dlDir dl </> dlFileName dl

mkDownloadInfo :: DownloadInfo
mkDownloadInfo = DownloadInfo 0 (0, 0) 0 Nothing Nothing

mkDownload :: String -> FilePath -> Int -> IO Download
mkDownload url dir numThreads = do
  req  <- parseRequest url
  info <- newTVarIO mkDownloadInfo

  let p = unpack . decodeUtf8 . path $ req
  let filename
        | null (takeFileName p) || last p == '/' = "index.html"
        | otherwise                              = takeFileName p

  return Download
    {
      dlUrl = url
    , dlDir = dir
    , dlFileName = filename
    , dlNumWorkThreads = numThreads
    , dlInfo = info
    , dlCachedInfo = mkDownloadInfo
    , dlCachedStatus = mempty
    }
