module Lib.Download where
import Control.Concurrent.STM.TVar

data Download = Download
  {
    dlUrl :: String
  , dlFileDir :: String
  , dlFileName :: String
  , dlNumWorkThreads :: Int
  , dlInfo :: TVar DownloadInfo
  , dlCachedInfo :: DownloadInfo
  }

data DownloadInfo = DownloadInfo
  {
    dlSpeed :: Double
  , dlEta :: Double
  , dlBytesDownloaded :: Int
  , dlSize :: (Maybe Int)
  }

mkDownloadInfo :: DownloadInfo
mkDownloadInfo = DownloadInfo 0 0 0 Nothing
