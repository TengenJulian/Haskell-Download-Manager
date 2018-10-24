module DownloadError
  (
    DownloadError (..)
  )
where

import Control.Exception (SomeException, Exception)
import Network.HTTP.Conduit (HttpException)

data DownloadError = HttpError HttpException
                   | NoContentLength
                   | FailedCreatingTempFiles
                   | HttpRangeNotSupported
                   | UnknownError SomeException
                 deriving (Show)

instance Exception DownloadError
