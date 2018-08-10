{-# LANGUAGE OverloadedStrings #-}
module Lib.Thread.DownloadThread where
  -- (
  --   module Lib.Thread.DownloadThread
  -- ) where

import qualified Data.ByteString as S (hPut, length, ByteString (..))
import           Data.ByteString.UTF8 (fromString)
import           Data.ByteString.Char8 (readInt)
import           Data.Either.Combinators (isLeft, mapLeft, maybeToLeft, fromLeft)
import           Data.Int (Int64)
import           Data.Maybe
import           Data.List (mapAccumL, unfoldr)
import           Data.Conduit
import           Data.Conduit.Combinators (takeExactly, sourceHandle, sinkHandle)
import qualified System.Clock as CL
import           Conduit (MonadResource (..), runResourceT)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Catch (try, finally, bracket, MonadCatch, Exception)
import           Network.HTTP.Simple (getResponseHeader, setRequestHeaders)
import           Network.HTTP.Conduit
import           System.IO.Temp (openTempFile)
import           System.FilePath
import           System.Directory (removeFile)
import           System.IO (withFile, openFile, hClose, hGetPosn, hFlush, hSeek, SeekMode (..), IOMode (..), Handle)

import Lib.Thread
import Lib.Log
import Lib.UserAgent
import Lib.Download
import Lib.DownloadError
import qualified Lib.DownloadMeasurement as DM

data SlaveData = SlaveData (TVar Int)

downloadLink = "http://ipv4.download.thinkbroadband.com/5MB.zip"

cleanUpFiles :: [(FilePath, Handle)] -> IO ()
cleanUpFiles fs = forM_ fs $ \(filePath, handle) -> do
  hClose handle
  removeFile filePath

safeOpenTempFiles :: Int -> FilePath -> String -> IO (Maybe [(FilePath, Handle)])
safeOpenTempFiles n filePath fileName = loop n []
  where loop 0 hs = return (Just hs)
        loop n hs = do
          r <- try $ openTempFile filePath fileName :: IO (Either IOError (FilePath, Handle))

          case r of
            Right p -> loop (n - 1) (hs ++ [p])
            Left _  -> do
              cleanUpFiles hs

              return Nothing

withTempFiles :: Int -> FilePath -> String -> ([(FilePath, Handle)] -> IO a) -> IO a
withTempFiles n filePath filename = bracket openTmps cleanUpFiles
  where openTmps = do
          tmpsM <- safeOpenTempFiles n filePath filename
          case tmpsM of
            Just tmps -> return tmps
            Nothing   -> return []

liftAction :: (MonadCatch m, Exception e) => (e -> e') -> m a -> ExceptT e' m a
liftAction f a = ExceptT $ mapLeft f <$> try a

liftMaybe :: MonadCatch m => e -> ExceptT e m (Maybe a) -> ExceptT e m a
liftMaybe e a = do
  m <- a
  case m of
    Nothing -> throwE e
    Just x  -> return x

-- Divides the `total` number of bytes, into at most `n` inclusive ranges that cover all the bytes.
-- All ranges are of the same size, with the exception of the last range, which also contains all
-- of the remaining bytes.
-- Assuming that `minSize` <= `total`, every range covers at least `minSize` bytes.
calcRanges :: Int -> Int -> Int -> [(Int, Int)]
calcRanges n minSize total = unfoldr aux total
  where d = max (total `div` n) minSize
        aux rest | rest <= 0      = Nothing
                 | d + n >= rest  = Just ((total - rest, total - 1), 0)
                 | otherwise      = Just ((total - rest, total - rest + d - 1), rest - d)

testDownload :: String -> Int -> IO ()
testDownload url numThreads = do
  manager <- newManager tlsManagerSettings
  dlInfo' <- newTVarIO mkDownloadInfo
  let dl = Download {
          dlUrl = url
        , dlNumWorkThreads = numThreads
        , dlFileDir = "/tmp"
        , dlFileName = "file.data"
        , dlInfo = dlInfo'
        , dlCachedInfo = mkDownloadInfo
        }
  startTime <- CL.getTime CL.Monotonic
  t <- startDownload manager dl

  r <- waitThread t

  endTime <- CL.getTime CL.Monotonic
  print r

  print $ CL.diffTimeSpec endTime startTime

getDownloadResources :: Manager -> Download -> ExceptT DownloadError IO (Request, Int, [(Int, Int)], [(FilePath, Handle)])
getDownloadResources manager dl = do
  baseReq <- liftAction UrlParseError $ parseRequest (dlUrl dl)
  clM     <- liftAction HttpError $ getContentLength manager baseReq

  unless (isJust clM) $
    throwE NoContentLength

  let Just cl = clM

  supported <- liftIO $ isHttpRangeSupported manager baseReq
  unless supported $
    throwE HttpRangeNotSupported

  let ranges = calcRanges (dlNumWorkThreads dl) 8192 cl

  tmpFiles <- liftMaybe FailedCreatingTempFiles . liftIO $
    safeOpenTempFiles (length ranges) (dlFileDir dl) (dlFileName dl)

  return (baseReq, cl, ranges, tmpFiles)

startDownloadFromResources :: Thread () -> Manager -> Request -> Int -> [(Int, Int)] -> [(FilePath, Handle)] -> Download -> IO ()
startDownloadFromResources t manager baseReq cl ranges tmpFiles dl = do
  let (_, handles) = unzip tmpFiles

  byteVar <- newTVarIO 0
  threads <- forM (zip handles ranges) $ \(handle, (left, right)) -> do
    userAgent <- randomUserAgent

    let range = fromString $ mconcat ["bytes=", show left, "-", show right]
        req   = setRequestHeaders [("Range", range), ("User-Agent", userAgent)] baseReq
        size  = right - left + 1

    startDownloadWorker handle manager req byteVar size

  let loop dm = do
  -- let loop oldTime lastUpdate oldB s ss d ds = do
        status <- getThreadStatus t
        bytesDownloaded <- readTVarIO byteVar

        newTime <- CL.getTime CL.Monotonic

        case status of
          Right Stopping            -> do
            forM_ threads stopThread
            return False
          _ | bytesDownloaded == cl ->
            return True
          _                         -> do
            ss <- forM threads getThreadStatus
            let errors = [e | Left e <- ss]

            if null errors
              then do
              newDm <- case DM.updateMeasurement dm newTime bytesDownloaded of
                Left DM.NoUpdate -> return dm
                Left DM.Timeout  -> return dm
                Right dm'        -> do
                  -- info $ "Bytes: " ++ show bytesDownloaded
                  info $ "Speed: " ++ show (DM.downloadSpeed dm')
                  -- info $ show dm'
                  atomically . modifyTVar' (dlInfo dl) $ \oldInfo ->
                    oldInfo
                    {
                      dlSpeed = DM.downloadSpeed dm'
                    , dlBytesDownloaded = DM.bytesDownloaded dm'
                    }

                  return dm'

              threadDelay 100000
              loop newDm
              else do
              info "Worker thread failed"
              forM_ threads stopThread

              setThreadStatus t (Left (head errors))
              return False

  startTime <- CL.getTime CL.Monotonic
  sucess <- loop (DM.emptyDownloadMeasurement startTime)
  mapM_ waitThread threads

  when sucess $ do
    forM_ handles $ \h -> do
      hFlush h
      hSeek h AbsoluteSeek 0

    let filePath = dlFileDir dl </> dlFileName dl
    withFile filePath WriteMode $ \outHandle ->
      runConduitRes . forM_ handles $ \h -> sourceHandle h .| sinkHandle outHandle

startDownload :: Manager -> Download -> IO (Thread ())
startDownload manager dl = runThread $ \t -> do
  res <- runExceptT (getDownloadResources manager dl)

  case res of
    Left e ->
      setThreadStatus t (Left e)
    Right (baseReq, cl, ranges, tmpFiles) ->
      startDownloadFromResources t manager baseReq cl ranges tmpFiles dl `finally`
        cleanUpFiles tmpFiles

testSlave = withFile "/tmp/test-file" WriteMode $ \h -> do
  manager <- newManager tlsManagerSettings
  clO <- getContentLength manager downloadLink

  case clO of
    Nothing -> info "Could not get content Length"
    Just cl -> do
      byteCount <- newTVarIO 0
      info ("Content-length: " ++ show cl)

      t <- startDownloadWorker h manager downloadLink byteCount cl

      waitThread t
      downloaded <- readTVarIO byteCount

      info ("Downloaded: " ++ show downloaded ++ " bytes")
      return ()

getContentLength :: Manager -> Request -> IO (Maybe Int)
getContentLength manager req = do

    let conduit = do
          response <- http req manager
          let cl = listToMaybe (getResponseHeader "Content-Length" response)
                    >>= readInt
                    >>= return . fst

          return cl
    runConduitRes conduit

isHttpRangeSupported :: Manager -> Request -> IO Bool
isHttpRangeSupported manager baseReq = do
  let req = setRequestHeaders [("Range", "bytes=0-11")] baseReq

  cl <- getContentLength manager req
  return (cl == Just 12)

startDownloadWorker :: Handle -> Manager -> Request -> TVar Int -> Int -> IO (Thread ())
startDownloadWorker h manager req bytesDownloaded bytes = runThread $ \t -> do
    let addByteCount = liftIO . atomically . modifyTVar' bytesDownloaded . (+)

    let loop _   n | n <= 0 = return ()
        loop res n = do
          shouldStop <- liftIO (threadShouldStop t)
          if shouldStop
            then return ()
            else do (res', numBytes) <- res $$++ takeExactly 4 (sinkHandleCountBytes h)
                    addByteCount numBytes
                    loop res' (n - numBytes)

    let conduit = do
          response <- http req manager
          (res, numBytes) <- responseBody response $$+ takeExactly 4 (sinkHandleCountBytes h)
          addByteCount numBytes

          loop res (bytes - numBytes)

    runConduitRes conduit

sinkHandleCountBytes :: MonadIO m => Handle -> ConduitT S.ByteString o m Int
sinkHandleCountBytes h = loop 0
      where loop n = do
              r <- await
              case r of
                Just c -> liftIO (S.hPut h c) >> loop (n + S.length c)
                Nothing -> return n
