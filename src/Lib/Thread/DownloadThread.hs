{-# LANGUAGE OverloadedStrings #-}

module Lib.Thread.DownloadThread where

import qualified Data.ByteString as S (hPut, length, ByteString)
import           Data.ByteString.UTF8 (fromString)
import           Data.ByteString.Char8 (readInt)
import           Data.Either.Combinators (maybeToRight, mapLeft)
import           Data.Maybe
import           Data.List (unfoldr)
import           Data.Conduit
import           Data.Conduit.Combinators (takeExactly, sourceHandle, sinkHandle)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Catch (try, finally, MonadCatch, Exception)

import           Network.HTTP.Conduit
import           Network.HTTP.Simple (getResponseHeader, setRequestHeaders)

import           System.IO.Temp (openTempFile)
import           System.Directory (removeFile)
import qualified System.Clock as CL
import           System.IO (withFile, hClose, hFlush, hSeek, SeekMode (..), IOMode (..), Handle)

import Lib.Thread
import Lib.Log
import Lib.UserAgent
import Lib.Download
import Lib.DownloadError
import qualified Lib.DownloadMeasurement as DM

downloadLink :: Request
downloadLink = "http://ipv4.download.thinkbroadband.com/5MB.zip"

cleanUpFiles :: [(FilePath, Handle)] -> IO ()
cleanUpFiles fs = forM_ fs $ \(filePath, handle) -> do
  hClose handle
  removeFile filePath

safeOpenTempFiles :: Int -> FilePath -> String -> IO (Maybe [(FilePath, Handle)])
safeOpenTempFiles n filePath fileName = loop n []
  where loop m hs
          | m <= 0    = return (Just hs)
          | otherwise = do
              r <- try $ openTempFile filePath fileName :: IO (Either IOError (FilePath, Handle))

              case r of
                Right p -> loop (m - 1) (hs ++ [p])
                Left _  -> do
                  cleanUpFiles hs

                  return Nothing

testDownload :: String -> Int -> IO ()
testDownload url numThreads = do
  manager <- newManager tlsManagerSettings
  dlInfo' <- newTVarIO mkDownloadInfo
  let dl = Download {
          dlUrl = url
        , dlNumWorkThreads = numThreads
        , dlDir = "/tmp"
        , dlFileName = "file.data"
        , dlInfo = dlInfo'
        , dlCachedInfo = mkDownloadInfo
        , dlCachedStatus = Right Starting
        }
  startTime <- CL.getTime CL.Monotonic
  t <- startDownload manager dl

  r <- waitThread t

  endTime <- CL.getTime CL.Monotonic
  print r

  print $ CL.diffTimeSpec endTime startTime

-- | This function also sets the downloadThread, before the function returns
startDownload :: Manager -> Download -> IO (Thread ())
startDownload manager dl = runThread threadAction >>= setDlThread
  where setDlThread t = do
          atomically . modifyTVar (dlInfo dl) $ \info' -> info' {dlThread = Just t}
          return t

        threadAction t = do

          res <- runExceptT (getDownloadResources manager dl)

          case res of
            Left e ->
              setThreadError t e
            Right (baseReq, cl, ranges, tmpFiles) ->
              startDownloadFromResources t manager baseReq cl ranges tmpFiles dl `finally`
              cleanUpFiles tmpFiles

getDownloadResources :: Manager -> Download -> ExceptT DownloadError IO (Request, Int, [(Int, Int)], [(FilePath, Handle)])
getDownloadResources manager dl = do
  baseReq <- liftAction HttpError $ parseUrlThrow (dlUrl dl)
  clM     <- liftAction HttpError $ getContentLength manager baseReq

  when (isNothing clM) $
    throwE NoContentLength
  let Just cl = clM

  supported <- liftIO $ isHttpRangeSupported manager baseReq
  unless supported $
    throwE HttpRangeNotSupported

  let ranges = calcRanges (dlNumWorkThreads dl) 8192 cl

  tmpFiles <- liftMaybe FailedCreatingTempFiles $
    safeOpenTempFiles (length ranges) (dlDir dl) (dlFileName dl)

  return (baseReq, cl, ranges, tmpFiles)

liftAction :: (MonadCatch m, Exception e) => (e -> e') -> m a -> ExceptT e' m a
liftAction f a = ExceptT $ mapLeft f <$> try a

liftMaybe :: MonadCatch m => e -> m (Maybe a) -> ExceptT e m a
liftMaybe e mm = ExceptT $ maybeToRight e <$> mm

-- Divides the 'total' number of bytes, into at most 'n' inclusive ranges that cover all the bytes.
-- All ranges are of the same size, with the exception of the last range, which also contains all
-- of the remaining bytes.
-- Assuming that 'minSize' <= 'total', every range covers at least 'minSize' bytes.
calcRanges :: Int -> Int -> Int -> [(Int, Int)]
calcRanges n minSize total = unfoldr aux total
  where d = max (total `div` n) minSize
        aux rest | rest <= 0      = Nothing
                 | d + n >= rest  = Just ((total - rest, total - 1), 0)
                 | otherwise      = Just ((total - rest, total - rest + d - 1), rest - d)

startDownloadFromResources :: Thread () -> Manager -> Request -> Int -> [(Int, Int)] -> [(FilePath, Handle)] -> Download -> IO ()
startDownloadFromResources t manager baseReq cl ranges tmpFiles dl = do
  let (_, handles) = unzip tmpFiles

  atomically . modifyTVar (dlInfo dl) $ \info' -> info' {dlSize = Just cl}

  byteVar <- newTVarIO 0
  threads <- forM (zip handles ranges) $ \(handle, (left, right)) -> do
    userAgent <- randomUserAgent

    let range = fromString $ mconcat ["bytes=", show left, "-", show right]
        req   = setRequestHeaders [("Range", range), ("User-Agent", userAgent)] baseReq
        size  = right - left + 1

    startDownloadWorker handle manager req byteVar size

  let loop dm = do
        status <- getThreadStatus t
        bytesDownloaded <- readTVarIO byteVar

        newTime <- CL.getTime CL.Monotonic

        case status of
          Right Stopping            -> do
            forM_ threads stopThread
            return False
          _ | bytesDownloaded == cl -> do
            atomically . modifyTVar' (dlInfo dl) $ \oldInfo ->
              oldInfo { dlBytesDownloaded = cl}

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
                  atomically . modifyTVar' (dlInfo dl) $ \oldInfo ->
                    oldInfo {
                      dlSpeed = DM.downloadSpeed dm'
                    , dlBytesDownloaded = bytesDownloaded
                    , dlEta = DM.etaEq dm'
                    }

                  return dm'

              threadDelay (100 * 1000)
              loop newDm

              else do
              info "Worker thread failed"
              forM_ threads stopThread

              setThreadError t (head errors)
              return False

  startTime <- CL.getTime CL.Monotonic
  sucess <- loop (DM.emptyDownloadMeasurement startTime)
  mapM_ waitThread threads

  when sucess $ do
    forM_ handles $ \h -> do
      hFlush h
      hSeek h AbsoluteSeek 0

    let filePath = dlPath dl
    withFile filePath WriteMode $ \outHandle ->
      runConduitRes . forM_ handles $ \h -> sourceHandle h .| sinkHandle outHandle

testSlave :: IO ()
testSlave = withFile "/tmp/test-file" WriteMode $ \h -> do
  manager <- newManager tlsManagerSettings
  clO <- getContentLength manager downloadLink

  case clO of
    Nothing -> info "Could not get content Length"
    Just cl -> do
      byteCount <- newTVarIO 0
      info ("Content-length: " ++ show cl)

      t <- startDownloadWorker h manager downloadLink byteCount cl

      _ <- waitThread t
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
