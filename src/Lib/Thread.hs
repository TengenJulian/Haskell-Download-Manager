module Lib.Thread
  ( runThread
  , getThreadStatus
  , setThreadError
  , stopThread
  , threadShouldStop
  , isThreadDone
  , waitThread
  , stopWaitThread
  , threadId
  , ThreadRunningState (..)
  , ThreadStatus
  , Thread
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar

import qualified Control.Concurrent as CC

import           Control.Exception (try, fromException, SomeException)

import           Data.Maybe (fromJust)

import Lib.DownloadError
import Lib.Thread.Log
import Lib.Thread.Types

runThread :: (Thread r -> IO r) -> IO (Thread r)
runThread action = do
  statusVar <- newTVarIO mempty
  resultVar <- newEmptyTMVarIO

  tid <- CC.forkIO $ do

    tid' <- CC.myThreadId
    let thread = Thread statusVar resultVar tid'
        setStatusResult s r = atomically $ do
          modifyTVar' (threadStatus thread) (`mappend` s)
          putTMVar resultVar r

    setThreadStatus thread (Right Running)
    r <- try $ action thread

    case r of
      Left e -> do
        logInfoThread thread " thread action failed"
        let mDownloadError = fromException e :: Maybe DownloadError

        let endStatus = case mDownloadError of
              Just de -> Left de
              Nothing -> Left (UnknownError (e :: SomeException))

        setStatusResult endStatus Nothing

      Right r' -> do
        s <- getThreadStatus thread

        let endStatus = case s of
              Right Stopping -> Right Stopped
              _              -> Right Finished

        setStatusResult endStatus (Just r')

  return (Thread statusVar resultVar tid)

setThreadError :: Thread r -> DownloadError -> IO ()
setThreadError t = setThreadStatus t . Left

setThreadStatus :: Thread r -> ThreadStatus -> IO ()
setThreadStatus t s = atomically $ modifyTVar' (threadStatus t) (`mappend` s)

getThreadStatus :: Thread r -> IO ThreadStatus
getThreadStatus t = readTVarIO (threadStatus t)

stopThread :: Thread r -> IO ()
stopThread t = setThreadStatus t (Right Stopping)

threadShouldStop :: Thread r -> IO Bool
threadShouldStop t = p <$> getThreadStatus t
  where p (Right Stopping) = True
        p _                = False

isThreadDone :: Thread r -> IO Bool
isThreadDone t = do
  b <- atomically . isEmptyTMVar . threadResult $ t

  return (not b)

waitThread :: Thread r -> IO (Either DownloadError r)
waitThread t = do
  r <- atomically (takeTMVar $ threadResult t)
  s <- getThreadStatus t

  case s of
    Right _ -> return (Right (fromJust r))
    Left e  -> return (Left e)

stopWaitThread :: Thread r -> IO (Either DownloadError r)
stopWaitThread t =  stopThread t >> waitThread t
