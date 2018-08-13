{-# LANGUAGE FlexibleInstances #-}
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

import Lib.Log
import Lib.DownloadError

data ThreadRunningState = Starting | Running | Finished | Paused | Stopping | Stopped deriving (Show, Eq)

instance Monoid ThreadRunningState where
  mempty = Starting

  mappend Starting x = x
  mappend x Starting = x
  mappend Running Paused = Paused
  mappend Running Finished = Finished
  mappend Running Stopping = Stopping
  mappend Stopping Stopped = Stopped
  mappend x _ = x

type ThreadStatus = Either DownloadError ThreadRunningState

instance Monoid (Either DownloadError ThreadRunningState) where
  mempty = Right mempty

  mappend (Left e) (Right _) = Left e
  mappend _ (Left e) = Left e
  mappend (Right l) (Right r) = Right (l `mappend` r)

data Thread r = Thread
  { threadStatus :: TVar ThreadStatus
  , threadResult :: TMVar (Maybe r)
  , threadId :: CC.ThreadId
  }

instance Eq (Thread r) where
  (==) t1 t2 = threadId t1 == threadId t2

instance Show (Thread r) where
  show t = "Thread <" ++ show (threadId t) ++ ">"

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
        info "thread action failed"
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
