{-# LANGUAGE FlexibleInstances #-}
module Lib.Thread
  (
    module Lib.Thread
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Control.Exception (try, fromException, SomeException)

import qualified Control.Concurrent as CC

import Data.Maybe (fromJust)

import Lib.Log
import Lib.DownloadError

data ThreadRunningState = Unstarting | Running | Finished | Paused | Stopping deriving (Show, Eq)

instance Monoid ThreadRunningState where
  mempty = Unstarting

  mappend Unstarting x = x
  mappend x Unstarting = x
  mappend Running Paused = Paused
  mappend Running Finished = Finished
  mappend Running Stopping = Stopping
  mappend Stopping Finished = Finished
  mappend x _ = x

type ThreadStatus = Either DownloadError ThreadRunningState

instance Monoid (Either DownloadError ThreadRunningState) where
  mempty = Right mempty

  mappend (Left e) (Right _) = Left e
  mappend _ (Left e) = Left e
  mappend (Right l) (Right r) = Right (l `mappend` r)

data Thread r = Thread
  {
    threadStatus :: TVar ThreadStatus
  , threadResult :: TMVar (Maybe r)
  }

runThread :: (Thread r -> IO r) -> IO (Thread r)
runThread action = do
  statusVar <- newTVarIO mempty
  resultVar <- newEmptyTMVarIO

  let thread = Thread statusVar resultVar
  _ <- CC.forkIO $ do

    setThreadStatus thread (Right Running)
    r <- try $ action thread

    case r of
      Left e -> do
        info "thread action failed"
        let mDownloadError = fromException e :: Maybe DownloadError

        case mDownloadError of
          Just de -> setThreadStatus thread (Left de)
          Nothing -> setThreadStatus thread (Left (UnknownError (e :: SomeException)))

        atomically $ putTMVar resultVar Nothing

      Right r' -> do
        setThreadStatus thread (Right Finished)
        atomically $ putTMVar resultVar (Just r')

  return thread

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

waitThread :: Thread r -> IO (Either DownloadError r)
waitThread t = do
  r <- atomically (takeTMVar $ threadResult t)
  s <- getThreadStatus t

  case s of
    Right _ -> return (Right (fromJust r))
    Left e  -> return (Left e)

