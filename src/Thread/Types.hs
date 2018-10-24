{-# LANGUAGE FlexibleInstances #-}
module Thread.Types where

import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import qualified Control.Concurrent as CC

import DownloadError

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
