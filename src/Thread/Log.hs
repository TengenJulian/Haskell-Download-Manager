module Thread.Log where

import Log
import Thread.Types

logInfoThread :: Thread r -> String -> IO ()
logInfoThread t = logInfo . prependThreadId t

logWarnThread :: Thread r -> String -> IO ()
logWarnThread t = logWarn . prependThreadId t

logErrorThread :: Thread r -> String -> IO ()
logErrorThread t = logError . prependThreadId t

prependThreadId :: Thread r -> String -> String
prependThreadId t = ((show (threadId t) ++ " - ") ++)
