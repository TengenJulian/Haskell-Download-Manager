module Main where

import           Brick
import qualified Brick.BChan           as B
import qualified Brick.Widgets.List    as L

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (unless, forM_)
import           Control.Monad.Catch (catch)

import           Data.Vector (toList)

import qualified Graphics.Vty as V
import           Graphics.Vty.Config (defaultConfig)

import           Lens.Micro
import           Network.HTTP.Conduit

import           System.Directory (removeFile, getHomeDirectory)
import           System.FilePath ((</>))
import           System.Exit (die)

import Lib.Download
import Lib.Log
import Lib.Thread
import Lib.Tui (app, TuiEvent (..), initState, TuiState (..), downloadRemoveQueueL)
import Lib.Util

oneMB :: Num a => a
oneMB = 1024 * 1024

genUpdateEventTUI :: B.BChan TuiEvent -> IO (Thread ())
genUpdateEventTUI chan = runThread $ \t -> do
  let loop = do
        b <- threadShouldStop t

        unless b $ do
          B.writeBChan chan UpdateTui
          threadDelay (300 * 1000)
          loop
  loop

main :: IO ()
main = do
  homeFolder  <- getHomeDirectory

  let dstDir = homeFolder </> "Downloads"
  mErrMsg <- checkDir dstDir

  case mErrMsg of
    Just e  -> die ("Invalid download direcotry.\n" ++ e ++ "\nExiting...")
    Nothing -> return ()

  eventChan   <- B.newBChan 32

  m  <- newManager tlsManagerSettings
  ut  <- genUpdateEventTUI eventChan
  st <- customMain (V.mkVty defaultConfig)
                   (Just eventChan) app (initState [] dstDir m)

  putStrLn "Exiting..."
  stopWaitThread ut

  let dls = st ^. downloadRemoveQueueL ++ (toList . L.listElements . downloads) st
  forM_ dls $ \dl -> do
        info <- readTVarIO (dlInfo dl)

        case dlThread info of
          Nothing -> return ()
          Just t  -> do
            stopWaitThread t

            s <- getThreadStatus t

            case s of
              Right Finished -> return ()
              _              -> removeFile (dlPath dl)
                                `catch` ((\_ -> return ()) :: IOError -> IO ())
