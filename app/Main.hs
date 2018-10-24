module Main where

import           Brick
import qualified Brick.BChan           as B
import qualified Brick.Widgets.List    as L

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (unless, forM_, when)
import           Control.Monad.Catch (catch)

import           Data.Vector (toList)
import           Data.Semigroup ((<>))

import qualified Graphics.Vty as V
import           Graphics.Vty.Config (defaultConfig)

import           Lens.Micro
import           Network.HTTP.Conduit
import           Options.Applicative

import           System.Directory (removeFile, getHomeDirectory)
import           System.FilePath ((</>))
import           System.Exit (die)

import Download
import Log
import Thread
import Tui (app, TuiEvent (..), initState, TuiState (..), downloadRemoveQueueL)
import Util

data ArgOptions = ArgOptions
  { argDstDir :: FilePath
  , argNumThreads :: Int
  , argEnableDebug :: Bool
  } deriving (Show)

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

argOptions :: FilePath -> Parser ArgOptions
argOptions defaultDir = ArgOptions
  <$> strOption
      (    long "dir"
        <> help "Directory to save downloads"
        <> metavar "DIR"
        <> showDefault
        <> value defaultDir )
  <*> option auto
      (    long "num-threads"
        <> short 'n'
        <> help "Number of threads per download"
        <> metavar "NUM"
        <> showDefault
        <> value 1 )
  <*> switch
      (    long "enable-debug"
        <> short 'd'
        <> help "Whether to enable debugging" )

main = do
  homeFolder <- getHomeDirectory

  let defaultDir = homeFolder </> "Downloads"

      opts = info (argOptions defaultDir <**> helper)
        (    fullDesc
          <> progDesc "Start the download manager"
          <> header "hdm - Haskell download manager" )

  args <- execParser opts
  mainWithArgs args

mainWithArgs :: ArgOptions -> IO ()
mainWithArgs args = do
  mErrMsg <- checkDir (argDstDir args)

  case mErrMsg of
    Just e  -> die ("Invalid download directory.\n" ++ e ++ "\nExiting...")
    Nothing -> return ()

  when (argEnableDebug args)
    initLogger

  eventChan <- B.newBChan 32

  m  <- newManager tlsManagerSettings
  ut <- genUpdateEventTUI eventChan
  st <- customMain (V.mkVty defaultConfig)
                   (Just eventChan) app (initState [] (argDstDir args) m (argNumThreads args))

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
