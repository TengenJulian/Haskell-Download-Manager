{-# LANGUAGE OverloadedStrings, TemplateHaskell, RankNTypes #-}

module Tui where

import           Numeric

import           Brick
import qualified Brick.AttrMap         as A
import qualified Brick.Widgets.Border  as B
import qualified Brick.Widgets.Center  as C
import qualified Brick.Widgets.Dialog  as D
import qualified Brick.Widgets.Edit    as E
import qualified Brick.Widgets.List    as L
import qualified Brick.Widgets.ProgressBar as PB

import           Control.Monad (forM, when, filterM)
import           Control.Monad.IO.Class
import           Control.Monad.Catch (fromException, try, displayException)
import           Control.Concurrent.STM.TVar

import           Data.ByteString (ByteString )
import           Data.Either.Combinators (mapRight)
import           Data.List (intercalate)
import           Data.Maybe (isNothing, isJust)
import qualified Data.Vector  as V
import           Data.Text (unpack)
import           Data.Text.Encoding (decodeUtf8')
import           Data.Text.Zipper (gotoEOL, insertMany)

import qualified Graphics.Vty as V

import           Network.HTTP.Conduit
import           Network.HTTP.Types

import           System.IO.Error (ioeGetErrorString, doesNotExistErrorType, ioeGetErrorType)
import           System.Directory (removeFile)
import qualified System.Clock as CL
import           System.FilePath (takeFileName)

import           Text.Read (readMaybe)

import Lens.Micro

import Download
import Downloader
import DownloadError
import Thread
import Util

data Name = DownloadList
          | QuitDialog
          | CancelDownloadDialog
          | AddDownloadDialog
          | AddDownloadEditor
          | SettingsDialog
          | SettingsDirEditor
          | SettingsNumThreadEditor
          deriving (Show, Eq, Ord)

data TuiState = TuiState
  { downloads :: L.List Name Download
  , mainDialog :: MainDialog
  , focus :: Name
  , downloadDir :: FilePath
  , numThreads :: Int
  , downloadRemoveQueue :: [Download]
  , manager :: Manager
  , time :: Double
  }

data MainDialog = MainDialog
  { mainBrickDialog :: D.Dialog Bool
  , dialogErrorMsg :: Maybe String
  , dialogFocus :: Maybe Int
  , editors :: [(String, E.Editor String Name )]
  }

data TuiEvent = UpdateTui

suffixLenses ''TuiState
suffixLenses ''MainDialog

maxThreadsPerDownload :: Num a => a
maxThreadsPerDownload = 16

mkMainDialog :: Maybe String -> Maybe (Int, [(String, Bool)]) -> [(String, E.Editor String Name)] -> MainDialog
mkMainDialog title buttons = MainDialog dlg Nothing Nothing
  where dlg = D.dialog title buttons 60

initState :: [Download] -> FilePath -> Manager -> Int -> TuiState
initState dls downloadDir' m numThreads' = TuiState list (mkMainDialog Nothing Nothing []) DownloadList downloadDir' numThreads' [] m 0
  where list = L.list DownloadList (V.fromList dls) 2

dialogHasFocus :: TuiState -> Bool
dialogHasFocus st = DownloadList /= st ^. focusL

app :: App TuiState TuiEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = initBrick
          , appAttrMap = const theMap
          }

initBrick :: TuiState -> EventM Name TuiState
initBrick st = do
  vty <- getVtyHandle

  let output = V.outputIface vty
  when (V.supportsMode output V.BracketedPaste) $
      liftIO $ V.setMode output V.BracketedPaste True

  return st

handleEvent :: TuiState -> BrickEvent Name TuiEvent -> EventM Name (Next TuiState)
handleEvent st (AppEvent UpdateTui) = do
  t <- liftIO (toSecs <$> CL.getTime CL.Monotonic)

  traverseOf downloadsL (liftIO . updateDownloadList) st {time = t}
    >>= liftIO . processDownloadRemoveQueue >>= continue

handleEvent st (VtyEvent e) = handleVtyEvent st e
handleEvent st _ = continue st

updateDownloadList :: L.List Name Download -> IO (L.List Name Download)
updateDownloadList ls = do
  let dls = L.listElements ls

  newDls <- forM dls $ \dl -> do
    newInfo <- readTVarIO (dlInfo dl)
    case dlThread newInfo of
      Nothing -> return dl {dlCachedInfo = newInfo}
      Just t -> do
        s <- getThreadStatus t
        return dl {dlCachedInfo = newInfo, dlCachedStatus = s}

  return $ L.listReplace newDls (L.listSelected ls) ls

processDownloadRemoveQueue :: TuiState -> IO TuiState
processDownloadRemoveQueue st = do
  let p dl = do
        info <- readTVarIO (dlInfo dl)

        case dlThread info of
          Nothing -> return True
          Just t  -> do
            done <- isThreadDone t

            when done $
              removeFile (dlPath dl)

            return (not done)

  newQueue <- filterM p (st ^. downloadRemoveQueueL)

  return (st & downloadRemoveQueueL .~ newQueue)


handleVtyEvent :: TuiState -> V.Event -> EventM Name (Next TuiState)
handleVtyEvent st e
  | not (dialogHasFocus st) = case e of
      V.EvKey (V.KChar 'a') []  -> continue . setFocusAndDialog st $ AddDownloadEditor
      V.EvKey (V.KChar 'c') []
        | isJust (st ^. downloadsL . L.listSelectedL) ->
          continue . setFocusAndDialog st $ CancelDownloadDialog
      V.EvKey (V.KChar 's') []  -> continue . setFocusAndDialog st $ SettingsDialog
      V.EvKey (V.KChar 'q') []  -> continue . setFocusAndDialog st $ QuitDialog
      V.EvKey V.KEsc []         -> continue . setFocusAndDialog st $ QuitDialog
      _                         -> focusHandleEvent st e >>= continue

  | otherwise         = case e of
      V.EvKey V.KEsc []         -> continue (st & focusL .~ DownloadList)

      V.EvKey (V.KChar '\t') [] ->
        case st ^. focusL of
          AddDownloadDialog -> continue (st & focusL .~ AddDownloadEditor)
          _                 -> focusHandleEvent st e >>= continue

      V.EvKey (V.KChar 'd') []  | st ^. focusL == SettingsDialog ->
        continue $ st & setFocus SettingsDirEditor
      V.EvKey (V.KChar 'n') []  | st ^. focusL == SettingsDialog ->
        continue $ st & setFocus SettingsNumThreadEditor
      V.EvKey V.KEnter []       -> handleVtyEnter st
      _                         -> focusHandleEvent st e >>= continue

setFocusAndDialog :: TuiState -> Name -> TuiState
setFocusAndDialog st focus' = st & focusL .~ focus' & mainDialogL .~ mainDlg'
  where mainDlg' = case focus' of
          QuitDialog              -> mkMainDialog Nothing (Just (0, okCancel)) []
          CancelDownloadDialog    -> mkMainDialog (Just " Cancel Download? ") (Just (0, okCancel)) []
          AddDownloadDialog       -> addDlDialog
          AddDownloadEditor       -> addDlDialog & dialogFocusL ?~ 0
          SettingsDialog          -> settingsDialog
          SettingsDirEditor       -> settingsDialog & dialogFocusL ?~ 0
          SettingsNumThreadEditor -> settingsDialog & dialogFocusL ?~ 1
          _                       -> mkMainDialog Nothing Nothing []

        okCancel = [("Ok", True), ("Cancel", False)]
        addDlDialog = mkMainDialog
          (Just " Add Download ") (Just (0, [("Add", True), ("Cancel", False)]))
          [("Url: ", E.editor AddDownloadEditor (Just 1) "")]

        settingsDialog = mkMainDialog
          (Just " Settings ") (Just (0, okCancel)) [
          ("Download Directory:", E.applyEdit gotoEOL $ E.editor SettingsDirEditor (Just 1) (st ^. downloadDirL))
          , ("Number of threads per download: ", E.applyEdit gotoEOL $
              E.editor SettingsNumThreadEditor (Just 1) (show $ st ^. numThreadsL))
          ]

-- | This function assumes that the correct dialog is set.
setFocus :: Name -> TuiState -> TuiState
setFocus focus' st = case focus' of
  AddDownloadEditor       -> st & focusL .~ focus' & mainDialogL . dialogFocusL ?~ 0
  SettingsDirEditor       -> st & focusL .~ focus' & mainDialogL . dialogFocusL ?~ 0
  SettingsNumThreadEditor -> st & focusL .~ focus' & mainDialogL . dialogFocusL ?~ 1
  _                       -> st & focusL .~ focus' & mainDialogL . dialogFocusL .~ Nothing

handleVtyEnter :: TuiState -> EventM Name (Next TuiState)
handleVtyEnter st = case (st ^. focusL, D.dialogSelection (st ^.mainDialogL . mainBrickDialogL )) of
  (_, Just False)                      -> continue (st & focusL .~ DownloadList)
  (QuitDialog, Just True)              -> halt st
  (AddDownloadEditor, _)               -> handleAddDownloadEnter st >>= continue
  (AddDownloadDialog, Just True)       -> handleAddDownloadEnter st >>= continue
  (SettingsDialog, Just True)          -> handleSettingsEnter st >>= continue
  (SettingsDirEditor, _)               -> handleSettingsEnter st >>= continue
  (SettingsNumThreadEditor, _)         -> handleSettingsEnter st >>= continue
  (CancelDownloadDialog, Just True)    ->
    case st ^. downloadsL & L.listSelectedElement of
      Nothing       -> continue st
      Just (i, dl) -> do
        let rm = do
              info <- readTVarIO (dlInfo dl)

              case dlThread info of
                Just t  -> stopThread t
                Nothing -> return ()

        liftIO rm

        continue $ st & downloadRemoveQueueL %~ (dl :)
                      & downloadsL           %~ L.listRemove i
                      & setFocus DownloadList

  _                                    -> continue st

handleAddDownloadEnter :: TuiState -> EventM Name TuiState
handleAddDownloadEnter st
  | null (st ^. mainDialogL . editorsL) = return st
  | otherwise = do
      let url = head . E.getEditContents . snd . head . editors . mainDialog $ st

      dlE <- liftIO . try $ mkDownload url (st ^. downloadDirL) (st ^. numThreadsL)

      case dlE of
        Left (InvalidUrlException _ reason) ->
            return $ st & mainDialogL . dialogErrorMsgL ?~ reason
        Left _ ->
            return $ st & mainDialogL . dialogErrorMsgL ?~ "Invalid Url"
        Right dl
            | st ^. focusL == AddDownloadDialog -> do
                newDlE <- liftIO . try $ do
                  newPath <- createUnusedFile (dlPath dl)
                  return dl {dlFileName = takeFileName newPath}

                case newDlE of
                  Left ioe ->
                    let errMsg = "Could not create download file in \'"
                                 ++ (st ^. downloadDirL)
                                 ++ "\'. Reason: "
                                 ++ ioeGetErrorString ioe
                    in return $ st & mainDialogL . dialogErrorMsgL ?~ errMsg
                  Right newDl -> do
                    liftIO $ startDownload (st ^. managerL) newDl
                    return $ st & downloadsL %~ L.listMoveBy 1 . L.listInsert 0 newDl
                                & setFocus DownloadList

            | otherwise ->
                return $ st & mainDialogL . dialogErrorMsgL .~ Nothing
                            & setFocus AddDownloadDialog

handleSettingsEnter :: TuiState -> EventM Name TuiState
handleSettingsEnter st
  | length (st ^. mainDialogL . editorsL) /= 2 = return st
  | otherwise =
      case st ^. focusL of
        SettingsDirEditor -> do
          mErrMsg <- liftIO $ checkDir dir
          case mErrMsg of
            Just errMsg -> return $ st & mainDialogL . dialogErrorMsgL ?~ errMsg
            Nothing     -> return $ st & mainDialogL . dialogErrorMsgL .~ Nothing
                                       & setFocus SettingsDialog
        SettingsNumThreadEditor ->
          case readMaybe n :: Maybe Int of
            Nothing -> return $ st & mainDialogL . dialogErrorMsgL ?~ ("\'" ++ n ++ "\'" ++ " is not a number")
            Just n'
              | n' > maxThreadsPerDownload ->
                return $ st & mainDialogL . dialogErrorMsgL ?~ "Maximum threads per download is 16"
              | otherwise ->
                return $ st & mainDialogL . dialogErrorMsgL .~ Nothing
                            & setFocus SettingsDialog
        _ -> do

          mErrMsg <- liftIO $ checkDir dir

          case (mErrMsg, readMaybe n :: Maybe Int) of
            (Just errMsg, _)   -> return $ st & mainDialogL . dialogErrorMsgL ?~ errMsg
            (_, Nothing)       -> return $ st & mainDialogL . dialogErrorMsgL ?~
                                  "\'" ++ n ++ "\'" ++ " is not a number"
            (Nothing, Just n')
              | n' > maxThreadsPerDownload ->
                return $ st & mainDialogL . dialogErrorMsgL ?~ "Maximum threads per download is 16"
              | otherwise ->
                return $ st & numThreadsL  .~ n'
                            & downloadDirL .~ dir
                            & setFocus DownloadList

  where dir = head . E.getEditContents . snd . head   . editors .  mainDialog $ st
        n   = head . E.getEditContents . snd . (!! 1) . editors .  mainDialog $ st

focusHandleEvent :: TuiState -> V.Event -> EventM Name TuiState
focusHandleEvent st (V.EvPaste paste) = handlePaste st paste
focusHandleEvent st e = case st ^. focusL of
  DownloadList            -> handleEventTraversal st downloadsL L.handleListEvent e
  AddDownloadEditor       -> handleEventTraversal st (mainDialogL . editorsL . ix 0 . _2) E.handleEditorEvent e
  SettingsDirEditor       -> handleEventTraversal st (mainDialogL . editorsL . ix 0 . _2) E.handleEditorEvent e
  SettingsNumThreadEditor -> handleEventTraversal st (mainDialogL . editorsL . ix 1 . _2) E.handleEditorEvent e
  _                       -> handleEventTraversal st (mainDialogL . mainBrickDialogL) D.handleDialogEvent e

handleEventTraversal :: s -> Traversal' s a -> (e -> a -> EventM n a) -> e -> EventM n s
handleEventTraversal st t h e = traverseOf t (h e) st

handlePaste :: TuiState -> ByteString -> EventM Name TuiState
handlePaste st paste
  | (st ^. focusL) `notElem` eds = return st
  | otherwise = case st ^. mainDialogL . dialogFocusL of
      Nothing -> return st
      Just i  ->
        case mapRight unpack (decodeUtf8' paste) of
          Left  _ -> return $ st & mainDialogL . dialogErrorMsgL ?~ "Could not paste text with unsupported encoding"
          Right s -> return $ st & mainDialogL . editorsL . ix i . _2 %~ E.applyEdit (insertMany s)
                                 & mainDialogL . dialogErrorMsgL      .~ Nothing

  where eds = [AddDownloadEditor, SettingsDirEditor, SettingsNumThreadEditor]

drawUI :: TuiState -> [Widget Name]
drawUI st = [
  renderDialog st
  , vBox [ C.hCenter . renderDownloadList (time st) $ (st ^. downloadsL)
         , renderKbHelp st
         ]
  ]

renderDownloadList :: Double -> L.List Name Download -> Widget Name
renderDownloadList curTime dlList = B.borderWithLabel (str " Downloads ") $
                                    L.renderList (renderDl curTime) True dlList

renderDl :: Double -> Bool -> Download -> Widget Name
renderDl curTime _ dl = case (dlCachedStatus dl, dlSize info) of
  (Left e, _)         ->
    filename <=> (padRight Max . padLeft (Pad 2)) (renderDownloadError e)
  (Right Finished, Just cl) ->
    (filename <+> padLeft (Pad 2) (byteInfo cl))
    <=> padLeft Max (progBar cl)
  (Right Running, Just cl) ->
    (filename <+> padLeft (Pad 2) (byteInfo cl))
    <=> (padLeft (Pad 2) (str "eta ") <+> eta cl <+> str " @ " <+> speed <+> padLeft Max (progBar cl))
  _ ->
    filename <=> padLeft Max (str " ")

  where info = dlCachedInfo dl

        filename = whiteStr "* " <+> str (dlFileName dl)
        speed  = str (fmtBytes (dlSpeed info) ++ "/s")

        progBar cl = progressBar $ fromIntegral (dlBytesDownloaded info) / fromIntegral cl

        byteInfo cl = str ("[" ++ fmtBytes (fromIntegral (dlBytesDownloaded info))
                                  ++ " / " ++ fmtBytes ( fromIntegral cl) ++ "]")

        eta cl = str . fmtDeltaTime . max 0 . round $ secs
          where (b, m) = dlEta info

                secs = if m == 0 then 0 else (fromIntegral cl - b) / m - curTime

renderDownloadError :: DownloadError -> Widget Name
renderDownloadError e = withAttr errorAttr (strWrap errMsg)
  where errMsg = case e of
          UnknownError ex         -> "Unknown error: " ++ displayException ex
          HttpError ex            -> "Http error: " ++ httpExceptionErrorMsg ex
          NoContentLength         -> "Could not retrieve size of download"
          HttpRangeNotSupported   -> "Website doesn't support multi-part downloading"
          FailedCreatingTempFiles -> "Could not create temporary files"

httpExceptionErrorMsg :: HttpException -> String
httpExceptionErrorMsg (InvalidUrlException _ reason) = reason
httpExceptionErrorMsg (HttpExceptionRequest _ e) = httpExceptionContentErrorMsg e

httpExceptionContentErrorMsg :: HttpExceptionContent -> String
httpExceptionContentErrorMsg (StatusCodeException res _) =
  "status " ++ show (statusCode s) ++ ", " ++ show (statusMessage s)
  where s = responseStatus res
httpExceptionContentErrorMsg (TooManyRedirects _ )            = "Too many redirects"
httpExceptionContentErrorMsg ResponseTimeout                  = "Timeout, server took too long to response"
httpExceptionContentErrorMsg ConnectionTimeout                = "Timeout, took too long to connect to server"
httpExceptionContentErrorMsg (ConnectionFailure e)            =
  case fromException e :: Maybe IOError of
    Just eio
      | ioeGetErrorType eio == doesNotExistErrorType -> "Failed to connect to the server"
      | otherwise                                    -> "io error, " ++ ioeGetErrorString eio
    Nothing -> "Failed to connect to the server"
httpExceptionContentErrorMsg (InvalidStatusLine _)            = "Server sent malformed response"
httpExceptionContentErrorMsg (InvalidHeader _)                = "Server sent malformed response"
httpExceptionContentErrorMsg (WrongRequestBodyStreamSize _ _) = "Server sent malformed response"
httpExceptionContentErrorMsg (ResponseBodyTooShort _ _)       = "Server sent malformed response"
httpExceptionContentErrorMsg InvalidChunkHeaders              = "Server sent malformed response"
httpExceptionContentErrorMsg IncompleteHeaders                = "Server sent malformed response"
httpExceptionContentErrorMsg e                                = show e

fmtBytes :: Double -> String
fmtBytes bs = showFFloat (Just 2) (bs / fst powAndLabel) (" " ++ snd powAndLabel)
  where powAndLabel
          | bs < 1000         = (1, "B")
          | bs < 500 * 10 ^ 3 = (10 ^ 3, "KB")
          | bs < 500 * 10 ^ 6 = (10 ^ 6, "MB")
          | bs < 500 * 10 ^ 9 = (10 ^ 9, "GB")
          | otherwise         = (10 ^ 12, "TB")

progressBar :: Float -> Widget Name
progressBar r = whiteStr "| " <+> hLimit 30 (PB.progressBar (Just percentage) r) <+> whiteStr " |"
  where percentage = show (round (r * 100) :: Int) ++ " %"

whiteStr :: String -> Widget Name
whiteStr = withAttr whiteAttr . str

fmtDeltaTime :: Int -> String
fmtDeltaTime secs =
  (if d > 0 then showPadZero2 d ++ ":" else "")
  ++ showPadZero2 h
  ++ ":" ++ showPadZero2 m
  ++ ":" ++ showPadZero2 s
  where (d, r) = divMod secs (3600 * 24)
        (h, r') = divMod r 3600
        (m, s) = divMod r' 60

        showPadZero2 x = if length sx < 2 then "0" ++ sx else sx
          where sx = show x

renderColors :: Widget Name
renderColors =     (hBox . map colorTxt . take  5 $ colors)
               <=> (hBox . map colorTxt . drop  5 $ colors)
  where colorTxt color = withAttr (attrName color) . str $ color
        colors = ["red" , "yellow" , "magenta" , "blue" , "cyan" , "brightBlack"
                 , "brightGreen" , "brightYellow" , "brightBlue" , "brightMagenta"
                 , "brightCyan"
                 ]

renderDialog :: TuiState -> Widget Name
renderDialog st = case st ^. focusL of
  DownloadList            -> emptyWidget
  QuitDialog              -> renderMainDialog (st ^. mainDialogL) . C.hCenter $ str "Do you want to quit?"
  AddDownloadEditor       -> overrideAttr D.buttonSelectedAttr D.buttonAttr  $ renderMainDialog (st ^. mainDialogL) emptyWidget
  AddDownloadDialog       -> renderMainDialog (st ^. mainDialogL) emptyWidget
  CancelDownloadDialog    -> renderMainDialog (st ^. mainDialogL) $
    let Just (_, dl) = st ^. downloadsL & L.listSelectedElement
    in vBox [
      C.hCenter $ str "Cancel download of the following file?"
      , str " "
      , strWrap ("file: " ++ dlFileName dl)
      , strWrap ("url:  " ++ dlUrl dl)
      ]
  SettingsDialog          -> renderMainDialog (st ^. mainDialogL) emptyWidget
  SettingsDirEditor       -> renderMainDialog (st ^. mainDialogL) emptyWidget
  SettingsNumThreadEditor -> renderMainDialog (st ^. mainDialogL) emptyWidget

renderMainDialog :: MainDialog -> Widget Name -> Widget Name
renderMainDialog m body
  | isNothing (m ^. dialogFocusL) = dialog
  | otherwise                     = overrideAttr D.buttonSelectedAttr D.buttonAttr dialog
  where dialog = D.renderDialog (m ^. mainBrickDialogL) . padTopBottom 3 $ (errorString <=> body <=> es)

        es = vBox . intercalate [str " "] $
          [[str h, renderEditor e (m ^. dialogFocusL == Just i)]
          | (i, (h, e)) <- zip [0..] (m ^. editorsL)
          ]

        errorString = case m ^. dialogErrorMsgL of
          Nothing -> str ""
          Just s  -> C.hCenter (withAttr errorAttr (strWrap s)) <=> str " "

        renderEditor editor isFocused = E.renderEditor (vBox . map str) isFocused editor

renderKbHelp :: TuiState -> Widget Name
renderKbHelp st = case st ^. focusL of
  DownloadList            ->
    C.hCenter (strWrap "[q/Esc] Quit program - [a] Add download - [c] Cancel Selected Download - [s] Settings")
    <=> C.hCenter (strWrap "[Up/Down] Scroll download list")
  SettingsDialog          ->
    C.hCenter (strWrap "[Left/Right] Select button - [Enter] Confirm choice - [d] Edit Download Dir - [n] Edit number of threads")
  AddDownloadEditor       ->
    helpEditor
  SettingsDirEditor       ->
    helpEditor
  SettingsNumThreadEditor ->
    helpEditor
  _                       ->
    C.hCenter (strWrap "[Left/Right] Select button - [Enter] Confirm choice")
  where helpEditor = C.hCenter (strWrap "[Enter] Confirm edit")

errorAttr :: A.AttrName
errorAttr = "error"

whiteAttr :: A.AttrName
whiteAttr = "white"

theMap :: AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr           , V.brightBlue `on` V.black)
    , (L.listSelectedAttr   , V.black  `on` V.brightBlue)
    , (errorAttr            , V.white `on` V.red)
    , (PB.progressCompleteAttr,   V.white `on` V.brightGreen)
    , (PB.progressIncompleteAttr, fg V.white )
    , (D.buttonSelectedAttr, V.black `on` V.brightBlue)
    , (E.editAttr, V.white  `on` V.blue)
    , (E.editFocusedAttr, V.white  `on` V.blue)
    , ("white", fg V.white)
    , ("red", fg V.red)
    , ("yellow", V.yellow `on` V.black)
    , ("magenta", V.magenta `on` V.black)
    , ("cyan", V.cyan `on` V.black)
    , ("brightBlack", V.brightBlack `on` V.black)
    , ("brightGreen", V.brightGreen `on` V.black)
    , ("brightYellow", V.brightYellow `on` V.black)
    , ("brightBlue", V.brightBlue `on` V.black)
    , ("brightMagenta", V.brightMagenta `on` V.black)
    , ("brightCyan", V.brightCyan `on` V.black)
    ]
