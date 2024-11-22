module Home where

import Protolude (
  Applicative (pure),
  Bool (..),
  Fractional ((/)),
  IO,
  Int,
  Maybe (Just, Nothing),
  Num,
  putText,
 )

import Data.Text qualified as T
import Brillo.Interface.IO.Game as Gl (
  Event (..),
  Key (MouseButton),
  KeyState (Down),
  MouseButton (LeftButton),
 )

import TinyFileDialogs (openFileDialog)
import Types (AppState(..), View(..))
import Utils (isInRect, loadFileIntoState)


ticksPerSecond :: Int
ticksPerSecond = 10


data Message
  = ClickSelectFiles
  | OpenFileDialog


handleMsg :: Message -> AppState -> IO AppState
handleMsg msg appState =
  case msg of
    ClickSelectFiles -> do
      putText "ClickSelectFiles"
      pure appState
    OpenFileDialog -> do
      selectedFiles <-
        openFileDialog
          {- Title -} "Open File"
          {- Default path -} "/"
          {- File patterns -} ["*.jpeg", ".jpg", ".png"]
          {- Filter description -} "Image files"
          {- Allow multiple selects -} False

      case selectedFiles of
        Just [filePath] -> do
          stateWithFile <- loadFileIntoState
            appState{ currentView = ImageView }
            (T.unpack filePath)
          pure stateWithFile
        Just _files -> do
          putText "Selecting several files is not supported yet!"
          -- TODO
          -- putText $ "Selected file: " <> filePath
          -- loadAndStart appState filePath
          pure appState
        Nothing -> do
          putText "No file selected"
          pure appState


handleHomeEvent :: Event -> AppState -> IO AppState
handleHomeEvent event appState =
  case event of
    EventKey (MouseButton Gl.LeftButton) Gl.Down _ clickedPoint -> do
      let
        fileSelectBtnWidth :: (Num a) => a
        fileSelectBtnWidth = 120

        fileSelectBtnHeight :: (Num a) => a
        fileSelectBtnHeight = 40

        fileSelectBtnRect =
          ( -(fileSelectBtnWidth / 2)
          , -(fileSelectBtnHeight / 2)
          , fileSelectBtnWidth / 2
          , fileSelectBtnHeight / 2
          )
        fileSelectBtnWasClicked = clickedPoint `isInRect` fileSelectBtnRect

      if fileSelectBtnWasClicked
        then handleMsg OpenFileDialog appState
        else pure appState
    _ -> pure appState
