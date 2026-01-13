module Home where

import Protolude (
  Applicative (pure),
  Bool (..),
  Fractional ((/)),
  IO,
  Int,
  Maybe (..),
  Num,
  putText,
  void,
  ($),
 )

import Brillo.Interface.IO.Game as Gl (
  Event (..),
  Key (MouseButton),
  KeyState (Down),
  MouseButton (LeftButton),
 )
import Control.Concurrent (forkOS, newEmptyMVar, putMVar)

import TinyFileDialogs (openFileDialog)
import Types (AppState (..))
import Utils (isInRect)


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
      -- Spawn file dialog in separate OS thread to keep UI responsive
      resultVar <- newEmptyMVar
      void $ forkOS $ do
        selectedFiles <-
          openFileDialog
            {- Title -} "Open File"
            {- Default path -} "/"
            {- File patterns -} ["*.jpeg", "*.jpg", "*.png"]
            {- Filter description -} "Image files"
            {- Allow multiple selects -} True
        putMVar resultVar selectedFiles
      -- Return immediately with pending dialog; stepWorld will poll for result
      pure appState{pendingFileDialog = Just resultVar}


handleHomeEvent :: Event -> AppState -> IO AppState
handleHomeEvent event appState = do
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
  case event of
    EventKey (MouseButton Gl.LeftButton) Gl.Down _ clickedPoint -> do
      let fileSelectBtnWasClicked = clickedPoint `isInRect` fileSelectBtnRect
      if fileSelectBtnWasClicked
        then handleMsg OpenFileDialog appState
        else pure appState
    EventMotion mousePoint -> do
      let
        isOverBtn = mousePoint `isInRect` fileSelectBtnRect
        newHoveredBtn = if isOverBtn then Just 0 else Nothing
      pure appState{hoveredButton = newHoveredBtn}
    _ -> pure appState
