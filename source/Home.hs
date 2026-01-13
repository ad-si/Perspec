module Home where

import Protolude (
  Applicative (pure),
  Bool (..),
  Fractional ((/)),
  IO,
  Maybe (..),
  Num,
  putText,
  void,
  ($),
  (<&>),
 )

import Brillo.Interface.IO.Game as Gl (
  Event (..),
  Key (MouseButton),
  KeyState (Down),
  MouseButton (LeftButton),
 )
import Brillo.Interface.IO.Interact (Controller (..))
import Control.Concurrent (forkOS)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text qualified as T

import TinyFileDialogs (openFileDialog)
import Types (AppState (..), ImageData (..), View (..))
import Utils (isInRect, loadFileIntoState)


-- | Open file dialog and update state directly when complete
openFileDialogAsync :: IORef AppState -> Controller -> IO ()
openFileDialogAsync stateRef controller = do
  void $ forkOS $ do
    selectedFiles <-
      openFileDialog
        {- Title -} "Open File"
        {- Default path -} "/"
        {- File patterns -} ["*.jpeg", "*.jpg", "*.png"]
        {- Filter description -} "Image files"
        {- Allow multiple selects -} True
    case selectedFiles of
      Nothing -> do
        putText "No file selected"
      Just files -> do
        appState <- readIORef stateRef
        let newState =
              appState
                { currentView = ImageView
                , images =
                    files <&> \filePath ->
                      ImageToLoad{filePath = T.unpack filePath}
                }
        loadedState <- loadFileIntoState newState
        writeIORef stateRef loadedState
        controllerSetRedraw controller


handleHomeEvent ::
  IORef AppState -> Controller -> Event -> AppState -> IO AppState
handleHomeEvent stateRef controller event appState = do
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
        then do
          openFileDialogAsync stateRef controller
          pure appState
        else pure appState
    EventMotion mousePoint -> do
      let
        isOverBtn = mousePoint `isInRect` fileSelectBtnRect
        newHoveredBtn = if isOverBtn then Just 0 else Nothing
      pure appState{hoveredButton = newHoveredBtn}
    _ -> pure appState
