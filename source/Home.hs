module Home where

import Protolude (
  Applicative (pure),
  Fractional ((/)),
  IO,
  Maybe (..),
  Num,
  putText,
  void,
  ($),
  (<&>),
 )

import Brillo.Data.Cursor (CursorShape (..))
import Brillo.Data.FileDialog (FileDialog (..), SelectionMode (..))
import Brillo.Interface.Environment (openFileDialog)
import Brillo.Interface.IO.Game as Gl (
  Event (..),
  Key (MouseButton),
  KeyState (Down),
  MouseButton (LeftButton),
 )
import Brillo.Interface.IO.Interact (Controller (..))
import Control.Concurrent (forkOS)
import Data.IORef (IORef, readIORef, writeIORef)
import Types (AppState (..), ImageData (..), View (..))
import Utils (isInRect, loadFileIntoState)


-- | Open file dialog and update state directly when complete
openFileDialogAsync :: IORef AppState -> Controller -> IO ()
openFileDialogAsync stateRef controller = do
  void $ forkOS $ do
    let fileDialog =
          FileDialog
            { title = "Open File"
            , defaultPath = "/"
            , filterPatterns = ["*.jpeg", "*.jpg", "*.png"]
            , filterDescription = "Image files"
            , selectionMode = MultiFileSelect
            }
    selectedFiles <- openFileDialog fileDialog
    case selectedFiles of
      Nothing -> do
        putText "No file selected"
      Just files -> do
        appState <- readIORef stateRef
        let newState =
              appState
                { currentView = ImageView
                , images = files <&> \filePath -> ImageToLoad{filePath}
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
      -- Set cursor based on hover state
      controllerSetCursor controller $ if isOverBtn then CursorHand else CursorArrow
      pure appState{hoveredButton = newHoveredBtn}
    _ -> pure appState
