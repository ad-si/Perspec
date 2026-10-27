module Home where

import Protolude (
  Applicative (pure),
  FilePath,
  Fractional ((/)),
  IO,
  Maybe (..),
  Num,
  putText,
  void,
  ($),
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
import Data.IORef (IORef, readIORef)
import Types (AppState (..))
import Utils (isInRect)


{-| Open the file dialog without blocking the event loop
and pass the selected files to 'openFiles'
-}
openFileDialogAsync :: ([FilePath] -> IO ()) -> IO ()
openFileDialogAsync openFiles = do
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
      Just files -> openFiles files


{-| Handle an event of the home view.
'openFiles' shows the given files in the image view
and loads them in the background.
-}
handleHomeEvent ::
  IORef AppState ->
  ([FilePath] -> IO ()) ->
  Controller ->
  Event ->
  AppState ->
  IO AppState
handleHomeEvent stateRef openFiles controller event appState = do
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
    EventDrop filePaths -> do
      openFiles filePaths
      -- Returned, as the state is replaced with the returned one afterwards
      readIORef stateRef
    EventKey (MouseButton Gl.LeftButton) Gl.Down _ clickedPoint -> do
      let fileSelectBtnWasClicked = clickedPoint `isInRect` fileSelectBtnRect
      if fileSelectBtnWasClicked
        then do
          openFileDialogAsync openFiles
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
