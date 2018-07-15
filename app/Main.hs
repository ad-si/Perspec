{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game as Gl
import System.Environment
import Protolude as P
import Data.Text as T


type Corner = Point

-- | State of app (list of corners is reversed to order of addition)
data AppState = AppState [Corner] [Picture]
  deriving Show


load :: FilePath -> IO Picture
load filePath = do
  picMaybe <- loadJuicy filePath

  case picMaybe of
    Nothing -> do
      putText "Error: Image couldn't be loaded"
      pure Blank
    Just picture -> pure picture
    -- display
    --   (InWindow filePath (width, height) )
    --   black
    --   picture


startApp :: FilePath -> Picture -> IO ()
startApp filePath pic = do
  let
    width = 640
    height = 320
    initialX = 800
    initalY = 100
    initalState = AppState [] [pic]

  putText "Starting the app …"

  play
    (InWindow filePath (width, height) (initialX, initalY))
    black
    10
    initalState
    makePicture
    handleEvent
    stepWorld


stepWorld :: Float -> AppState -> AppState
stepWorld _ = identity


-- | Render the app state to a picture.
makePicture :: AppState -> Picture
makePicture (AppState corners pics) =
  let
    radius = 6
    thickness = 4
    drawCorner (x, y) =
      Translate x y (color green $ ThickCircle radius thickness)
  in
    Pictures $ pics <> (fmap drawCorner corners)


addCorner :: AppState -> Corner -> AppState
addCorner (AppState corners pics) corner =
  AppState (corner : corners) pics


handleEvent :: Event -> AppState -> AppState
handleEvent event appState =
  case event of
    EventKey (MouseButton LeftButton) Gl.Down _ point ->
      addCorner appState point
    _ ->
      appState


-- | Displays uncompressed 24/32 bit BMP images.
main :: IO ()
main = do
  args <- getArgs

  case args of
    [filePath] -> do
      image <- load filePath
      startApp filePath image
    _ ->
      putText $ T.unlines
        [ "usage: bitmap <file.bmp>"
        , "  file.bmp should be a 24 or 32-bit uncompressed BMP file"
        ]
