{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Data.Either
import Data.List as DL
import Data.Text as T
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as Gl
import Graphics.Gloss.Juicy
import Protolude as P
import System.FilePath
import System.Process


type Corner = Point

type CornersTup = (Corner, Corner, Corner, Corner)


-- | Projection map from corner to corner
type ProjMap =
  ( (Corner, Corner)
  , (Corner, Corner)
  , (Corner, Corner)
  , (Corner, Corner)
  )


-- | State of app (list of corners is reversed to order of addition)
data AppState = AppState
  { corners :: [Corner]
  , layers :: [Picture]
  , imgViewWidth :: Int
  , imgViewHeight :: Int
  , imgWidth :: Int
  , imgHeight :: Int
  , inputPath :: FilePath
  , outputPath :: FilePath
  }
  deriving Show


initialState :: AppState
initialState = AppState
  { corners = []
  , layers = []
  , imgViewWidth = 640
  , imgViewHeight = 320
  , imgWidth = 0
  , imgHeight = 0
  , inputPath = ""
  , outputPath = ""
  }


load :: FilePath -> IO Picture
load filePath = do
  picMaybe <- loadJuicy filePath

  case picMaybe of
    Nothing -> do
      putText "Error: Image couldn't be loaded"
      pure Blank
    Just picture -> pure picture


startApp :: FilePath -> FilePath -> Int -> Int -> Picture -> IO ()
startApp inPath outPath imgWdth imgHgt pic = do
  let
    initialX = 800
    initialY = 100
    ticksPerSecond = 10
    stateWithImage = initialState
      { layers = [pic]
      , imgWidth = imgWdth
      , imgHeight = imgHgt
      , inputPath = inPath
      , outputPath = outPath
      }
    window = InWindow
      inPath
      ((imgViewWidth stateWithImage), (imgViewHeight stateWithImage))
      (initialX, initialY)

  putText "Starting the app …"

  playIO
    window
    black
    ticksPerSecond
    stateWithImage
    makePicture
    handleEvent
    stepWorld


stepWorld :: Float -> AppState -> IO AppState
stepWorld _ =
  pure . identity


-- | Render the app state to a picture.
makePicture :: AppState -> IO Picture
makePicture appState =
  let
    radius = 6
    thickness = 4
    drawCorner (x, y) =
      Translate x y (color green $ ThickCircle radius thickness)
  in
    pure $
    Pictures $
    (layers appState) <> (fmap drawCorner (corners appState))


replaceElemAtIndex :: Int -> a -> [a] -> [a]
replaceElemAtIndex theIndex newElem (x:xs) =
  if theIndex == 0
  then newElem : xs
  else x : replaceElemAtIndex (theIndex - 1) newElem xs
replaceElemAtIndex _ _ [] = []


calcDistance :: Point -> Point -> Float
calcDistance (x1 , y1) (x2 , y2) =
  let
    xDelta = x1 - x2
    yDelta = y1 - y2
  in
    sqrt (xDelta * xDelta + yDelta * yDelta)


-- | Get index of closest point
getIndexClosest :: [Point] -> Point -> Int
getIndexClosest points point =
  let
    distances = fmap (calcDistance point) points
    minDistance = DL.minimum distances
  in
    fromMaybe 0 (elemIndex minDistance distances)


addCorner :: AppState -> Corner -> AppState
addCorner appState newCorner =
  let
    theCorners = corners appState
    newCorners =
      if (P.length theCorners) < 4
      then newCorner : theCorners
      else replaceElemAtIndex
        (getIndexClosest theCorners newCorner)
        newCorner
        theCorners
  in
    appState {corners = newCorners}


-- TODO: Use correct algorithm as described in the readme
getTargetShape :: CornersTup -> (Float, Float)
getTargetShape (topLeft, topRight, btmLeft, btmRight) =
  let
    topEdgeLength    = calcDistance topLeft topRight
    bottomEdgeLength = calcDistance btmRight btmLeft
    width            = max topEdgeLength bottomEdgeLength

    leftEdgeLength   = calcDistance topLeft btmRight
    rightEdgeLength  = calcDistance topRight btmLeft
    height           = max leftEdgeLength rightEdgeLength
  in
    (width, height)


toQuadTuple :: [a] -> Either Text (a, a, a, a)
toQuadTuple [tl, tr, br, bl] = Right (tl, tr, br, bl)
toQuadTuple _                = Left "The list must contain 4 values"


-- | Assuming coordinate system starts top left
-- | 'getProjectionMap clickShape targetShape'
getProjectionMap :: CornersTup -> (Float, Float) -> ProjMap
getProjectionMap (tl, tr, br, bl) (wdth, hgt) =
  ( (tl, (0,    0))
  , (tr, (wdth, 0))
  , (br, (wdth, hgt))
  , (bl, (0,    hgt))
  )


-- | Accomodate ImageMagick's counter-clockwise direction
toCounterClock :: (a, a, a, a) -> (a, a, a, a)
toCounterClock (tl, tr, br, bl) = (tl, bl, br, tr)


-- | Fix weird gloss coordinate system
originTopLeft :: Int -> Int -> [Point] -> [Point]
originTopLeft width height = fmap
  (\(x, y) ->
    ( x + ((fromIntegral width) / 2.0)
    , - (y - ((fromIntegral height) / 2.0))
    )
  )


getCorners :: AppState -> [Point]
getCorners appState =
  originTopLeft
    (imgWidth appState)
    (imgHeight appState)
    (P.reverse $ corners appState)


handleEvent :: Event -> AppState -> IO AppState
handleEvent event appState =
  case event of
    EventKey (MouseButton LeftButton) Gl.Down _ point ->
      pure $ addCorner appState point

    EventKey (SpecialKey KeyEnter) Gl.Down _ _ -> do
      let
        cornersTrans = getCorners appState
        cornerTuple = fromRight
          ((0,0), (0,0), (0,0), (0,0))
          (toQuadTuple cornersTrans)
        targetShape = getTargetShape cornerTuple
        projectionMap =
          getProjectionMap cornerTuple targetShape
        convertArgs = getConvertArgs
          (inputPath appState)
          (outputPath appState)
          (toCounterClock projectionMap)
          targetShape

      putText $ "Target shape: " <> (show targetShape)
      putText $ "Marked corners: " <> (show cornerTuple)
      putText $ "Arguments for convert command:\n" <> (T.unlines convertArgs)

      correctAndWrite convertArgs

      putText $ "✅ Successfully stored fixed image at \""
        <> (T.pack $ outputPath appState) <> "\""

      exitSuccess

    _ ->
      pure $ appState


-- FIXME: Don't rely on show implementation
showProjectionMap :: ProjMap -> Text
showProjectionMap pMap = pMap
  & show
  & T.replace "),(" " "
  & T.replace "(" ""
  & T.replace ")" ""


getConvertArgs :: FilePath -> FilePath -> ProjMap -> (Float, Float) -> [Text]
getConvertArgs inPath outPath projMap shape =
  [ (T.pack inPath)
  , "-distort", "Perspective", showProjectionMap projMap
  , "-crop", (show $ fst shape) <> "x" <> (show $ snd shape) <> "+0+0"
  , (T.pack outPath)
  ]


correctAndWrite :: [Text] -> IO ()
correctAndWrite args =
  callProcess "convert" (fmap T.unpack args)


-- | Displays uncompressed 24/32 bit BMP images.
main :: IO ()
main = do
  args <- getArgs

  case args of
    [filePath] -> do
      let outName = (takeBaseName filePath) <> "-fixed"

      image@(Bitmap imgWdth imgHgt _ _) <- load filePath
      putStrLn $ "Loaded file " <> filePath <> " " <> (show (imgWdth,imgHgt))
      startApp
        filePath
        (replaceBaseName filePath outName)
        imgWdth imgHgt image
    _ ->
      putText $ T.unlines
        [ "usage: bitmap <file.bmp>"
        , "  file.bmp should be a 24 or 32-bit uncompressed BMP file"
        ]
