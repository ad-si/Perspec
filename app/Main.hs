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


data ConversionMode
  = CallConversion
  | SpawnConversion


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
  , scaleFactor :: Float
  }
  deriving Show


initialState :: AppState
initialState = AppState
  { corners = []
  , layers = []
  , imgViewWidth = 1280
  , imgViewHeight = 960
  , imgWidth = 0
  , imgHeight = 0
  , inputPath = ""
  , outputPath = ""
  , scaleFactor = 1
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
    maxWidth = imgViewWidth stateWithImage
    maxHeight = imgViewHeight stateWithImage
    wdthFrac = fromIntegral imgWdth
    hgtFrac = fromIntegral imgHgt
    scaleFactorX = (fromIntegral maxWidth) / wdthFrac
    scaleFactorY = (fromIntegral maxHeight) / hgtFrac
    scaleFac = min scaleFactorX scaleFactorY
    imgWScaled = round $ scaleFac * (fromIntegral imgWdth)
    imgHScaled = round $ scaleFac * (fromIntegral imgHgt)
    distance = 0.1

    stateWithImage = initialState
      { corners = originTopLeft (-imgWScaled) imgHScaled $
          scalePoints (1 / scaleFac) $ P.reverse
            [ (wdthFrac * distance, hgtFrac * distance)
            , (wdthFrac * (1 - distance), hgtFrac * distance)
            , (wdthFrac * (1 - distance), hgtFrac * (1 - distance))
            , (wdthFrac * distance, hgtFrac * (1 - distance))
            ]
      , layers = [Scale scaleFac scaleFac pic]
      , imgWidth = imgWScaled
      , imgHeight = imgHScaled
      , inputPath = inPath
      , outputPath = outPath
      , scaleFactor = scaleFac
      }

    window = InWindow
      inPath
      (maxWidth, maxHeight)
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
    drawEdges points =
      color (makeColor 0.2 1 0.5 0.4) $ Polygon points
  in
    pure $ Pictures $
      (layers appState)
      <> (fmap drawCorner $ corners appState)
      <> [drawEdges $ corners appState]

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
getTargetShape (topLeft, topRight, btmRight, btmLeft) =
  let
    topEdgeLength    = calcDistance topLeft topRight
    bottomEdgeLength = calcDistance btmLeft btmRight
    width            = max topEdgeLength bottomEdgeLength

    leftEdgeLength   = calcDistance topLeft btmLeft
    rightEdgeLength  = calcDistance topRight btmRight
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


scalePoints :: Float -> [Point] -> [Point]
scalePoints scaleFac = fmap $
  \(x, y) -> (x / scaleFac, y / scaleFac)


getCorners :: AppState -> [Point]
getCorners appState =
  scalePoints (scaleFactor appState) $ originTopLeft
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

      exitSuccess

    EventKey (SpecialKey KeyEsc) Gl.Down _ _ -> do
      pure $ appState { corners = [] }

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
  , "-define", "distort:viewport="
      <> (show $ fst shape) <> "x" <> (show $ snd shape) <> "+0+0"

  -- TODO: Add flag to support this
  -- Use interpolated lookup instead of area resampling
  -- https://www.imagemagick.org/Usage/distorts/#area_vs_super
  -- , "-filter", "point"

  -- Prevent interpolation of unused pixels and avoid adding alpha channel
  , "-virtual-pixel", "black"

  -- TODO: Add flag to support switching
  -- , "-virtual-pixel", "Edge" -- default
  -- , "-virtual-pixel", "Dither"
  -- , "-virtual-pixel", "Random"
  -- TODO: Implement more sophisticated one upstream in Imagemagick

  , "-distort", "Perspective", showProjectionMap projMap
  , "+repage"
  , (T.pack outPath)
  ]


correctAndWrite :: [Text] -> IO ()
correctAndWrite args = do
  let
    conversionMode = CallConversion
    -- TODO: Add CLI flag to automatically use local during build
    -- convertBin = "convert"  -- global
    convertBin = "./convert"  -- local

  -- TODO: Add CLI flag to switch between them
  case conversionMode of
    CallConversion -> do
      callProcess convertBin (fmap T.unpack args)
      putText $ "✅ Successfully saved converted image"

    SpawnConversion -> do
      _ <- spawnProcess convertBin (fmap T.unpack args)
      putText $ "✅ Successfully initiated conversion"

  pure ()


loadAndStart :: FilePath -> IO ()
loadAndStart filePath = do
  let outName = (takeBaseName filePath) <> "-fixed"

  image@(Bitmap imgWdth imgHgt _ _) <- load filePath
  putStrLn $ "Loaded file " <> filePath <> " " <> (show (imgWdth,imgHgt))
  startApp
    filePath
    (replaceBaseName filePath outName)
    imgWdth
    imgHgt
    image


helpMessage :: Text
helpMessage =
  T.unlines [ "Usage: perspec <image> [image…]" ]


main :: IO ()
main = do
  args <- getArgs

  case args of
    [filePath] -> loadAndStart filePath
    _          -> putText helpMessage
