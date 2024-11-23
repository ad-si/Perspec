{-# LANGUAGE DataKinds #-}

module Lib where

import Protolude (
  Applicative (pure),
  Bool (..),
  Double,
  Either (Left, Right),
  Eq ((==)),
  FilePath,
  Float,
  Floating (sqrt),
  Fractional ((/)),
  Functor (fmap),
  IO,
  IOException,
  Int,
  Maybe (Just, Nothing),
  Monoid (mempty),
  Num ((*), (+), (-)),
  Ord (max, min, (<), (>)),
  Semigroup ((<>)),
  Text,
  const,
  either,
  exitSuccess,
  flip,
  fromIntegral,
  fromMaybe,
  fromRight,
  fst,
  not,
  putText,
  realToFrac,
  show,
  snd,
  try,
  when,
  ($),
  (&),
  (&&),
  (++),
  (<$>),
  (<&>),
 )
import Protolude qualified as P

import Brillo (
  Display (InWindow),
  Picture (Line, Pictures, Scale, ThickArc, ThickCircle, Translate),
  Point,
  bitmapOfBMP,
  black,
  color,
  greyN,
  lineLoop,
  makeColor,
  pictures,
  rectangleSolid,
 )
import Brillo.Interface.Environment (getScreenSize)
import Brillo.Interface.IO.Game as Gl (
  Color,
  Event (..),
  Key (MouseButton, SpecialKey),
  KeyState (Down, Up),
  MouseButton (LeftButton),
  SpecialKey (KeyEnter, KeyEsc),
  playIO,
 )
import Codec.BMP (parseBMP)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.List as DL (elemIndex, minimum)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TSE
import Home (handleHomeEvent)
import System.Directory (getCurrentDirectory)
import System.Environment (setEnv)
import System.FilePath (replaceExtension, (</>))
import System.Info (os)
import System.Process (callProcess, readProcessWithExitCode, spawnProcess)

-- hip
import Graphics.Image (
  Alpha,
  Bilinear (Bilinear),
  Image,
  Interpolation (interpolate),
  Ix2 (..),
  Linearity (Linear),
  SRGB,
  Sz (Sz),
  readImageRGBA,
  transform,
  writeImage,
 )

-- linear

import Correct (calculatePerspectiveTransform, determineSize)
import Linear (M33, V2 (V2), V3 (V3), V4 (V4), (!*))
import Types (
  AppState (..),
  Config (transformAppFlag),
  ConversionMode (CallConversion, SpawnConversion),
  Coordinate (..),
  Corner,
  CornersTup,
  ExportMode (..),
  ProjMap,
  TransformApp (..),
  UiComponent (Button, Select, text),
  View (..),
  initialState,
 )
import Utils (
  calcInitWindowPos,
  calculateSizes,
  getCorners,
  getWordSprite,
  loadFileIntoState,
  originTopLeft,
  scalePoints,
 )


-- | This is replaced with valid licenses during CI build
licenses :: [Text]
licenses = []


-- | Radius of the circles to mark the corners of the selection
cornCircRadius :: Float
cornCircRadius = 6


-- | Border thickness of the circles to mark the corners of the selection
cornCircThickness :: Float
cornCircThickness = 4


numGridLines :: Int
numGridLines = 7


gridColor :: Gl.Color
gridColor = makeColor 0.2 1 0.7 0.6


sidebarPaddingTop :: Int
sidebarPaddingTop = 50


sidebarGridHeight :: Int
sidebarGridHeight = 40


ticksPerSecond :: Int
ticksPerSecond = 10


bannerTime :: Float
bannerTime = 10 -- seconds


bannerImage :: Picture
bannerImage =
  either
    (const mempty)
    bitmapOfBMP
    (parseBMP (BL.fromStrict $(embedFile "images/banner.bmp")))


-- | Get initial corner positions by shelling out to a Python script
getInitialCorners :: AppState -> FilePath -> IO [Corner]
getInitialCorners appState inPath = do
  currentDir <- getCurrentDirectory

  let
    wdthFrac = fromIntegral appState.imgWidthOrig
    hgtFrac = fromIntegral appState.imgHeightOrig

    pyScriptPathMac = currentDir </> "scripts/perspectra/perspectra"
    pyScriptPathWindows = currentDir </> "TODO: Windows EXE path"

  -- Run the Python script
  let
    pyScriptPath =
      if os == "mingw32"
        then pyScriptPathWindows
        else pyScriptPathMac

  (exitCode, stdout, stderr) <-
    readProcessWithExitCode pyScriptPath ["corners", inPath] ""

  if exitCode == P.ExitSuccess
    then do
      let
        -- Parse JSON output in stdout with Aeson in the form of:
        -- [{x: 0, y: 0}, {x: 0, y: 0}, {x: 0, y: 0}, {x: 0, y: 0}]
        corners :: Maybe [Coordinate] =
          Aeson.decode $ BL.fromStrict (TSE.encodeUtf8 $ T.pack stdout)

      pure $
        corners
          & fromMaybe []
          & P.map (\coord -> (coord.x, coord.y))
          & originAtCenter appState.imgWidthOrig appState.imgHeightOrig
          & scalePoints (1 / appState.scaleFactor)
          & P.reverse
    else do
      P.putErrLn stderr

      let
        -- Initial distance of the corners from the image border
        distance = 0.1

      pure
        $ originTopLeft
          (-appState.imgWidthTrgt)
          appState.imgHeightTrgt
        $ scalePoints (1 / appState.scaleFactor)
        $ P.reverse
          [ (wdthFrac * distance, hgtFrac * distance)
          , (wdthFrac * (1 - distance), hgtFrac * distance)
          , (wdthFrac * (1 - distance), hgtFrac * (1 - distance))
          , (wdthFrac * distance, hgtFrac * (1 - distance))
          ]


appStateToWindow :: (Int, Int) -> AppState -> Display
appStateToWindow screenSize appState = do
  let appSize = (appState.appWidth, appState.appHeight)

  case appState.currentView of
    HomeView -> do
      InWindow
        "Perspec - Select a file"
        appSize
        (calcInitWindowPos screenSize appSize)
    ImageView -> do
      case appState.inputPath of
        Nothing -> InWindow "SHOULD NOT BE POSSIBLE" (100, 100) (0, 0)
        Just inPath ->
          InWindow
            ( "Perspec - "
                <> inPath
                <> if appState.isRegistered
                  then mempty
                  else " - ⚠️ NOT REGISTERED"
            )
            appSize
            (calcInitWindowPos screenSize appSize)
    BannerView ->
      InWindow "Perspec - Banner" (800, 600) (10, 10)


startApp :: AppState -> IO ()
startApp appState = do
  screenSize <- getScreenSize

  case (appState.inputPath, appState.outputPath) of
    (Just inPath, Just _outPath) -> do
      corners <- getInitialCorners appState inPath
      let stateWithImage = appState{corners = corners}

      putText "Starting the app …"

      playIO
        (appStateToWindow screenSize stateWithImage)
        black
        ticksPerSecond
        stateWithImage
        makePicture
        handleEvent
        stepWorld
    (_, _) -> do
      playIO
        (appStateToWindow screenSize appState)
        black
        ticksPerSecond
        appState
        makePicture
        handleEvent
        stepWorld


stepWorld :: Float -> AppState -> IO AppState
stepWorld _ appState =
  if not appState.isRegistered
    && ( fromIntegral appState.tickCounter
          < (bannerTime * fromIntegral ticksPerSecond)
       )
    then pure appState{tickCounter = appState.tickCounter + 1}
    else pure appState{bannerIsVisible = False}


drawCorner :: Point -> Picture
drawCorner (x, y) =
  Translate
    x
    y
    (color gridColor $ ThickCircle cornCircRadius cornCircThickness)


drawEdges :: [Point] -> Picture
drawEdges points =
  color gridColor $ lineLoop points


drawGrid :: [Point] -> Picture
drawGrid [p1, p2, p3, p4] =
  let
    numSegments = fromIntegral $ numGridLines + 1

    getLinePoint :: Int -> Int -> Point -> Point -> Point
    getLinePoint sgmnts idx pA pB =
      let
        fraction = 1 / fromIntegral sgmnts
      in
        ( fst pA + (fst pB - fst pA) * (fromIntegral idx * fraction)
        , snd pA + (snd pB - snd pA) * (fromIntegral idx * fraction)
        )

    getGridLineVert num =
      color gridColor $
        Line
          [ getLinePoint numSegments num p1 p2
          , getLinePoint numSegments num p4 p3
          ]

    getGridLineHor num =
      color gridColor $
        Line
          [ getLinePoint numSegments num p1 p4
          , getLinePoint numSegments num p2 p3
          ]
  in
    Pictures $
      ([1 .. numGridLines] <&> getGridLineVert)
        <> ([1 .. numGridLines] <&> getGridLineHor)
drawGrid _ = mempty


drawSidebar :: Int -> Int -> Int -> Picture
drawSidebar appWidth appHeight width =
  Translate
    ( (fromIntegral appWidth / 2.0)
        - (fromIntegral width / 2.0)
    )
    0
    ( color (greyN 0.1) $
        rectangleSolid
          (fromIntegral width)
          (fromIntegral appHeight)
    )


drawButton :: (Int, Int) -> Int -> Int -> Text -> (Int, Int) -> Picture
drawButton
  (appWidth, appHeight)
  sidebarWidth
  topOffset
  btnText
  (btnWidth, btnHeight) =
    Translate
      ( (fromIntegral appWidth / 2.0)
          - (fromIntegral btnWidth / 2.0)
          - ((fromIntegral sidebarWidth - fromIntegral btnWidth) / 2.0)
      )
      ( (fromIntegral appHeight / 2.0)
          - fromIntegral topOffset
          - (fromIntegral btnHeight / 2.0)
      )
      $ pictures
        [ color (greyN 0.2) $
            rectangleSolid
              (fromIntegral btnWidth)
              (fromIntegral btnHeight)
        , Translate 0 (-4) $ getWordSprite btnText
        ]


drawUiComponent :: AppState -> UiComponent -> Int -> Picture
drawUiComponent appState uiComponent componentIndex =
  case uiComponent of
    Button btnText btnWidth btnHeight _ ->
      drawButton
        (appState.appWidth, appState.appHeight)
        appState.sidebarWidth
        (sidebarPaddingTop + (componentIndex * sidebarGridHeight))
        btnText
        (btnWidth, btnHeight)
    Select -> mempty


-- | Render the app state to a picture.
makePicture :: AppState -> IO Picture
makePicture appState =
  case appState.currentView of
    HomeView -> do
      let
        fileSelectBtnWidth :: (Num a) => a
        fileSelectBtnWidth = 120

        fileSelectBtnHeight :: (Num a) => a
        fileSelectBtnHeight = 40

        uiElements =
          pictures
            [ color (greyN 0.2) $
                rectangleSolid fileSelectBtnWidth fileSelectBtnHeight
            , Translate 0 (-4) $ getWordSprite "Select Files"
            ]
      pure uiElements
    ImageView -> do
      let
        appWidthInteg = fromIntegral appState.appWidth
        sidebarWidthInteg = fromIntegral appState.sidebarWidth

      pure $
        Pictures $
          ( ( [ Scale
                  appState.scaleFactor
                  appState.scaleFactor
                  appState.image
              , appState.corners & drawEdges
              , appState.corners & drawGrid
              ]
                <> (appState.corners <&> drawCorner)
            )
              <&> Translate (-(sidebarWidthInteg / 2.0)) 0
          )
            <> [ drawSidebar
                  appWidthInteg
                  appState.appHeight
                  appState.sidebarWidth
               ]
            <> P.zipWith (drawUiComponent appState) appState.uiComponents [0 ..]
            <> [ if appState.bannerIsVisible
                  then Scale 0.5 0.5 bannerImage
                  else mempty
               , if appState.bannerIsVisible
                  then
                    Translate 300 (-250) $
                      Scale 0.2 0.2 $
                        ThickArc
                          0 -- Start angle
                          -- End angle
                          ( ( fromIntegral appState.tickCounter
                                / (bannerTime * fromIntegral ticksPerSecond)
                            )
                              * 360
                          )
                          50 -- Radius
                          100 -- Thickness
                          -- \$
                          --     -
                  else mempty
               ]
    BannerView -> pure $ Pictures []


replaceElemAtIndex :: Int -> a -> [a] -> [a]
replaceElemAtIndex theIndex newElem (x : xs) =
  if theIndex == 0
    then newElem : xs
    else x : replaceElemAtIndex (theIndex - 1) newElem xs
replaceElemAtIndex _ _ [] = []


calcDistance :: Point -> Point -> Float
calcDistance (x1, y1) (x2, y2) =
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
    fromMaybe 0 (DL.elemIndex minDistance distances)


addCorner :: AppState -> Corner -> AppState
addCorner appState newCorner =
  let
    theCorners = corners appState
    newCorners =
      if P.length theCorners < 4
        then newCorner : theCorners
        else
          replaceElemAtIndex
            (getIndexClosest theCorners newCorner)
            newCorner
            theCorners
  in
    appState{corners = newCorners}


-- TODO: Use correct algorithm as described in the readme
getTargetShape :: CornersTup -> (Float, Float)
getTargetShape (topLeft, topRight, btmRight, btmLeft) =
  let
    topEdgeLength = calcDistance topLeft topRight
    bottomEdgeLength = calcDistance btmLeft btmRight
    width = (topEdgeLength + bottomEdgeLength) / 2

    leftEdgeLength = calcDistance topLeft btmLeft
    rightEdgeLength = calcDistance topRight btmRight
    height = (leftEdgeLength + rightEdgeLength) / 2
  in
    (width, height)


toQuadTuple :: [a] -> Either Text (a, a, a, a)
toQuadTuple [tl, tr, br, bl] = Right (tl, tr, br, bl)
toQuadTuple _ = Left "The list must contain 4 values"


{-| Assuming coordinate system starts top left
 | 'getProjectionMap clickShape targetShape'
-}
getProjectionMap :: CornersTup -> (Float, Float) -> ProjMap
getProjectionMap (tl, tr, br, bl) (wdth, hgt) =
  -- Somehow the coordinate system is flipped on macOS with GLFW
  if os == "darwin"
    then
      ( (tl, (0, hgt))
      , (tr, (wdth, hgt))
      , (br, (wdth, 0))
      , (bl, (0, 0))
      )
    else
      ( (tl, (0, 0))
      , (tr, (wdth, 0))
      , (br, (wdth, hgt))
      , (bl, (0, hgt))
      )


-- | Accommodate ImageMagick's counter-clockwise direction
toCounterClock :: (a, a, a, a) -> (a, a, a, a)
toCounterClock (tl, tr, br, bl) = (tl, bl, br, tr)


-- | Transform from origin in top left to origin in center
originAtCenter :: Int -> Int -> [Point] -> [Point]
originAtCenter width height =
  fmap
    ( \(x, y) ->
        ( -((fromIntegral width / 2.0) - x)
        , (fromIntegral height / 2.0) - y
        )
    )


appCoordToImgCoord :: AppState -> Point -> Point
appCoordToImgCoord appState point =
  ( fst point + (fromIntegral appState.sidebarWidth / 2.0)
  , snd point
  )


checkSidebarRectHit
  :: (Int, Int) -> Int -> Int -> (Int, Int) -> (Float, Float) -> Bool
checkSidebarRectHit
  (appW, appH)
  sidebarW
  topOffset
  (rectW, rectH)
  (hitX, hitY) =
    let
      minX =
        (fromIntegral appW / 2.0)
          - fromIntegral rectW
          - ((fromIntegral sidebarW - fromIntegral rectW) / 2.0)
      maxX = minX + fromIntegral rectW

      minY =
        (fromIntegral appH / 2.0)
          - fromIntegral topOffset
          - fromIntegral rectH
      maxY = minY + fromIntegral rectH
    in
      hitX > minX
        && hitX < maxX
        && hitY > minY
        && hitY < maxY


submitSelection :: AppState -> ExportMode -> IO AppState
submitSelection appState exportMode = do
  let
    cornersTrans = getCorners appState
    cornerTuple =
      fromRight
        ((0, 0), (0, 0), (0, 0), (0, 0))
        (toQuadTuple cornersTrans)
    targetShape = getTargetShape cornerTuple
    projectionMapNorm =
      toCounterClock $
        getProjectionMap cornerTuple targetShape

  putText $ "Target shape: " <> show targetShape
  putText $ "Marked corners: " <> show cornerTuple

  case (appState.inputPath, appState.outputPath) of
    (Just inputPath, Just outputPath) -> do
      let
        convertArgs =
          getConvertArgs
            inputPath
            outputPath
            projectionMapNorm
            targetShape
            exportMode

      if appState.transformApp == ImageMagick
        then
          putText $
            "Arguments for convert command:\n"
              <> T.unlines convertArgs
        else putText $ "Write file to " <> show appState.outputPath

      correctAndWrite
        appState.transformApp
        inputPath
        outputPath
        projectionMapNorm
        exportMode
        convertArgs

      exitSuccess
    (_, _) -> do
      P.die "Input path and output path must be set before submitting"


handleImageViewEvent :: Event -> AppState -> IO AppState
handleImageViewEvent event appState =
  case event of
    EventKey (MouseButton LeftButton) Gl.Down _ clickedPoint -> do
      -- Check if a UiComponent was clicked
      let
        clickedComponent =
          P.find
            ( \(component, componentIndex) -> case component of
                Button _ width height _ ->
                  checkSidebarRectHit
                    (appState.appWidth, appState.appHeight)
                    appState.sidebarWidth
                    (sidebarPaddingTop + (componentIndex * sidebarGridHeight))
                    (width, height)
                    clickedPoint
                _ -> False
            )
            (P.zip appState.uiComponents [0 ..])
            <&> fst

      case clickedComponent of
        Just (Button{text = "Save"}) ->
          submitSelection appState UnmodifiedExport
        Just (Button{text = "Save Gray"}) ->
          submitSelection appState GrayscaleExport
        Just (Button{text = "Save BW"}) ->
          submitSelection appState BlackWhiteExport
        _ -> do
          let
            point = appCoordToImgCoord appState clickedPoint
            clickedCorner =
              P.find
                ( \corner ->
                    calcDistance point corner
                      < (cornCircRadius + cornCircThickness)
                )
                appState.corners

          pure $ case clickedCorner of
            Nothing ->
              appState
                & flip addCorner point
                & (\state_ -> state_{cornerDragged = Just point})
            Just cornerPoint ->
              appState{cornerDragged = Just cornerPoint}
    EventKey (MouseButton LeftButton) Gl.Up _ _ -> do
      pure $ appState{cornerDragged = Nothing}
    EventMotion newPoint -> do
      let
        point = appCoordToImgCoord appState newPoint

      pure $ case appState.cornerDragged of
        Nothing -> appState
        Just cornerPoint ->
          let
            cornerIndexMb =
              elemIndex
                cornerPoint
                appState.corners
          in
            case cornerIndexMb of
              Nothing -> appState
              Just cornerIndex ->
                appState
                  { corners =
                      replaceElemAtIndex
                        cornerIndex
                        point
                        appState.corners
                  , cornerDragged = Just point
                  }
    EventKey (SpecialKey KeyEnter) Gl.Down _ _ ->
      submitSelection appState UnmodifiedExport
    EventKey (SpecialKey KeyEsc) Gl.Down _ _ -> do
      pure $ appState{corners = []}
    EventResize (windowWidth, windowHeight) -> do
      pure $
        calculateSizes $
          appState
            { appWidth = windowWidth
            , appHeight = windowHeight
            }
    _ ->
      pure appState


handleEvent :: Event -> AppState -> IO AppState
handleEvent event appState =
  case appState.currentView of
    HomeView -> handleHomeEvent event appState
    ImageView -> handleImageViewEvent event appState
    BannerView -> pure appState


-- FIXME: Don't rely on show implementation
showProjectionMap :: ProjMap -> Text
showProjectionMap pMap =
  pMap
    & show
    & T.replace "),(" " "
    & T.replace "(" ""
    & T.replace ")" ""


fixOutputPath :: ExportMode -> FilePath -> Text
fixOutputPath exportMode outPath =
  case exportMode of
    BlackWhiteExport -> T.pack $ replaceExtension outPath "png"
    _ -> T.pack outPath


getConvertArgs
  :: FilePath -> FilePath -> ProjMap -> (Float, Float) -> ExportMode -> [Text]
getConvertArgs inPath outPath projMap shape exportMode =
  [ T.pack inPath
  , "-auto-orient"
  , "-define"
  , "distort:viewport="
      <> show (fst shape)
      <> "x"
      <> show (snd shape)
      <> "+0+0"
  , -- TODO: Add flag to support this
    -- Use interpolated lookup instead of area resampling
    -- https://www.imagemagick.org/Usage/distorts/#area_vs_super
    -- , "-filter", "point"

    -- Prevent interpolation of unused pixels and avoid adding alpha channel
    "-virtual-pixel"
  , "black"
  , -- TODO: Add flag to support switching
    -- , "-virtual-pixel", "Edge" -- default
    -- , "-virtual-pixel", "Dither"
    -- , "-virtual-pixel", "Random"
    -- TODO: Implement more sophisticated one upstream in Imagemagick

    "-distort"
  , "Perspective"
  , showProjectionMap projMap
  ]
    <> case exportMode of
      UnmodifiedExport -> []
      GrayscaleExport -> ["-colorspace", "gray", "-normalize"]
      BlackWhiteExport -> ["-auto-threshold", "OTSU", "-monochrome"]
    <> [ "+repage"
       , fixOutputPath exportMode outPath
       ]


correctAndWrite
  :: TransformApp
  -> FilePath
  -> FilePath
  -> ProjMap
  -> ExportMode
  -> [Text]
  -> IO ()
correctAndWrite transformApp inPath outPath ((bl, _), (tl, _), (tr, _), (br, _)) exportMode args = do
  currentDir <- getCurrentDirectory

  case transformApp of
    ImageMagick -> do
      let
        conversionMode = CallConversion
        magickBin = case os of
          "darwin" -> currentDir ++ "/imagemagick/bin/magick"
          "mingw32" -> "TODO_implement"
          _ -> "TODO_bundle_imagemagick"

      when (os == "darwin") $ do
        setEnv "MAGICK_HOME" (currentDir ++ "/imagemagick")
        setEnv "DYLD_LIBRARY_PATH" (currentDir ++ "/imagemagick/lib")

      let
        argsNorm = ("magick" : args) <&> T.unpack
        successMessage =
          "✅ Successfully saved converted image at "
            <> fixOutputPath exportMode outPath

      -- TODO: Add CLI flag to switch between them
      case conversionMode of
        CallConversion -> do
          resultBundled <- try $ callProcess magickBin argsNorm

          case resultBundled of
            Right _ -> putText successMessage
            Left (_ :: IOException) -> do
              putText "Uses global installation of ImageMagick"
              resultMagick <- try $ callProcess "magick" (args <&> T.unpack)

              case resultMagick of
                Right _ -> putText successMessage
                Left (error :: IOException) -> do
                  P.putErrLn $ P.displayException error
                  putText $
                    "⚠️  Please install ImageMagick first: "
                      <> "https://imagemagick.org/script/download.php"
        SpawnConversion -> do
          _ <- spawnProcess magickBin (fmap T.unpack args)
          putText "✅ Successfully initiated conversion"
    Hip -> do
      uncorrected <- readImageRGBA inPath

      let
        cornersClockwiseFromTopLeft :: V4 (V2 Double)
        cornersClockwiseFromTopLeft =
          let
            toV2 :: (Float, Float) -> V2 Double
            toV2 (x, y) = realToFrac <$> V2 x y
          in
            V4 (toV2 tl) (toV2 tr) (toV2 br) (toV2 bl)

        correctionTransform :: M33 Double
        correctionTransform =
          calculatePerspectiveTransform
            ( fmap fromIntegral
                <$> V4
                  (V2 0 0)
                  (V2 width 0)
                  (V2 width height)
                  (V2 0 height)
            )
            cornersClockwiseFromTopLeft

        size :: Sz Ix2
        size@(Sz (height :. width)) =
          determineSize cornersClockwiseFromTopLeft

        corrected :: Image (Alpha (SRGB 'Linear)) Double
        corrected =
          transform
            (size,)
            ( \(Sz (sourceHeight :. sourceWidth)) getPixel (Ix2 irow icol) ->
                let
                  V3 colCrd rowCrd p =
                    correctionTransform !* V3 (fromIntegral icol) (fromIntegral irow) 1

                  colCrd' = max 0 (min (colCrd / p) (fromIntegral $ sourceWidth - 1))

                  rowCrd' = max 0 (min (rowCrd / p) (fromIntegral $ sourceHeight - 1))
                in
                  interpolate Bilinear getPixel (rowCrd', colCrd')
            )
            uncorrected

      writeImage outPath corrected

  pure ()


loadAndStart :: Config -> Maybe FilePath -> IO ()
loadAndStart config filePathMb = do
  let
    isRegistered = True -- (config&licenseKey) `elem` licenses
    stateDraft =
      initialState
        { transformApp = config.transformAppFlag
        , isRegistered = isRegistered
        , bannerIsVisible = not isRegistered
        }

  case filePathMb of
    Nothing -> startApp stateDraft
    Just filePath -> do
      stateWithFile <- loadFileIntoState stateDraft filePath
      startApp stateWithFile


helpMessage :: Text
helpMessage =
  T.unlines ["Usage: perspec <image> [image…]"]
