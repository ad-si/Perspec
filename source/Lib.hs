module Lib where

import Protolude as P

import Codec.BMP
import Codec.Picture (decodePng)
import Codec.Picture.Metadata (Keys(Exif), Metadatas, lookup)
import Codec.Picture.Metadata.Exif (ExifTag(..), ExifData(ExifShort))
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import Data.List as DL (minimum, elemIndex, findIndex)
import Data.Text as T
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game as Gl
import Graphics.Gloss.Juicy
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.Info (os)

import Types


-- | This is replaced with valid licenses during CI build
licenses :: [Text]
licenses = []

-- | Radius of the circles to mark the corners of the selection
cornCircRadius :: Float
cornCircRadius = 6

-- | Border thickness of the circles to mark the corners of the selection
cornCircThickness :: Float
cornCircThickness = 4

sidebarPaddingTop :: Int
sidebarPaddingTop = 50

sidebarGridHeight :: Int
sidebarGridHeight = 40

ticksPerSecond :: Int
ticksPerSecond = 10

bannerTime :: Float
bannerTime = 10  -- seconds

bannerImage :: Picture
bannerImage = fromRight mempty $ bitmapOfBMP
                <$> (parseBMP $ BL.fromStrict $(embedFile "images/banner.bmp"))


loadImage :: FilePath -> IO (Either Text (Picture, Metadatas))
loadImage filePath = do
  picMetaMaybe <- loadJuicyWithMetadata filePath

  let
    allowedExtensions =
      [ ".jpeg"
      , ".jpg"
      , ".png"
      , ".bmp"
      , ".gif"
      , ".hdr"
      ]
    fileExtension = takeExtension filePath

  case picMetaMaybe of
    Nothing -> do
      if elem fileExtension allowedExtensions
      then pure $ Left "Error: Image couldn't be loaded"
      else pure $ Left $ "Error: File extension \""
                          <> T.pack fileExtension
                          <> "\" is not supported"

    Just (picture, metadata) ->
      pure $ Right (picture, metadata)


calculateSizes :: AppState -> AppState
calculateSizes appState =
  let
    imgViewWidth = (appState&appWidth) - (appState&sidebarWidth)
    imgViewHeight = appState&appHeight

    imgWidthFrac = fromIntegral $ appState&imgWidthOrig
    imgHeightFrac = fromIntegral $ appState&imgHeightOrig

    scaleFactorX = (fromIntegral imgViewWidth) / imgWidthFrac
    scaleFactorY = (fromIntegral imgViewHeight) / imgHeightFrac

    scaleFactor = min scaleFactorX scaleFactorY
    imgWidthTrgt = round $ scaleFactor * imgWidthFrac
    imgHeightTrgt = round $ scaleFactor * imgHeightFrac
  in
    appState
      { imgWidthTrgt
      , imgHeightTrgt
      , scaleFactor
      , corners = originTopLeft (-imgWidthTrgt) (imgHeightTrgt) $
          scalePoints (1 / scaleFactor) (getCorners appState)
      }


startApp
  :: Config
  -> FilePath
  -> FilePath
  -> Int
  -> Int
  -> Float
  -> Picture
  -> IO ()
startApp config inPath outPath imgWdth imgHgt rota pic = do
  (screenWidth, screenHeight) <- getScreenSize

  let
    distance = 0.1
    isRegistered = (config&licenseKey) `elem` licenses
    stateWithSizes = calculateSizes $ initialState
      { imgWidthOrig = imgWdth
      , imgHeightOrig = imgHgt
      , rotation = rota
      , image = pic
      , inputPath = inPath
      , outputPath = outPath
      , isRegistered = isRegistered
      , bannerIsVisible = not $ isRegistered
      }

  let
    wdthFrac = fromIntegral $ stateWithSizes&imgWidthOrig
    hgtFrac = fromIntegral $ stateWithSizes&imgHeightOrig

    stateWithImage = stateWithSizes
      { corners = originTopLeft
          (-(stateWithSizes&imgWidthTrgt))
          (stateWithSizes&imgHeightTrgt) $
            scalePoints (1 / (stateWithSizes&scaleFactor)) $ P.reverse
              [ (wdthFrac * distance, hgtFrac * distance)
              , (wdthFrac * (1 - distance), hgtFrac * distance)
              , (wdthFrac * (1 - distance), hgtFrac * (1 - distance))
              , (wdthFrac * distance, hgtFrac * (1 - distance))
              ]
      }

    initialX =
      ((fromIntegral screenWidth :: Float) / 2)
      - ((fromIntegral $ stateWithSizes&appWidth) / 2)
    initialY =
      ((fromIntegral screenHeight :: Float) / 2)
      - ((fromIntegral $ stateWithSizes&appHeight) / 2)

    window = InWindow
      ("Perspec - " <> inPath
        <> if isRegistered
            then mempty
            else " - ⚠️ NOT REGISTERED")
      (stateWithImage&appWidth, stateWithImage&appHeight)
      (round initialX, round initialY)

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
stepWorld _ appState =
  if
    (not $ appState&isRegistered)
    && ((fromIntegral $ appState&tickCounter)
            < (bannerTime * fromIntegral ticksPerSecond))
  then pure appState { tickCounter = (appState&tickCounter) + 1 }
  else pure appState { bannerIsVisible = False }


wordsSprite :: ByteString
wordsSprite = $(embedFile "images/words.png")


wordsPic :: Picture
wordsPic = fromMaybe mempty
  ((either (const Nothing) Just $ decodePng wordsSprite)
    >>= fromDynamicImage)


getWordSprite :: Text -> Picture
getWordSprite spriteText =
  case wordsPic of
    Bitmap bitmapData -> case spriteText of
      "Save" -> BitmapSection
        Rectangle { rectPos = (0, 40), rectSize = (90, 20) }
        bitmapData

      "Save BW" -> BitmapSection
        Rectangle { rectPos = (0, 60), rectSize = (90, 20) }
        bitmapData

      "Save Gray" -> BitmapSection
        Rectangle { rectPos = (0, 80), rectSize = (90, 20) }
        bitmapData

      _ -> mempty
    _ -> mempty


drawCorner :: Point -> Picture
drawCorner (x, y) = Translate x y
  (color green $ ThickCircle cornCircRadius cornCircThickness)


drawEdges :: [Point] -> Picture
drawEdges points =
  color (makeColor 0.2 1 0.5 0.4) $ Polygon points


drawSidebar :: Int -> Int -> Int -> Picture
drawSidebar appWidth appHeight width =
  Translate
    (((fromIntegral appWidth) / 2.0)
      - ((fromIntegral $ width) / 2.0)
    )
    0
    (color (greyN 0.1) $ rectangleSolid
      (fromIntegral $ width)
      (fromIntegral $ appHeight)
    )


drawButton :: (Int, Int) -> Int -> Int -> Text -> (Int, Int) -> Picture
drawButton
  (appWidth, appHeight) sidebarWidth topOffset btnText (btnWidth, btnHeight) =
  Translate
    ((fromIntegral appWidth / 2.0)
      - ((fromIntegral btnWidth) / 2.0)
      - ((fromIntegral sidebarWidth - fromIntegral btnWidth) / 2.0)
    )
    ((fromIntegral appHeight / 2.0)
      - fromIntegral topOffset
      - (fromIntegral btnHeight / 2.0)
    )
    $ pictures
        [ color (greyN 0.2) $ rectangleSolid
            (fromIntegral btnWidth)
            (fromIntegral btnHeight)
        , Translate 0 (-4) $ getWordSprite btnText
        ]


drawUiComponent :: AppState -> UiComponent -> Int -> Picture
drawUiComponent appState uiComponent componentIndex =
  case uiComponent of
    Button btnText btnWidth btnHeight _ ->
      drawButton
        (appState&appWidth, appState&appHeight)
        (appState&sidebarWidth)
        (sidebarPaddingTop + (componentIndex * sidebarGridHeight))
        btnText
        (btnWidth, btnHeight)
    Select -> mempty


-- | Render the app state to a picture.
makePicture :: AppState -> IO Picture
makePicture appState =
  let
    appWidthInteg = fromIntegral $ appState&appWidth
    sidebarWidthInteg = fromIntegral $ appState&sidebarWidth
  in
    pure $ Pictures
      $ ( (
            [ Scale
                (appState&scaleFactor)
                (appState&scaleFactor)
                (appState&image)
            , (appState&corners) & drawEdges
            ]
            <> ((appState&corners) <&> drawCorner)
          )
            <&> Translate (-sidebarWidthInteg / 2.0) 0
        )
      <> [ drawSidebar
            appWidthInteg
            (appState&appHeight)
            (appState&sidebarWidth)
        ]
      <> (P.zipWith (drawUiComponent appState) (appState&uiComponents) [0..])
      <>  [ if appState&bannerIsVisible
              then Scale 0.5 0.5 bannerImage
              else mempty
          , if appState&bannerIsVisible
              then Translate 300 (-250)
                    $ Scale 0.2 0.2
                    $ ThickArc
                        0  -- Start angle
                        -- End angle
                        (((fromIntegral $ appState&tickCounter)
                          / (bannerTime * fromIntegral ticksPerSecond)) * 360)
                        50  -- Radius
                        100  -- Thickness
                      -- $
                      --     -
              else mempty
          ]


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
    fromMaybe 0 (DL.elemIndex minDistance distances)


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
    width            = (topEdgeLength + bottomEdgeLength) / 2

    leftEdgeLength   = calcDistance topLeft btmLeft
    rightEdgeLength  = calcDistance topRight btmRight
    height           = (leftEdgeLength + rightEdgeLength) / 2
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


-- | Accommodate ImageMagick's counter-clockwise direction
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
    (appState&imgWidthTrgt)
    (appState&imgHeightTrgt)
    (P.reverse $ corners appState)


appCoordToImgCoord :: AppState -> Point -> Point
appCoordToImgCoord appState point =
  ( fst point + ((fromIntegral $ appState&sidebarWidth) / 2.0)
  , snd point
  )


checkSidebarRectHit
  :: (Int, Int) -> Int -> Int -> (Int, Int) -> (Float, Float) -> Bool
checkSidebarRectHit
  (appW, appH) sidebarW topOffset (rectW, rectH) (hitX, hitY) =
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
         hitX > minX && hitX < maxX
      && hitY > minY && hitY < maxY


submitSelection :: AppState -> ExportMode -> IO AppState
submitSelection appState exportMode = do
  let
    cornersTrans = getCorners appState
    cornerTuple = fromRight
      ((0,0), (0,0), (0,0), (0,0))
      (toQuadTuple cornersTrans)
    targetShape = getTargetShape cornerTuple
    projectionMapNorm = toCounterClock $
      getProjectionMap cornerTuple targetShape

  putText $ "Target shape: " <> (show targetShape)
  putText $ "Marked corners: " <> (show cornerTuple)

  let
    convertArgs = getConvertArgs
      (appState&inputPath)
      (appState&outputPath)
      projectionMapNorm
      targetShape
      exportMode


  putText $ "Arguments for magick command:\n" <> (T.unlines convertArgs)

  correctAndWrite convertArgs

  exitSuccess


handleEvent :: Event -> AppState -> IO AppState
handleEvent event appState =
  case event of
    EventKey (MouseButton LeftButton) Gl.Down _ clickedPoint -> do
      -- Check if a UiComponent was clicked
      let clickedComponent =
            (P.find
              (\(component, componentIndex) -> case component of
                  Button _ width height _ -> checkSidebarRectHit
                    (appState&appWidth, appState&appHeight)
                    (appState&sidebarWidth)
                    (sidebarPaddingTop + (componentIndex * sidebarGridHeight))
                    (width, height)
                    clickedPoint
                  _ -> False
              )
              (P.zip (appState&uiComponents) [0..])
            )
            <&> fst

      case clickedComponent of
        Just (Button {text = "Save"}) ->
          submitSelection appState UnmodifiedExport

        Just (Button {text = "Save Gray"}) ->
          submitSelection appState GrayscaleExport

        Just (Button {text = "Save BW"}) ->
          submitSelection appState BlackWhiteExport

        _ -> do
          let
            point = appCoordToImgCoord appState clickedPoint
            clickedCorner = P.find
              (\corner ->
                (calcDistance point corner)
                < (cornCircRadius + cornCircThickness)
              )
              (appState&corners)

          pure $ case clickedCorner of
            Nothing -> appState
              & flip addCorner point
              & (\state_ -> state_ { cornerDragged = Just point })

            Just cornerPoint ->
              appState { cornerDragged = Just cornerPoint }

    EventKey (MouseButton LeftButton) Gl.Up _ _ -> do
      pure $ appState { cornerDragged = Nothing }

    EventMotion newPoint -> do
      let point = appCoordToImgCoord appState newPoint

      pure $ case appState&cornerDragged of
        Nothing -> appState
        Just cornerPoint ->
          let cornerIndexMb = DL.findIndex
                (\pnt -> pnt == cornerPoint)
                (appState&corners)
          in
            case cornerIndexMb of
              Nothing -> appState
              Just cornerIndex ->
                appState
                  { corners = replaceElemAtIndex
                      cornerIndex
                      point
                      (appState&corners)
                  , cornerDragged = Just point
                  }

    EventKey (SpecialKey KeyEnter) Gl.Down _ _ ->
      submitSelection appState UnmodifiedExport

    EventKey (SpecialKey KeyEsc) Gl.Down _ _ -> do
      pure $ appState { corners = [] }

    EventResize (windowWidth, windowHeight) -> do
      pure $ calculateSizes $ appState
        { appWidth = windowWidth
        , appHeight = windowHeight
        }

    _ ->
      pure $ appState


-- FIXME: Don't rely on show implementation
showProjectionMap :: ProjMap -> Text
showProjectionMap pMap = pMap
  & show
  & T.replace "),(" " "
  & T.replace "(" ""
  & T.replace ")" ""


getConvertArgs
  :: FilePath -> FilePath -> ProjMap -> (Float, Float) -> ExportMode -> [Text]
getConvertArgs inPath outPath projMap shape exportMode =
  [ "convert"
  , (T.pack inPath)
  , "-auto-orient"
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
  ]
  <> case exportMode of
      UnmodifiedExport -> []
      GrayscaleExport -> [ "-colorspace", "gray", "-normalize" ]
      BlackWhiteExport -> [ "-auto-threshold", "OTSU", "-monochrome" ]
  <>
  [ "+repage"
  , case exportMode of
      BlackWhiteExport -> T.pack $ replaceExtension outPath "png"
      _  -> T.pack outPath
  ]


correctAndWrite :: [Text] -> IO ()
correctAndWrite args = do
  let
    conversionMode = CallConversion
    magickBin = case os of
      "darwin"  -> "./imagemagick/bin/magick"
      "mingw32" -> "TODO"
      _         -> "./magick"

  currentDir <- getCurrentDirectory

  when (os == "darwin") $ do
    setEnv "MAGICK_HOME" (currentDir ++ "/imagemagick")
    setEnv "DYLD_LIBRARY_PATH" (currentDir ++ "/imagemagick/lib")

  -- TODO: Add CLI flag to switch between them
  case conversionMode of
    CallConversion -> do
      callProcess magickBin (fmap T.unpack args)
      putText $ "✅ Successfully saved converted image"

    SpawnConversion -> do
      _ <- spawnProcess magickBin (fmap T.unpack args)
      putText $ "✅ Successfully initiated conversion"

  pure ()


imgOrientToRot :: ExifData -> Float
imgOrientToRot = \case
  ExifShort 6 -> -90
  ExifShort 1 ->   0
  ExifShort 8 ->  90
  ExifShort 3 -> 180

  -- TODO: Also apply mirroring to image
  ExifShort 5 -> -90
  ExifShort 2 ->   0
  ExifShort 7 ->  90
  ExifShort 4 -> 180

  _ -> 0


loadAndStart :: Config -> FilePath -> IO ()
loadAndStart config filePath = do
  let outName = (takeBaseName filePath) <> "-fixed"

  pictureMetadataEither <- loadImage filePath

  case pictureMetadataEither of
    Left error -> putText error

    Right (picture@(Bitmap bitmapData), metadata) -> do
      let
        rotation = lookup (Exif TagOrientation) metadata
          <&> imgOrientToRot
          & fromMaybe 0
        sizeTuple = bitmapSize bitmapData
        (imgWdth, imgHgt) = case rotation of
                              90  -> swap $ sizeTuple
                              -90 -> swap $ sizeTuple
                              _   -> sizeTuple

      putText $
        "Loaded file " <> (pack filePath) <> " " <> (show (imgWdth,imgHgt))
      putText $
        "with a rotation of " <> (show rotation) <> " degrees."

      startApp
        config
        filePath
        (replaceBaseName filePath outName)
        imgWdth
        imgHgt
        rotation
        (Rotate (-rotation) picture)

    Right _ -> putText $ "Error: Loaded file is not a Bitmap image. "
                      <> "This error should not be possible."


helpMessage :: Text
helpMessage =
  T.unlines [ "Usage: perspec <image> [image…]" ]
