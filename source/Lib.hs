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
  Floating (cos, pi, sin, sqrt),
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
  elem,
  exitSuccess,
  flip,
  fromIntegral,
  fromMaybe,
  fromRight,
  fst,
  putText,
  realToFrac,
  show,
  snd,
  toS,
  try,
  void,
  when,
  ($),
  (&),
  (&&),
  (++),
  (<$>),
  (<&>),
 )

import Control.Concurrent (forkIO, threadDelay)
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List as DL (elemIndex, minimum)
import Data.Text qualified as T
import Foreign (castForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (new)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import GHC.Float (float2Double, int2Double)
import Protolude qualified as P
import System.Directory (getCurrentDirectory)
import System.Environment (getEnv, setEnv)
import System.FilePath (replaceExtension)
import System.Info (os)
import System.Process (callProcess, spawnProcess)

import Brillo (
  Display (InWindow),
  Picture (
    Bitmap,
    Pictures,
    Rotate,
    Scale,
    ThickArc,
    ThickCircle,
    ThickLineSmooth,
    Translate
  ),
  Point,
  bitmapOfBMP,
  black,
  color,
  greyN,
  lineLoop,
  makeColor,
  pictures,
  polygon,
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
 )
import Brillo.Interface.IO.Interact (Controller (..), interactIO)
import Brillo.Rendering (BitmapData (..))
import Codec.BMP (parseBMP)
import Codec.Picture (
  DynamicImage (ImageRGBA8),
  imageFromUnsafePtr,
  savePngImage,
 )
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
import Linear (M33, V2 (V2), V3 (V3), V4 (V4), (!*))

import Correct (calculatePerspectiveTransform, determineOutputSize)
import FlatCV (
  Corners (..),
  prettyShowCorners,
  prettyShowMatrix3x3,
 )
import FlatCV qualified as FCV
import Home (handleHomeEvent)
import Types (
  AppState (..),
  Config (licenseKey, transformBackendFlag),
  ConversionMode (CallConversion, SpawnConversion),
  Corner,
  CornersTup,
  EdgeIndex (..),
  ExportMode (..),
  ImageData (..),
  ProjMap,
  TransformBackend (..),
  UiComponent (Button, Select, text),
  View (..),
  initialState,
 )
import Utils (
  applyRotationToCorners,
  calcInitWindowPos,
  calculateSizes,
  getCorners,
  getTextPicture,
  loadFileIntoState,
  loadImage,
  prettyPrintArray,
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


-- | Draw a solid rounded rectangle centered at the origin
roundedRectSolid :: Float -> Float -> Float -> Picture
roundedRectSolid width height radius =
  let
    r = min radius (min (width / 2) (height / 2))
    hw = width / 2
    hh = height / 2
    -- Number of segments per corner arc
    segments :: Int
    segments = 8
    -- Generate points for a corner arc
    arcPoints :: Float -> Float -> Float -> Float -> [Point]
    arcPoints cx cy startAngle endAngle =
      [ ( cx
            + r
              * cos
                (startAngle + fromIntegral i * (endAngle - startAngle) / fromIntegral segments)
        , cy
            + r
              * sin
                (startAngle + fromIntegral i * (endAngle - startAngle) / fromIntegral segments)
        )
      | i <- [0 .. segments] :: [Int]
      ]
    -- Four corners: top-right, top-left, bottom-left, bottom-right
    topRight = arcPoints (hw - r) (hh - r) 0 (pi / 2)
    topLeft = arcPoints (-(hw - r)) (hh - r) (pi / 2) pi
    bottomLeft = arcPoints (-(hw - r)) (-(hh - r)) pi (3 * pi / 2)
    bottomRight = arcPoints (hw - r) (-(hh - r)) (3 * pi / 2) (2 * pi)
  in
    polygon $ topRight <> topLeft <> bottomLeft <> bottomRight


sidebarPaddingTop :: Int
sidebarPaddingTop = 50


sidebarGridHeight :: Int
sidebarGridHeight = 40


buttonCornerRadius :: Float
buttonCornerRadius = 5


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


appStateToWindow :: (Int, Int) -> AppState -> Display
appStateToWindow screenSize appState = do
  let
    appSize = (appState.appWidth, appState.appHeight)
    windowPos = calcInitWindowPos screenSize appSize

  case appState.images of
    [] -> InWindow "Perspec" appSize (0, 0)
    image : _otherImages -> do
      case appState.currentView of
        HomeView -> InWindow "Perspec - Select a file" appSize windowPos
        ImageView -> do
          InWindow
            ( "Perspec - "
                <> case image of
                  ImageToLoad{filePath} -> toS filePath
                  ImageData{inputPath} -> toS inputPath
                <> if appState.isRegistered
                  then mempty
                  else " - ⚠️ NOT REGISTERED"
            )
            appSize
            windowPos
        BannerView ->
          InWindow "Perspec - Banner" (800, 600) (10, 10)


drawCorner :: Gl.Color -> Point -> Picture
drawCorner cornerColor (x, y) =
  Translate
    x
    y
    (color cornerColor $ ThickCircle cornCircRadius cornCircThickness)


drawCornersWithColors :: [Point] -> [Picture]
drawCornersWithColors corners = do
  let
    redColor = makeColor 1 0.2 0.2 0.8
    greenColor = makeColor 0.2 1 0.2 0.8
    blueColor = makeColor 0.2 0.2 1 0.8
    magentaColor = makeColor 1 0.2 1 0.8
    -- Order: top-left, top-right, bottom-right, bottom-left
    cornerColors = [redColor, greenColor, blueColor, magentaColor]

  -- Reverse corners list,
  -- because AppState.corners stores them in reverse order of addition
  P.zipWith drawCorner cornerColors (P.reverse corners)


gridLineThickness :: Float
gridLineThickness = 2


-- | Width of edge handle pill shapes
edgeHandleWidth :: Float
edgeHandleWidth = 30


-- | Height of edge handle pill shapes
edgeHandleHeight :: Float
edgeHandleHeight = 10


-- | Number of segments for each semicircle end of the pill shape
pillSegments :: Int
pillSegments = 16


{-| Draw a handle at the midpoint of an edge
The pill shape is drawn as a filled polygon to avoid line self-intersection
-}
drawEdgeHandle :: Gl.Color -> Float -> Point -> Picture
drawEdgeHandle handleColor rotationAngle (x, y) =
  let
    hw = edgeHandleWidth / 2
    hh = edgeHandleHeight / 2
    radius = hh -- The radius of the semicircles equals half the height
    straightLength = hw - radius -- Length of straight sections
    -- Generate arc points including endpoints
    arcPoints :: Float -> Float -> Float -> Float -> Int -> [Point]
    arcPoints cx cy startAngle endAngle numSegments =
      let
        step = (endAngle - startAngle) / fromIntegral numSegments
        angles = [startAngle + step * fromIntegral i | i <- [0 .. numSegments]]
      in
        [(cx + radius * cos a, cy + radius * sin a) | a <- angles]
    -- Right arc: from top (pi/2) to bottom (-pi/2), curving right
    rightArc = arcPoints straightLength 0 (pi / 2) (-(pi / 2)) pillSegments
    -- Left arc: from bottom (-pi/2) to top (pi/2), curving left (going through pi)
    leftArc = arcPoints (-straightLength) 0 (-(pi / 2)) (-(3 * pi / 2)) pillSegments
    -- Combine: top edge (implicit), right arc, bottom edge (implicit), left arc
    pillShape = rightArc <> leftArc
  in
    Translate x y $
      Rotate rotationAngle $
        color handleColor $
          polygon pillShape


-- | Get the midpoint of an edge given two corners
getEdgeMidpoint :: Point -> Point -> Point
getEdgeMidpoint (x1, y1) (x2, y2) =
  ((x1 + x2) / 2, (y1 + y2) / 2)


-- | Calculate angle of edge in degrees (for Brillo's Rotate which uses degrees)
getEdgeAngle :: Point -> Point -> Float
getEdgeAngle (x1, y1) (x2, y2) =
  let
    dx = x2 - x1
    dy = y2 - y1
    -- atan2 returns radians, convert to degrees
    -- Brillo's Rotate is clockwise, atan2 is counter-clockwise
    radians = P.atan2 dy dx
  in
    -(radians * 180 / pi)


{-| Get edge midpoints from corners in display order
Returns [(edgeIndex, midpoint, angle)] for edges: top(0), right(1), bottom(2), left(3)
-}
getEdgeMidpoints :: [Point] -> [(EdgeIndex, Point, Float)]
getEdgeMidpoints corners =
  case P.reverse corners of
    -- Corners are stored in reverse order, so reverse to get display order
    -- Display order: top-left(0), top-right(1), bottom-right(2), bottom-left(3)
    [tl, tr, br, bl] ->
      [ (EdgeIndex 0, getEdgeMidpoint tl tr, getEdgeAngle tl tr) -- top edge
      , (EdgeIndex 1, getEdgeMidpoint tr br, getEdgeAngle tr br) -- right edge
      , (EdgeIndex 2, getEdgeMidpoint br bl, getEdgeAngle br bl) -- bottom edge
      , (EdgeIndex 3, getEdgeMidpoint bl tl, getEdgeAngle bl tl) -- left edge
      ]
    _ -> []


-- | Draw edge handles for all 4 edges
drawEdgeHandles :: [Point] -> [Picture]
drawEdgeHandles corners
  | P.length corners < 4 = []
  | P.otherwise =
      let
        midpoints = getEdgeMidpoints corners
      in
        fmap (\(_, midpt, angle) -> drawEdgeHandle gridColor angle midpt) midpoints


-- | Find which edge handle was clicked, if any
findClickedEdge :: Point -> [Point] -> Maybe EdgeIndex
findClickedEdge clickPoint corners
  | P.length corners < 4 = Nothing
  | P.otherwise =
      let
        midpoints = getEdgeMidpoints corners
        -- Use the larger dimension for hit detection
        hitRadius = max edgeHandleWidth edgeHandleHeight / 2
        clicked =
          P.find
            (\(_, midpt, _) -> calcDistance clickPoint midpt < hitRadius)
            midpoints
      in
        fmap (\(idx, _, _) -> idx) clicked


-- | Normalize a vector to unit length
normalize :: Point -> Point
normalize (x, y) =
  let
    len = sqrt (x * x + y * y)
  in
    if len < 0.0001 then (0, 0) else (x / len, y / len)


-- | Dot product of two vectors
dotProduct :: Point -> Point -> Float
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2


{-| Move an edge by moving its two corners by the same percentage toward/away
from their opposite corners. This keeps the quadrilateral's proportions.
-}
moveEdgePreservingAngles :: EdgeIndex -> Point -> Point -> [Point] -> [Point]
moveEdgePreservingAngles edgeIdx oldMousePos newMousePos corners =
  case P.reverse corners of
    -- Corners in display order: tl(0), tr(1), br(2), bl(3)
    [tl, tr, br, bl] ->
      let
        -- Get the two corners of the edge being moved and their opposite corners
        -- Edge 0 (top): tl->bl, tr->br
        -- Edge 1 (right): tr->tl, br->bl
        -- Edge 2 (bottom): br->tr, bl->tl
        -- Edge 3 (left): bl->br, tl->tr
        (c1, opp1, c2, opp2) = case edgeIdx of
          EdgeIndex 0 -> (tl, bl, tr, br)
          EdgeIndex 1 -> (tr, tl, br, bl)
          EdgeIndex 2 -> (br, tr, bl, tl)
          EdgeIndex 3 -> (bl, br, tl, tr)
          _ -> (tl, bl, tr, br)

        -- Direction vectors from corners to their opposites
        dir1 = (fst opp1 - fst c1, snd opp1 - snd c1)
        dir2 = (fst opp2 - fst c2, snd opp2 - snd c2)
        len1 = sqrt (fst dir1 * fst dir1 + snd dir1 * snd dir1)
        len2 = sqrt (fst dir2 * fst dir2 + snd dir2 * snd dir2)

        -- Use the average length as the reference for percentage calculation
        avgLen = (len1 + len2) / 2

        -- Calculate mouse movement projected onto the average direction
        -- This gives us movement in the "depth" direction of the quadrilateral
        avgDir =
          if avgLen < 0.0001
            then (0, 0)
            else
              ((fst dir1 + fst dir2) / (2 * avgLen), (snd dir1 + snd dir2) / (2 * avgLen))
        mouseDelta = (fst newMousePos - fst oldMousePos, snd newMousePos - snd oldMousePos)
        moveAmount = dotProduct mouseDelta (normalize avgDir)

        -- Convert to percentage: moveAmount pixels / avgLen pixels = percentage
        movePercent =
          if avgLen < 0.0001
            then 0
            else moveAmount / avgLen

        -- Move each corner by this percentage toward its opposite
        -- newC = c + movePercent * (opp - c)
        moveCorner c opp =
          ( fst c + movePercent * (fst opp - fst c)
          , snd c + movePercent * (snd opp - snd c)
          )

        newC1 = moveCorner c1 opp1
        newC2 = moveCorner c2 opp2

        -- Build new corners in display order [tl, tr, br, bl], then reverse for storage
        newCornersDisplay = case edgeIdx of
          EdgeIndex 0 -> [newC1, newC2, br, bl] -- top edge: c1=tl, c2=tr
          EdgeIndex 1 -> [tl, newC1, newC2, bl] -- right edge: c1=tr, c2=br
          EdgeIndex 2 -> [tl, tr, newC1, newC2] -- bottom edge: c1=br, c2=bl
          EdgeIndex 3 -> [newC2, tr, br, newC1] -- left edge: c1=bl, c2=tl -> newC2=tl, newC1=bl
          _ -> [tl, tr, br, bl]
      in
        P.reverse newCornersDisplay
    _ -> corners


-- TODO: Use thick lines for line-loop
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
        ThickLineSmooth
          [ getLinePoint numSegments num p1 p2
          , getLinePoint numSegments num p4 p3
          ]
          gridLineThickness

    getGridLineHor num =
      color gridColor $
        ThickLineSmooth
          [ getLinePoint numSegments num p1 p4
          , getLinePoint numSegments num p2 p3
          ]
          gridLineThickness
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


drawButton :: (Int, Int) -> Int -> Int -> Text -> (Int, Int) -> Bool -> Picture
drawButton
  (appWidth, appHeight)
  sidebarWidth
  topOffset
  btnText
  (btnWidth, btnHeight)
  isHovered =
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
        [ color (greyN $ if isHovered then 0.35 else 0.2) $
            roundedRectSolid
              (fromIntegral btnWidth)
              (fromIntegral btnHeight)
              buttonCornerRadius
        , Translate (-(fromIntegral btnWidth / 2.0) + 8) (-6) $
            getTextPicture btnText
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
        (appState.hoveredButton == Just componentIndex)
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

        isHovered = appState.hoveredButton == Just 0

        uiElements =
          pictures
            [ color (greyN $ if isHovered then 0.35 else 0.2) $
                roundedRectSolid fileSelectBtnWidth fileSelectBtnHeight buttonCornerRadius
            , Translate (-43) (-7) $ getTextPicture "Select Files"
            ]
      pure uiElements
    ImageView -> do
      case appState.images of
        [] -> pure mempty
        image : _otherImages -> do
          let
            appWidthInteg = fromIntegral appState.appWidth
            sidebarWidthInteg = fromIntegral appState.sidebarWidth

          pure $
            Pictures $
              ( ( [ Scale
                      appState.scaleFactor
                      appState.scaleFactor
                      ( case image of
                          ImageToLoad{} -> mempty
                          ImageData{content} -> content
                      )
                  , appState.corners & drawEdges
                  , appState.corners & drawGrid
                  ]
                    <> drawCornersWithColors appState.corners
                    <> drawEdgeHandles appState.corners
                )
                  <&> Translate (-(sidebarWidthInteg / 2.0)) 0
              )
                <> [ drawSidebar
                      appWidthInteg
                      appState.appHeight
                      appState.sidebarWidth
                   ]
                <> P.zipWith
                  (drawUiComponent appState)
                  appState.uiComponents
                  [0 ..]
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
  ( (tl, (0, 0))
  , (tr, (wdth, 0))
  , (br, (wdth, hgt))
  , (bl, (0, hgt))
  )


-- | Accommodate ImageMagick's counter-clockwise direction
toCounterClock :: (a, a, a, a) -> (a, a, a, a)
toCounterClock (tl, tr, br, bl) = (tl, bl, br, tr)


appCoordToImgCoord :: AppState -> Point -> Point
appCoordToImgCoord appState point =
  ( fst point + (fromIntegral appState.sidebarWidth / 2.0)
  , snd point
  )


checkSidebarRectHit ::
  (Int, Int) -> Int -> Int -> (Int, Int) -> (Float, Float) -> Bool
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


submitSelection ::
  IORef AppState -> Controller -> AppState -> ExportMode -> IO AppState
submitSelection stateRef controller appState exportMode = do
  if appState.isRegistered
    then performExport appState exportMode
    else do
      -- Show banner and spawn delayed export
      let bannerState =
            appState
              { bannerIsVisible = True
              , tickCounter = 0
              }
      writeIORef stateRef bannerState
      controllerSetRedraw controller
      -- Spawn thread to handle banner countdown and export
      void $ forkIO $ do
        let
          totalTicks = P.round (bannerTime * fromIntegral ticksPerSecond) :: Int
          tickDelay = 1000000 `P.div` ticksPerSecond -- microseconds per tick
          -- Animate the countdown
        P.forM_ [1 .. totalTicks] $ \tick -> do
          threadDelay tickDelay
          currentState <- readIORef stateRef
          writeIORef stateRef currentState{tickCounter = tick}
          controllerSetRedraw controller
        -- Countdown finished, hide banner and perform export
        finalState <- readIORef stateRef
        let stateWithBannerHidden = finalState{bannerIsVisible = False}
        writeIORef stateRef stateWithBannerHidden
        controllerSetRedraw controller
        newState <- performExport stateWithBannerHidden exportMode
        writeIORef stateRef newState
        controllerSetRedraw controller
      pure bannerState


performExport :: AppState -> ExportMode -> IO AppState
performExport appState exportMode = do
  case appState.images of
    [] -> pure appState
    image : otherImages -> do
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
      putText $ "Projection map: " <> show projectionMapNorm

      let
        convertArgs =
          getConvertArgs
            image.inputPath
            image.outputPath
            projectionMapNorm
            targetShape
            exportMode

      when (appState.transformBackend == ImageMagickBackend) $ do
        putText $
          "Arguments for convert command:\n"
            <> T.unlines convertArgs

      correctAndWrite
        appState.transformBackend
        image.inputPath
        image.outputPath
        projectionMapNorm
        exportMode
        convertArgs
        image.rotation
        image.isFlipped

      if P.null otherImages
        then exitSuccess
        else loadFileIntoState appState{images = otherImages}


handleImageViewEvent ::
  IORef AppState -> Controller -> Event -> AppState -> IO AppState
handleImageViewEvent stateRef controller event appState =
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
          submitSelection stateRef controller appState UnmodifiedExport
        Just (Button{text = "Save Gray"}) ->
          submitSelection stateRef controller appState GrayscaleExport
        Just (Button{text = "Save BW"}) ->
          submitSelection stateRef controller appState BlackWhiteExport
        Just (Button{text = "Save BW Smooth"}) ->
          submitSelection stateRef controller appState BlackWhiteSmoothExport
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
            clickedEdge = findClickedEdge point appState.corners

          pure $ case clickedCorner of
            Just cornerPoint ->
              appState
                { cornerDragged = Just cornerPoint
                , edgeDragged = Nothing
                , lastMousePos = Nothing
                }
            Nothing -> case clickedEdge of
              Just edgeIdx ->
                appState
                  { edgeDragged = Just edgeIdx
                  , cornerDragged = Nothing
                  , lastMousePos = Just point
                  }
              Nothing ->
                appState
                  & flip addCorner point
                  & ( \state_ ->
                        state_
                          { cornerDragged = Just point
                          , edgeDragged = Nothing
                          , lastMousePos = Nothing
                          }
                    )
    EventKey (MouseButton LeftButton) Gl.Up _ _ -> do
      pure $
        appState{cornerDragged = Nothing, edgeDragged = Nothing, lastMousePos = Nothing}
    EventMotion newPoint -> do
      let
        point = appCoordToImgCoord appState newPoint

        -- Check which button is being hovered
        hoveredBtn =
          P.find
            ( \(component, componentIndex) -> case component of
                Button _ width height _ ->
                  checkSidebarRectHit
                    (appState.appWidth, appState.appHeight)
                    appState.sidebarWidth
                    (sidebarPaddingTop + (componentIndex * sidebarGridHeight))
                    (width, height)
                    newPoint
                _ -> False
            )
            (P.zip appState.uiComponents [0 ..])
            <&> snd

      pure $ case appState.cornerDragged of
        Just cornerPoint ->
          let
            cornerIndexMb =
              elemIndex
                cornerPoint
                appState.corners
          in
            case cornerIndexMb of
              Nothing -> appState{hoveredButton = hoveredBtn}
              Just cornerIndex ->
                appState
                  { corners =
                      replaceElemAtIndex
                        cornerIndex
                        point
                        appState.corners
                  , cornerDragged = Just point
                  , hoveredButton = hoveredBtn
                  }
        Nothing -> case (appState.edgeDragged, appState.lastMousePos) of
          (Just edgeIdx, Just lastPos) ->
            let
              newCorners = moveEdgePreservingAngles edgeIdx lastPos point appState.corners
            in
              appState
                { corners = newCorners
                , lastMousePos = Just point
                , hoveredButton = hoveredBtn
                }
          _ -> appState{hoveredButton = hoveredBtn}
    EventKey (SpecialKey KeyEnter) Gl.Down _ _ ->
      submitSelection stateRef controller appState UnmodifiedExport
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


handleEvent :: IORef AppState -> Controller -> Event -> AppState -> IO AppState
handleEvent stateRef controller event appState =
  case appState.currentView of
    HomeView -> handleHomeEvent stateRef controller event appState
    ImageView -> handleImageViewEvent stateRef controller event appState
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


getConvertArgs ::
  FilePath -> FilePath -> ProjMap -> (Float, Float) -> ExportMode -> [Text]
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
      -- TODO: Use the correct algorithm as seen in
      --       https://github.com/ad-si/dotfiles/blob/master/bin/level
      BlackWhiteSmoothExport -> ["-auto-threshold", "OTSU", "-monochrome"]
    <> [ "+repage"
       , fixOutputPath exportMode outPath
       ]


-- TODO: Projection map output coordinates must be used and not recalculated
correctAndWrite ::
  TransformBackend ->
  FilePath ->
  FilePath ->
  ProjMap ->
  ExportMode ->
  [Text] ->
  Float ->
  Bool ->
  IO ()
correctAndWrite transformBackend inPath outPath ((bl, _), (tl, _), (tr, _), (br, _)) exportMode args rotation isFlipped = do
  currentDir <- getCurrentDirectory

  case transformBackend of
    ImageMagickBackend -> do
      P.putText "ℹ️ Use ImageMagick backend"
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
        argsNorm = args <&> T.unpack
        successMessage =
          "✅ Successfully saved converted image at "
            <> fixOutputPath exportMode outPath

      -- TODO: Add CLI flag to switch between them
      case conversionMode of
        CallConversion -> do
          resultBundled <- try $ callProcess magickBin argsNorm

          case resultBundled of
            Right _ -> putText successMessage
            Left (errLocal :: IOException) -> do
              P.putErrLn $ P.displayException errLocal
              putText "Use global installation of ImageMagick"

              -- Add more possible installation paths to PATH
              path <- getEnv "PATH"
              setEnv "PATH" $
                path
                  <> ":"
                  <> P.intercalate
                    ":"
                    [ "/usr/local/bin"
                    , "/usr/local/sbin"
                    , "/opt/homebrew/bin"
                    ]

              resultMagick <- try $ callProcess "magick" argsNorm
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
    --
    HipBackend -> do
      P.putText "ℹ️ Use Hip backend"
      pictureMetadataEither <- loadImage inPath
      case pictureMetadataEither of
        Left error -> do
          P.putText error
        Right (Bitmap _bitmapData, _metadatas) -> do
          uncorrected <- readImageRGBA inPath

          let
            cornersClockwiseFromTopLeft :: V4 (V2 Double)
            cornersClockwiseFromTopLeft = do
              let
                toV2 :: (Float, Float) -> V2 Double
                toV2 (x, y) = realToFrac <$> V2 x y

              -- Map from AppState corner variables (tl, tr, br, bl)
              -- to clockwise order from top-left.
              -- AppState.corners are stored in reverse order,
              -- so we map: bl->tl, br->tr, tr->br, tl->bl
              V4 (toV2 bl) (toV2 br) (toV2 tr) (toV2 tl)

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

            outputSize :: Sz Ix2
            outputSize@(Sz (height :. width)) =
              determineOutputSize cornersClockwiseFromTopLeft

            corrected :: Image (Alpha (SRGB 'Linear)) Double
            corrected =
              uncorrected
                & transform
                  (outputSize,)
                  ( \(Sz (srcHeigh :. srcWidth)) getPixel (Ix2 irow icol) -> do
                      let
                        V3 colCrd rowCrd p =
                          correctionTransform
                            !* V3
                              (fromIntegral icol)
                              (fromIntegral irow)
                              1
                        colCrd' =
                          max 0 (min (colCrd / p) (fromIntegral $ srcWidth - 1))
                        rowCrd' =
                          max 0 (min (rowCrd / p) (fromIntegral $ srcHeigh - 1))

                      interpolate Bilinear getPixel (rowCrd', colCrd')
                  )

          case exportMode of
            UnmodifiedExport -> writeImage outPath corrected
            GrayscaleExport -> pure ()
            BlackWhiteExport -> pure ()
            BlackWhiteSmoothExport -> pure ()
        --
        Right _ -> do
          P.putText "Unsupported image format"
    --
    FlatCVBackend -> do
      P.putText "ℹ️ Use FlatCV backend"
      pictureMetadataEither <- loadImage inPath
      case pictureMetadataEither of
        Left error -> do
          P.putText error
        Right (Bitmap bitmapData, metadatas) -> do
          P.putText "" -- Line break
          prettyPrintArray metadatas

          let
            cornersClockwiseFromTopLeft :: V4 (V2 Double)
            cornersClockwiseFromTopLeft = do
              let
                toV2 :: (Float, Float) -> V2 Double
                toV2 (x, y) = realToFrac <$> V2 x y

              -- Map from AppState corner variables (tl, tr, br, bl)
              -- to clockwise order from top-left.
              -- AppState.corners are stored in reverse order,
              -- so we map: bl->tl, br->tr, tr->br, tl->bl.
              V4 (toV2 bl) (toV2 br) (toV2 tr) (toV2 tl)

            Sz (docHeight :. docWidth) =
              determineOutputSize cornersClockwiseFromTopLeft

            -- Output dimensions: the document appears with docWidth × docHeight
            -- in the displayed image. The output should preserve this aspect ratio.
            -- No swap needed - the destination corner rotation handles orientation.
            (outWidth, outHeight) = (docWidth, docHeight)

            -- Map AppState corner variables to logical coordinate positions
            -- AppState.corners are stored in reverse order,
            -- so we map: bl->tl, br->tr, tr->br, tl->bl
            srcCorners :: Corners
            srcCorners =
              Corners
                { tlX = float2Double $ fst bl -- bl from AppState maps to tl
                , tlY = float2Double $ snd bl
                , trX = float2Double $ fst br -- br from AppState maps to tr
                , trY = float2Double $ snd br
                , brX = float2Double $ fst tr -- tr from AppState maps to br
                , brY = float2Double $ snd tr
                , blX = float2Double $ fst tl -- tl from AppState maps to bl
                , blY = float2Double $ snd tl
                }

            -- Target rectangle for perspective transform.
            -- The output image is outWidth × outHeight.
            -- We need to rotate the destination corners to compensate for EXIF rotation
            -- so that the content appears upright in the output.
            --
            -- The perspective transform maps: for each output pixel (x,y), find source pixel.
            -- dstCorners defines where in output space each source corner maps to.
            -- So dstCorners.TL is where adjustedSrcCorners.TL content appears in output.
            --
            -- For EXIF 6 (90° CW display): displayed TL should be at output TL.
            -- adjustedSrcCorners.TL = raw position of displayed TL.
            -- We want this to appear at output (0,0), so dstCorners.TL = (0,0).
            --
            -- But wait - the raw content is rotated. If we just use an upright rectangle,
            -- the content comes out rotated. We need to rotate the dst rectangle so
            -- the sampling happens in the correct orientation.
            dstCorners :: Corners
            dstCorners =
              let
                ow = int2Double outWidth
                oh = int2Double outHeight
              in
                case P.round rotation :: Int of
                  90 ->
                    -- For 90° CW display rotation (EXIF 6):
                    -- The adjustedSrcCorners map displayed corners to raw positions.
                    -- We need dst corners that produce upright content.
                    -- Rotate dst 90° CCW so content rotates 90° CW in output.
                    Corners
                      { tlX = ow
                      , tlY = 0
                      , trX = ow
                      , trY = oh
                      , brX = 0
                      , brY = oh
                      , blX = 0
                      , blY = 0
                      }
                  -90 ->
                    -- For 90° CCW display rotation (EXIF 8):
                    -- Rotate dst 90° CW so content rotates 90° CCW in output.
                    Corners
                      { tlX = 0
                      , tlY = oh
                      , trX = 0
                      , trY = 0
                      , brX = ow
                      , brY = 0
                      , blX = ow
                      , blY = oh
                      }
                  270 ->
                    -- Same as -90°
                    Corners
                      { tlX = 0
                      , tlY = oh
                      , trX = 0
                      , trY = 0
                      , brX = ow
                      , brY = 0
                      , blX = ow
                      , blY = oh
                      }
                  -270 ->
                    -- Same as 90°
                    Corners
                      { tlX = ow
                      , tlY = 0
                      , trX = ow
                      , trY = oh
                      , brX = 0
                      , brY = oh
                      , blX = 0
                      , blY = 0
                      }
                  180 ->
                    -- Rotate destination 180°
                    Corners
                      { tlX = ow
                      , tlY = oh
                      , trX = 0
                      , trY = oh
                      , brX = 0
                      , brY = 0
                      , blX = ow
                      , blY = 0
                      }
                  -180 ->
                    -- Same as 180°
                    Corners
                      { tlX = ow
                      , tlY = oh
                      , trX = 0
                      , trY = oh
                      , brX = 0
                      , brY = 0
                      , blX = ow
                      , blY = 0
                      }
                  _ ->
                    -- No rotation - standard upright rectangle
                    Corners
                      { tlX = 0
                      , tlY = 0
                      , trX = ow
                      , trY = 0
                      , brX = ow
                      , brY = oh
                      , blX = 0
                      , blY = oh
                      }

            rawWidth = P.fst bitmapData.bitmapSize
            rawHeight = P.snd bitmapData.bitmapSize
            (srcWidth, srcHeight) =
              if (P.round rotation `P.mod` 180) == (90 :: Int)
                then (rawHeight, rawWidth)
                else (rawWidth, rawHeight)

            adjustedSrcCorners =
              applyRotationToCorners srcCorners srcWidth srcHeight rotation isFlipped

          putText "\nOriginal Source Corners:"
          P.putStrLn $ prettyShowCorners srcCorners

          putText "\nDestination Corners:"
          putText $ toS (prettyShowCorners dstCorners) & T.replace ".0" ""

          putText $ "\nEXIF Rotation: " <> show rotation

          putText "\nAdjusted Source Corners:"
          P.putStrLn $ prettyShowCorners adjustedSrcCorners

          srcCornersPtr <- new adjustedSrcCorners
          dstCornersPtr <- new dstCorners
          transMatPtr <- FCV.calculatePerspectiveTransformPtr dstCornersPtr srcCornersPtr
          free srcCornersPtr
          free dstCornersPtr
          transMat <- peek transMatPtr

          putText "\nTransformation Matrix:"
          P.putStrLn $ prettyShowMatrix3x3 transMat

          let pngOutPath = replaceExtension outPath "png"

          withForeignPtr (castForeignPtr bitmapData.bitmapPointer) $ \ptr -> do
            resultImg <-
              FCV.applyMatrix3x3Ptr
                (fromIntegral rawWidth)
                (fromIntegral rawHeight)
                ptr
                (fromIntegral outWidth)
                (fromIntegral outHeight)
                transMatPtr
            resultImgForeignPtr <- newForeignPtr_ (castPtr resultImg)
            free transMatPtr

            case exportMode of
              UnmodifiedExport -> do
                let img = imageFromUnsafePtr outWidth outHeight resultImgForeignPtr
                savePngImage pngOutPath (ImageRGBA8 img)
              --
              GrayscaleExport -> do
                grayImgPtr <-
                  FCV.grayscaleStretchPtr
                    (fromIntegral outWidth)
                    (fromIntegral outHeight)
                    resultImg
                grayImgForeignPtr <- newForeignPtr_ (castPtr grayImgPtr)
                let grayImg = imageFromUnsafePtr outWidth outHeight grayImgForeignPtr
                savePngImage pngOutPath (ImageRGBA8 grayImg)
              --
              BlackWhiteExport -> do
                bwImgPtr <-
                  FCV.bwSmartPtr (fromIntegral outWidth) (fromIntegral outHeight) False resultImg
                bwImgForeignPtr <- newForeignPtr_ (castPtr bwImgPtr)
                let bwImg = imageFromUnsafePtr outWidth outHeight bwImgForeignPtr
                savePngImage pngOutPath (ImageRGBA8 bwImg)
              --
              BlackWhiteSmoothExport -> do
                bwImgPtr <-
                  FCV.bwSmartPtr (fromIntegral outWidth) (fromIntegral outHeight) True resultImg
                bwImgForeignPtr <- newForeignPtr_ (castPtr bwImgPtr)
                let bwImg = imageFromUnsafePtr outWidth outHeight bwImgForeignPtr
                savePngImage pngOutPath (ImageRGBA8 bwImg)

            putText "\n✅ Wrote file to:"
            P.putStrLn pngOutPath
        --
        Right _ -> do
          P.putText "Unsupported image format"


loadAndStart :: Config -> Maybe [FilePath] -> IO ()
loadAndStart config filePathsMb = do
  let
    isRegistered = config.licenseKey `elem` licenses
    stateDraft =
      initialState
        { transformBackend = config.transformBackendFlag
        , isRegistered = isRegistered
        , bannerIsVisible = False
        }

  screenSize <- getScreenSize

  putText "Starting the app …"

  -- Create IORef to hold Controller once available
  controllerRef <-
    newIORef
      ( Controller
          { controllerSetRedraw = pure ()
          , controllerModifyViewPort = const (pure ())
          }
      )

  case filePathsMb of
    Nothing -> do
      stateRef <- newIORef stateDraft
      interactIO
        (appStateToWindow screenSize stateDraft)
        black
        stateRef
        (makePictureFromRef stateRef)
        (handleEventWithRef stateRef controllerRef)
        (initController controllerRef)
    Just filePaths -> do
      let
        images =
          filePaths <&> \filePath ->
            ImageToLoad{filePath = filePath}

      appState <- loadFileIntoState stateDraft{images = images}
      stateRef <- newIORef appState

      interactIO
        (appStateToWindow screenSize appState)
        black
        stateRef
        (makePictureFromRef stateRef)
        (handleEventWithRef stateRef controllerRef)
        (initController controllerRef)


-- | Wrapper to read state from IORef and render
makePictureFromRef :: IORef AppState -> IORef AppState -> IO Picture
makePictureFromRef stateRef _ = do
  appState <- readIORef stateRef
  makePicture appState


-- | Wrapper to handle events and update IORef
handleEventWithRef ::
  IORef AppState ->
  IORef Controller ->
  Event ->
  IORef AppState ->
  IO (IORef AppState)
handleEventWithRef stateRef controllerRef event _ = do
  appState <- readIORef stateRef
  controller <- readIORef controllerRef
  newState <- handleEvent stateRef controller event appState
  writeIORef stateRef newState
  pure stateRef


-- | Store the Controller for later use by async operations
initController :: IORef Controller -> Controller -> IO ()
initController controllerRef controller = do
  writeIORef controllerRef controller


helpMessage :: Text
helpMessage =
  T.unlines ["Usage: perspec <image> [image…]"]
