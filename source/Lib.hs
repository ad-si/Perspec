{-# LANGUAGE DataKinds #-}

module Lib where

import Protolude (
  Applicative (pure),
  Bool (..),
  Char,
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
  Ord (max, min, (<), (<=), (>), (>=)),
  Semigroup ((<>)),
  Text,
  const,
  elem,
  flip,
  fromIntegral,
  fromMaybe,
  fst,
  otherwise,
  putText,
  realToFrac,
  show,
  snd,
  toS,
  try,
  void,
  ($),
  (&),
  (&&),
  (<$>),
  (<&>),
  (||),
 )

import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay, throwTo)
import Data.IORef (
  IORef,
  atomicModifyIORef',
  modifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.List as DL (elemIndex, minimum, zip3)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Foreign (castForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (new)
import Foreign.Ptr (castPtr)
import GHC.Float (float2Double, int2Double)
import Protolude qualified as P
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (replaceExtension)
import System.Info (os)
import System.Process (callProcess)

import Brillo (
  Display (InWindow),
  Picture (
    Bitmap,
    Color,
    Pictures,
    Polygon,
    Rotate,
    Scale,
    ThickArc,
    ThickCircle,
    ThickLine,
    Translate,
    TrueTypeText
  ),
  Point,
  black,
  greyN,
  makeColor,
  rectangleSolid,
 )
import Brillo.Data.Cursor (CursorShape (..))
import Brillo.Interface.Environment (getScreenSize)
import Brillo.Interface.IO.Game as Gl (
  Color,
  Event (..),
  Key (MouseButton, SpecialKey),
  KeyState (Down, Up),
  Modifiers (..),
  MouseButton (LeftButton),
  SpecialKey (KeyEnter, KeyEsc),
 )
import Brillo.Interface.IO.Interact (Controller (..), interactIO)
import Brillo.Rendering (BitmapData (..))
import Codec.Picture (
  DynamicImage (ImageRGBA8),
  imageFromUnsafePtr,
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
import PngExif (clearExifOrientation, extractExifBytesFromFile, savePngWithExif)

import Correct (calculatePerspectiveTransform, determineOutputSize)
import FlatCV (
  Corners (..),
 )
import FlatCV qualified as FCV
import Home (handleHomeEvent)
import Ocr (recognizeWords)
import OpenFiles (installOpenFilesHandler, setOpenFilesCallback)
import Pdf (writeSearchablePdf)
import Types (
  AppState (..),
  Config (transformBackendFlag),
  Corner,
  CornersTup,
  EdgeIndex (..),
  ExportSettings (..),
  ExportStyle (..),
  ImageData (..),
  OcrLanguage,
  OutputFormat (..),
  PdfAction (..),
  PdfDocument (..),
  PdfPage (..),
  ProjMap,
  SidebarTarget (..),
  TransformBackend (..),
  UiComponent (..),
  View (..),
  exportStyleName,
  initialState,
  ocrLanguageName,
  outputFormatName,
 )
import Utils (
  applyRotationToCorners,
  buttonTextHeight,
  calcInitAppSize,
  calcInitWindowPos,
  calculateSizes,
  getColoredTextPicture,
  getCorners,
  getTextPicture,
  imagesToLoad,
  loadFileIntoState,
  loadImage,
  prettyPrintArray,
  resolveFontPath,
  resolveUiScale,
 )


{-| Deliver @ExitSuccess@ to the main thread so Brillo's main loop unwinds
and GHC's top-level handler performs a clean exit. Using @throwTo@ (rather
than @exitSuccess@) is necessary because the banner-countdown worker runs
in a forked thread — throwing there would only kill the worker while Brillo
kept running.
-}
exitCleanly :: ThreadId -> IO ()
exitCleanly mainTid = throwTo mainTid ExitSuccess


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
    Polygon $ topRight <> topLeft <> bottomLeft <> bottomRight


sidebarPaddingTop :: Int
sidebarPaddingTop = 50


sidebarGridHeight :: Int
sidebarGridHeight = 40


-- | Space between the 30 units high buttons in their grid slots
sidebarComponentGap :: Int
sidebarComponentGap = 10


-- | Height of the slot a separator takes up in the sidebar
sidebarSeparatorHeight :: Int
sidebarSeparatorHeight = 20


-- | Height of the slot a label takes up in the sidebar
sidebarLabelHeight :: Int
sidebarLabelHeight = 32


-- | Distance from the top of a label's slot to the baseline of its text
sidebarLabelBaseline :: Int
sidebarLabelBaseline = 24


-- | Sidebar component with its position
data PlacedComponent = PlacedComponent
  { component :: UiComponent
  , topOffset :: Int
  -- ^ Distance from the top of the window to the top of the component
  }


-- | Lay out the sidebar components from top to bottom
placeComponents :: [UiComponent] -> [PlacedComponent]
placeComponents components =
  let
    slotHeight = \case
      Label{} -> sidebarLabelHeight
      Separator{} -> sidebarSeparatorHeight
      _ -> sidebarGridHeight
    topOffsets = P.scanl (+) sidebarPaddingTop (components <&> slotHeight)
  in
    P.zipWith PlacedComponent components topOffsets


buttonCornerRadius :: Float
buttonCornerRadius = 5


ticksPerSecond :: Int
ticksPerSecond = 10


bannerTime :: Float
bannerTime = 10 -- seconds


-- | URL opened by the "Buy License" button in the banner
buyLicenseUrl :: Text
buyLicenseUrl = "https://feram.gumroad.com/l/perspec"


{-| Open a URL in the system default browser.
Returns 'Left' with an error message when the platform-specific command fails.
-}
openUrl :: Text -> IO (Either Text ())
openUrl url = do
  let
    urlStr = T.unpack url
    attempt =
      try $ case os of
        "darwin" -> callProcess "open" [urlStr]
        "mingw32" -> callProcess "cmd" ["/c", "start", "", urlStr]
        _ -> callProcess "xdg-open" [urlStr]
  result <- attempt
  case result of
    Right () -> pure $ Right ()
    Left (err :: IOException) ->
      pure $ Left $ toS $ P.displayException err


-- | Bounding box of the "Buy License" button relative to banner center
buyLicenseButtonCenter :: Point
buyLicenseButtonCenter = (0, 5)


buyLicenseButtonSize :: (Float, Float)
buyLicenseButtonSize = (220, 56)


isBuyLicenseClick :: Point -> Bool
isBuyLicenseClick (clickX, clickY) =
  let
    (cx, cy) = buyLicenseButtonCenter
    (bw, bh) = buyLicenseButtonSize
  in
    clickX > cx - bw / 2
      && clickX < cx + bw / 2
      && clickY > cy - bh / 2
      && clickY < cy + bh / 2


{-| Approximate rendered width at the given pixel height.

Brillo does not expose a FreeType-backed measurement, so this sums
per-character width factors calibrated for Arial / DejaVu Sans. A single
uniform factor noticeably miscenters strings with many narrow or wide
letters; this is closer but still an approximation, so expect minor offsets.
-}
textWidthApprox :: Int -> Text -> Float
textWidthApprox sz txt =
  let
    sizeF = fromIntegral sz
    charFactor :: Char -> Float
    charFactor c
      | c `elem` ("iIl1.,;:'`|!" :: [Char]) = 0.28
      | c == ' ' = 0.28
      | c `elem` ("frtj()[]/\\" :: [Char]) = 0.36
      | c `elem` ("cksvxyz" :: [Char]) = 0.50
      | c `elem` ("mw" :: [Char]) = 0.85
      | c `elem` ("MW" :: [Char]) = 0.88
      | c >= 'A' && c <= 'Z' = 0.67
      | otherwise = 0.56
  in
    T.foldr (\c acc -> charFactor c * sizeF + acc) 0 txt


bannerImage :: Maybe FilePath -> Maybe Text -> Picture
bannerImage fontPathMb urlErrorMb =
  let
    trueTypeText :: Int -> Text -> Picture
    trueTypeText sz txt =
      case fontPathMb of
        Nothing -> mempty
        Just fontPath -> TrueTypeText fontPath sz txt

    bgColor = makeColor 0.92 0.92 0.92 1.0
    titleColor = greyN 0.15
    bodyColor = greyN 0.25
    accentColor = makeColor 0.2 0.7 0.3 1.0
    errorColor = makeColor 0.8 0.2 0.2 1.0
    buttonColor = accentColor
    buttonTextColor = greyN 0.98

    bannerWidth = 800
    bannerHeight = 600

    titleSize = 42 :: Int
    bodySize = 26 :: Int
    buttonTextSize = 24 :: Int
    errorSize = 18 :: Int

    centeredText :: Int -> Float -> Gl.Color -> Text -> Picture
    centeredText sz y col txt =
      Translate (-(textWidthApprox sz txt / 2)) (y - fromIntegral sz / 2) $
        Color col $
          trueTypeText sz txt

    (btnCx, btnCy) = buyLicenseButtonCenter
    (btnW, btnH) = buyLicenseButtonSize
    btnLabel = "Buy License"
    btnLabelW = textWidthApprox buttonTextSize btnLabel

    buyLicenseButton =
      Translate btnCx btnCy $
        Pictures
          [ Color buttonColor $ roundedRectSolid btnW btnH 10
          , Translate
              (-(btnLabelW / 2))
              (-(fromIntegral buttonTextSize * 3 / 8))
              ( Color buttonTextColor $
                  trueTypeText buttonTextSize btnLabel
              )
          ]

    errorLines = case urlErrorMb of
      Nothing -> []
      Just _ ->
        [ centeredText
            errorSize
            (-45)
            errorColor
            "Could not open browser. Please visit:"
        , centeredText
            errorSize
            (-70)
            errorColor
            buyLicenseUrl
        ]
  in
    Pictures $
      [ Color bgColor $
          roundedRectSolid bannerWidth bannerHeight 20
      , centeredText titleSize 220 titleColor "Unlock All Features"
      , Translate 0 180 $
          Color accentColor $
            rectangleSolid 500 6
      , centeredText
          bodySize
          110
          bodyColor
          "To remove this annoying banner, please"
      , centeredText
          bodySize
          75
          bodyColor
          "obtain a licensed version of Perspec:"
      , buyLicenseButton
      ]
        <> errorLines
        <> [ centeredText
               bodySize
               (-105)
               bodyColor
               "I hate to bother you with this, but"
           , centeredText
               bodySize
               (-140)
               bodyColor
               "sustainable development is hardly possible"
           , centeredText
               bodySize
               (-175)
               bodyColor
               "otherwise."
           , centeredText
               bodySize
               (-245)
               bodyColor
               "Thank you for your help!"
           ]


appStateToWindow :: (Int, Int) -> AppState -> Display
appStateToWindow screenSize appState = do
  let
    toWindowUnits value =
      P.round (fromIntegral value * appState.uiScale)

    -- Windows are sized in the window coordinates of the display,
    -- while the app lays itself out in logical units
    appSize =
      ( toWindowUnits appState.appWidth
      , toWindowUnits appState.appHeight
      )
    windowPos = calcInitWindowPos screenSize appSize

  case appState.images of
    [] -> InWindow "Perspec" appSize windowPos
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
          InWindow
            "Perspec - Banner"
            (toWindowUnits (800 :: Int), toWindowUnits (600 :: Int))
            (10, 10)


drawCorner :: Gl.Color -> Point -> Picture
drawCorner cornerColor (x, y) =
  Translate
    x
    y
    (Color cornerColor $ ThickCircle cornCircRadius cornCircThickness)


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
        Color handleColor $
          Polygon pillShape


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


drawEdges :: [Point] -> Picture
drawEdges [p1, p2, p3, p4] =
  let
    -- Outer edge of corner circle
    cornerOffset = cornCircRadius + cornCircThickness / 2

    -- Shorten a line segment by offsetting both endpoints inward
    shortenLine :: Point -> Point -> (Point, Point)
    shortenLine (x1, y1) (x2, y2) =
      let
        dx = x2 - x1
        dy = y2 - y1
        len = sqrt (dx * dx + dy * dy)
        ux = dx / len -- unit vector
        uy = dy / len
      in
        ( (x1 + ux * cornerOffset, y1 + uy * cornerOffset)
        , (x2 - ux * cornerOffset, y2 - uy * cornerOffset)
        )

    mkEdge pA pB =
      let (start, end) = shortenLine pA pB
      in  ThickLine [start, end] gridLineThickness
  in
    Color gridColor $
      Pictures
        [ mkEdge p1 p2
        , mkEdge p2 p3
        , mkEdge p3 p4
        , mkEdge p4 p1
        ]
drawEdges _ = mempty


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
      Color gridColor $
        ThickLine
          [ getLinePoint numSegments num p1 p2
          , getLinePoint numSegments num p4 p3
          ]
          gridLineThickness

    getGridLineHor num =
      Color gridColor $
        ThickLine
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
    ( Color (greyN 0.1) $
        rectangleSolid
          (fromIntegral width)
          (fromIntegral appHeight)
    )


-- | Axis-aligned rectangle in window coordinates
data Rect = Rect
  { minX :: Float
  , minY :: Float
  , maxX :: Float
  , maxY :: Float
  }
  deriving (P.Show, Eq)


isInsideRect :: Rect -> Point -> Bool
isInsideRect rect (x, y) =
  x > rect.minX && x < rect.maxX && y > rect.minY && y < rect.maxY


rectCenter :: Rect -> Point
rectCenter rect =
  ((rect.minX + rect.maxX) / 2, (rect.minY + rect.maxY) / 2)


-- | Shrink the rectangle by the given amount on each side
insetRect :: Float -> Rect -> Rect
insetRect amount rect =
  Rect
    { minX = rect.minX + amount
    , minY = rect.minY + amount
    , maxX = rect.maxX - amount
    , maxY = rect.maxY - amount
    }


-- | Check if the point (in window coordinates) lies on the sidebar
isInsideSidebar :: AppState -> Point -> Bool
isInsideSidebar appState (x, _) =
  x
    >= (fromIntegral appState.appWidth / 2.0)
      - fromIntegral appState.sidebarWidth


-- | Bounds of a sidebar component, which is centered horizontally
sidebarComponentRect :: AppState -> Int -> (Int, Int) -> Rect
sidebarComponentRect appState topOffset (width, height) =
  let
    minX =
      (fromIntegral appState.appWidth / 2.0)
        - (fromIntegral appState.sidebarWidth / 2.0)
        - (fromIntegral width / 2.0)
    maxY = (fromIntegral appState.appHeight / 2.0) - fromIntegral topOffset
  in
    Rect
      { minX
      , minY = maxY - fromIntegral height
      , maxX = minX + fromIntegral width
      , maxY
      }


{-| Split the rectangle of a segmented control into its segments.
Each segment fits its text and the remaining space is shared equally.
-}
segmentRects :: Rect -> [Text] -> [Rect]
segmentRects rect labels =
  let
    textWidths = labels <&> textWidthApprox buttonTextHeight
    extraWidth =
      (rect.maxX - rect.minX - P.sum textWidths)
        / fromIntegral (P.length labels)
    widths = textWidths <&> (+ extraWidth)
    lefts = P.scanl (+) rect.minX widths
  in
    P.zipWith
      (\left width -> rect{minX = left, maxX = left + width})
      lefts
      widths


data ControlState
  = IdleControl
  | HoveredControl
  | SelectedControl
  | DisabledControl
  deriving (Eq)


drawControlBackground :: Rect -> ControlState -> Picture
drawControlBackground rect controlState =
  let
    (centerX, centerY) = rectCenter rect
    shade = case controlState of
      IdleControl -> 0.2
      HoveredControl -> 0.35
      SelectedControl -> 0.45
      DisabledControl -> 0.15
  in
    Translate centerX centerY $
      Color (greyN shade) $
        roundedRectSolid
          (rect.maxX - rect.minX)
          (rect.maxY - rect.minY)
          buttonCornerRadius


drawControlText :: Maybe FilePath -> ControlState -> Text -> Picture
drawControlText fontPathMb controlState =
  getColoredTextPicture
    fontPathMb
    (greyN $ if controlState == DisabledControl then 0.45 else 0.9)


-- | Button with left-aligned text
drawButton :: Maybe FilePath -> Rect -> Text -> ControlState -> Picture
drawButton fontPathMb rect btnText controlState =
  Pictures
    [ drawControlBackground rect controlState
    , Translate (rect.minX + 8) (snd (rectCenter rect) - 6) $
        drawControlText fontPathMb controlState btnText
    ]


-- | Row of segments of which exactly one is selected
drawSegmentedControl ::
  Maybe FilePath -> Rect -> [(Rect, Text, ControlState)] -> Picture
drawSegmentedControl fontPathMb rect segments =
  let
    drawSegment (segmentRect, segmentText, controlState) =
      let
        (centerX, centerY) = rectCenter segmentRect
      in
        Pictures
          [ if controlState == IdleControl
              then mempty
              else drawControlBackground (insetRect 2 segmentRect) controlState
          , Translate
              (centerX - (textWidthApprox buttonTextHeight segmentText / 2))
              (centerY - 6)
              $ drawControlText fontPathMb controlState segmentText
          ]
  in
    Pictures $
      drawControlBackground rect IdleControl : (segments <&> drawSegment)


-- | Triangle pointing down to mark a drop-down menu
drawDropDownArrow :: Rect -> ControlState -> Picture
drawDropDownArrow rect controlState =
  Translate (rect.maxX - 14) (snd $ rectCenter rect) $
    Color (greyN $ if controlState == DisabledControl then 0.45 else 0.9) $
      Polygon [(-5, 3), (5, 3), (0, -3)]


-- | All styles in the order they are listed in the style select
exportStyles :: [ExportStyle]
exportStyles = [P.minBound .. P.maxBound]


-- | All formats in the order they are listed in the output format select
outputFormats :: [OutputFormat]
outputFormats = [P.minBound .. P.maxBound]


-- | All languages in the order they are listed in the language select
ocrLanguages :: [OcrLanguage]
ocrLanguages = [P.minBound .. P.maxBound]


-- | Offset from the top of the window to an option of the language select
ocrLanguageOptionOffset :: Int -> Int -> Int -> Int
ocrLanguageOptionOffset selectTopOffset selectHeight optionIndex =
  selectTopOffset + ((optionIndex + 1) * selectHeight)


-- | The OCR language is only relevant for the PDF output
isOcrLanguageSelectEnabled :: AppState -> Bool
isOcrLanguageSelectEnabled appState =
  appState.exportSettings.outputFormat == PdfFormat


-- | Images can only be added to a PDF that was started before
isAddToPdfEnabled :: AppState -> Bool
isAddToPdfEnabled appState =
  appState.exportSettings.outputFormat == PdfFormat
    && P.isJust appState.pdfDocument


-- | Action of the Enter key
defaultPdfAction :: AppState -> PdfAction
defaultPdfAction appState =
  if isAddToPdfEnabled appState
    then AddToPdf
    else NewPdf


-- | Number of the PDF a new PDF gets
nextPdfNumber :: AppState -> Int
nextPdfNumber appState =
  P.maybe 1 (\pdfDocument -> pdfDocument.number + 1) appState.pdfDocument


-- | Text of the button of the action
pdfActionText :: AppState -> PdfAction -> Text
pdfActionText appState = \case
  NewPdf -> "Start PDF " <> show (nextPdfNumber appState)
  AddToPdf -> case appState.pdfDocument of
    Just pdfDocument ->
      "Add page "
        <> show (P.length pdfDocument.pages + 1)
        <> " to PDF "
        <> show pdfDocument.number
    Nothing -> "Add to PDF"


{-| PDF the current image is added to with the action.
A new PDF is named after the output path of its first image.
-}
pdfDocumentForImage :: AppState -> PdfAction -> FilePath -> PdfDocument
pdfDocumentForImage appState pdfAction imageOutputPath =
  case (appState.pdfDocument, pdfAction) of
    (Just pdfDocument, AddToPdf) -> pdfDocument
    _ ->
      PdfDocument
        { number = nextPdfNumber appState
        , outputPath = replaceExtension imageOutputPath "pdf"
        , pages = []
        }


{-| PDF that is complete, as the current image isn't added to it.
This is also the case for the image output.
-}
completedPdfDocument :: AppState -> PdfAction -> Maybe PdfDocument
completedPdfDocument appState pdfAction =
  if isAddToPdfEnabled appState && pdfAction == AddToPdf
    then Nothing
    else appState.pdfDocument


-- | Write the PDF, unless it has no pages
writePdfDocument :: PdfDocument -> IO ()
writePdfDocument pdfDocument =
  case NE.nonEmpty pdfDocument.pages of
    Nothing -> pure ()
    Just pages -> do
      putText $ "ℹ️ Write PDF " <> show pdfDocument.number <> " …"
      writeResult <- writeSearchablePdf pdfDocument.outputPath pages
      case writeResult of
        Left error -> P.putErrText $ "❌ Failed to write the PDF: " <> error
        Right () -> do
          putText "\n✅ Wrote file to:"
          P.putStrLn pdfDocument.outputPath


-- | Target and text of the button of an action button component
actionButton :: AppState -> UiComponent -> Maybe (SidebarTarget, Text)
actionButton appState = \case
  SaveButton{}
    | appState.exportSettings.outputFormat == PdfFormat ->
        Just (PdfActionTarget NewPdf, pdfActionText appState NewPdf)
    | otherwise -> Just (SaveTarget, "Save")
  AddToPdfButton{}
    | isAddToPdfEnabled appState ->
        Just (PdfActionTarget AddToPdf, pdfActionText appState AddToPdf)
  _ -> Nothing


{-| Clickable parts of the sidebar components with their bounds.
While the language select is open, only its options can be clicked,
as they cover the components below the select.
-}
sidebarTargets :: AppState -> [(SidebarTarget, Rect)]
sidebarTargets appState =
  let
    placedComponents = placeComponents appState.uiComponents
    rectOf placed size = sidebarComponentRect appState placed.topOffset size

    componentTargets placed = case placed.component of
      StyleSelect width height ->
        P.zip
          (exportStyles <&> StyleOption)
          ( segmentRects
              (rectOf placed (width, height))
              (exportStyles <&> exportStyleName)
          )
      OutputFormatSelect width height ->
        P.zip
          (outputFormats <&> OutputFormatOption)
          ( segmentRects
              (rectOf placed (width, height))
              (outputFormats <&> outputFormatName)
          )
      OcrLanguageSelect width height
        | isOcrLanguageSelectEnabled appState ->
            [(OcrLanguageSelectTarget, rectOf placed (width, height))]
      SaveButton width height ->
        actionButtonTarget placed (width, height)
      AddToPdfButton width height ->
        actionButtonTarget placed (width, height)
      _ -> []

    actionButtonTarget placed size =
      actionButton appState placed.component
        & P.maybeToList
          <&> \(target, _) -> (target, rectOf placed size)

    optionTargets placed = case placed.component of
      OcrLanguageSelect width height ->
        P.zip [0 ..] ocrLanguages <&> \(optionIndex, language) ->
          ( OcrLanguageOption language
          , sidebarComponentRect
              appState
              (ocrLanguageOptionOffset placed.topOffset height optionIndex)
              (width, height)
          )
      _ -> []
  in
    P.concatMap
      (if appState.ocrLanguageSelectIsOpen then optionTargets else componentTargets)
      placedComponents


-- | Clickable part of the sidebar at the given point
findSidebarTargetAt :: AppState -> Point -> Maybe SidebarTarget
findSidebarTargetAt appState point =
  sidebarTargets appState
    & P.find (\(_, rect) -> isInsideRect rect point)
      <&> fst


drawUiComponent :: AppState -> PlacedComponent -> Picture
drawUiComponent appState placed =
  let
    settings = appState.exportSettings
    rectOf size = sidebarComponentRect appState placed.topOffset size
    stateOf target =
      if appState.hoveredSidebarTarget == Just target
        then HoveredControl
        else IdleControl
    segmentStateOf target isSelected
      | isSelected = SelectedControl
      | otherwise = stateOf target
    drawActionButton size =
      case actionButton appState placed.component of
        Just (target, btnText) ->
          drawButton appState.fontPath (rectOf size) btnText (stateOf target)
        Nothing -> mempty
  in
    case placed.component of
      Label labelText labelWidth ->
        Translate
          (rectOf (labelWidth, 0)).minX
          ( (fromIntegral appState.appHeight / 2.0)
              - fromIntegral (placed.topOffset + sidebarLabelBaseline)
          )
          $ getColoredTextPicture appState.fontPath (greyN 0.65) labelText
      StyleSelect selectWidth selectHeight ->
        let
          rect = rectOf (selectWidth, selectHeight)
          labels = exportStyles <&> exportStyleName
        in
          drawSegmentedControl appState.fontPath rect $
            DL.zip3 (segmentRects rect labels) labels $
              exportStyles <&> \style ->
                segmentStateOf (StyleOption style) (style == settings.style)
      OutputFormatSelect selectWidth selectHeight ->
        let
          rect = rectOf (selectWidth, selectHeight)
          labels = outputFormats <&> outputFormatName
        in
          drawSegmentedControl appState.fontPath rect $
            DL.zip3 (segmentRects rect labels) labels $
              outputFormats <&> \format ->
                segmentStateOf
                  (OutputFormatOption format)
                  (format == settings.outputFormat)
      OcrLanguageSelect selectWidth selectHeight ->
        let
          rect = rectOf (selectWidth, selectHeight)
          controlState =
            if isOcrLanguageSelectEnabled appState
              then stateOf OcrLanguageSelectTarget
              else DisabledControl
        in
          Pictures
            [ drawButton
                appState.fontPath
                rect
                (ocrLanguageName settings.ocrLanguage)
                controlState
            , drawDropDownArrow rect controlState
            ]
      Separator lineWidth ->
        -- Centered in the gap between the neighboring buttons
        Translate
          ( (fromIntegral appState.appWidth / 2.0)
              - (fromIntegral appState.sidebarWidth / 2.0)
          )
          ( (fromIntegral appState.appHeight / 2.0)
              - fromIntegral placed.topOffset
              - (fromIntegral (sidebarSeparatorHeight - sidebarComponentGap) / 2.0)
          )
          $ Color (greyN 0.3)
          $ rectangleSolid (fromIntegral lineWidth) 1
      SaveButton btnWidth btnHeight ->
        drawActionButton (btnWidth, btnHeight)
      AddToPdfButton btnWidth btnHeight ->
        drawActionButton (btnWidth, btnHeight)


{-| Options of the open language select.
Drawn after all other components, as they overlap the ones below the select.
-}
drawOcrLanguageOptions :: AppState -> Picture
drawOcrLanguageOptions appState =
  Pictures $
    sidebarTargets appState <&> \case
      (target@(OcrLanguageOption language), rect) ->
        drawButton
          appState.fontPath
          rect
          (ocrLanguageName language)
          ( if appState.hoveredSidebarTarget == Just target
              then HoveredControl
              else IdleControl
          )
      _ -> mempty


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
          Pictures
            [ Color (greyN $ if isHovered then 0.35 else 0.2) $
                roundedRectSolid fileSelectBtnWidth fileSelectBtnHeight buttonCornerRadius
            , Translate (-43) (-7) $
                getTextPicture appState.fontPath "Select Files"
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
                  , case image of
                      ImageToLoad{} ->
                        Translate (-40) (-6) $
                          getTextPicture appState.fontPath "Loading ..."
                      ImageData{} -> mempty
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
                <> ( placeComponents appState.uiComponents
                       <&> drawUiComponent appState
                   )
                <> [drawOcrLanguageOptions appState]
                <> [ if appState.bannerIsVisible
                       then bannerImage appState.fontPath appState.bannerUrlError
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


-- | Get index of closest point. Returns 0 if the list is empty.
getIndexClosest :: [Point] -> Point -> Int
getIndexClosest [] _ = 0
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


-- | Reorder corners to counter-clockwise direction
toCounterClock :: (a, a, a, a) -> (a, a, a, a)
toCounterClock (tl, tr, br, bl) = (tl, bl, br, tr)


appCoordToImgCoord :: AppState -> Point -> Point
appCoordToImgCoord appState point =
  ( fst point + (fromIntegral appState.sidebarWidth / 2.0)
  , snd point
  )


{-| Export the current image.
The PDF action is only used for the PDF output.
-}
submitSelection ::
  IORef AppState -> Controller -> PdfAction -> AppState -> IO AppState
submitSelection stateRef controller pdfAction appState = do
  -- Captured here because submitSelection runs in the event-handler (main)
  -- thread; the forked worker below needs it to signal exit back to main.
  mainTid <- myThreadId
  if appState.isRegistered
    then performExport mainTid appState appState.exportSettings pdfAction
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
        newState <-
          performExport
            mainTid
            stateWithBannerHidden
            appState.exportSettings
            pdfAction
        writeIORef stateRef newState
        controllerSetRedraw controller
      pure bannerState


performExport ::
  ThreadId -> AppState -> ExportSettings -> PdfAction -> IO AppState
performExport mainTid appState exportSettings pdfAction = do
  case appState.images of
    image@ImageData{} : otherImages ->
      case toQuadTuple (getCorners appState) of
        Left err -> do
          P.putErrLn $ "Cannot export: " <> err
          pure appState
        Right cornerTuple -> do
          let
            targetShape = getTargetShape cornerTuple
            projectionMapNorm =
              toCounterClock $
                getProjectionMap cornerTuple targetShape

          pdfPageMb <-
            correctAndExport
              appState.transformBackend
              image.inputPath
              image.outputPath
              projectionMapNorm
              exportSettings
              image.rotation
              image.isFlipped

          let
            exportState = appState{exportSettings}
            openPdfDocument =
              if exportSettings.outputFormat == PdfFormat
                then
                  let
                    pdfDocument =
                      pdfDocumentForImage exportState pdfAction image.outputPath
                  in
                    Just
                      pdfDocument
                        { pages = pdfDocument.pages <> P.maybeToList pdfPageMb
                        }
                else Nothing
            exportedState =
              exportState
                { pdfDocument = openPdfDocument
                , hasExported = True
                }

          P.forM_ (completedPdfDocument exportState pdfAction) writePdfDocument

          case (openPdfDocument, pdfPageMb) of
            (Just pdfDocument, Just _) ->
              putText $
                "ℹ️ Added page "
                  <> show (P.length pdfDocument.pages)
                  <> " to PDF "
                  <> show pdfDocument.number
            _ -> pure ()

          if P.null otherImages
            then do
              P.forM_ openPdfDocument writePdfDocument
              exitCleanly mainTid
              pure exportedState{pdfDocument = Nothing}
            else loadFileIntoState exportedState{images = otherImages}
    -- No image, or the current one is still loading
    _ -> pure appState


handleImageViewEvent ::
  IORef AppState -> Controller -> Event -> AppState -> IO AppState
handleImageViewEvent stateRef controller event appState =
  case event of
    EventKey (MouseButton LeftButton) Gl.Down _ clickedPoint
      | appState.bannerIsVisible && isBuyLicenseClick clickedPoint -> do
          result <- openUrl buyLicenseUrl
          case result of
            Right () -> pure appState{bannerUrlError = Nothing}
            Left err -> do
              P.putErrLn $ "Failed to open browser: " <> err
              pure appState{bannerUrlError = Just err}
    -- Any click closes the open language select
    -- and selects the clicked language, if any
    EventKey (MouseButton LeftButton) Gl.Down _ clickedPoint
      | appState.ocrLanguageSelectIsOpen ->
          let
            settings = appState.exportSettings
          in
            pure
              appState
                { exportSettings =
                    case findSidebarTargetAt appState clickedPoint of
                      Just (OcrLanguageOption language) ->
                        settings{ocrLanguage = language}
                      _ -> settings
                , ocrLanguageSelectIsOpen = False
                , hoveredSidebarTarget = Nothing
                }
    EventKey (MouseButton LeftButton) Gl.Down _ clickedPoint -> do
      let
        settings = appState.exportSettings

      case findSidebarTargetAt appState clickedPoint of
        Just (StyleOption style) ->
          pure appState{exportSettings = settings{style}}
        Just (OutputFormatOption outputFormat) ->
          pure appState{exportSettings = settings{outputFormat}}
        Just OcrLanguageSelectTarget ->
          pure appState{ocrLanguageSelectIsOpen = True}
        Just (OcrLanguageOption _) ->
          -- Only clickable while the select is open, which is handled above
          pure appState
        Just SaveTarget ->
          submitSelection stateRef controller (defaultPdfAction appState) appState
        Just (PdfActionTarget pdfAction) ->
          submitSelection stateRef controller pdfAction appState
        Nothing
          | isInsideSidebar appState clickedPoint ->
              pure appState
        Nothing -> do
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

        hoveredTarget = findSidebarTargetAt appState newPoint

      -- Set cursor based on hover state
      let
        isBannerButtonHovered =
          appState.bannerIsVisible && isBuyLicenseClick newPoint
      controllerSetCursor controller $
        if isBannerButtonHovered || P.isJust hoveredTarget
          then CursorHand
          else CursorArrow

      pure $ case appState.cornerDragged of
        Just cornerPoint ->
          let
            cornerIndexMb =
              elemIndex
                cornerPoint
                appState.corners
          in
            case cornerIndexMb of
              Nothing -> appState{hoveredSidebarTarget = hoveredTarget}
              Just cornerIndex ->
                appState
                  { corners =
                      replaceElemAtIndex
                        cornerIndex
                        point
                        appState.corners
                  , cornerDragged = Just point
                  , hoveredSidebarTarget = hoveredTarget
                  }
        Nothing -> case (appState.edgeDragged, appState.lastMousePos) of
          (Just edgeIdx, Just lastPos) ->
            let
              newCorners = moveEdgePreservingAngles edgeIdx lastPos point appState.corners
            in
              appState
                { corners = newCorners
                , lastMousePos = Just point
                , hoveredSidebarTarget = hoveredTarget
                }
          _ -> appState{hoveredSidebarTarget = hoveredTarget}
    -- Shift + Enter starts a new PDF
    EventKey (SpecialKey KeyEnter) Gl.Down modifiers _ ->
      submitSelection
        stateRef
        controller
        ( if modifiers.shift == Gl.Down
            then NewPdf
            else defaultPdfAction appState
        )
        appState
    EventKey (SpecialKey KeyEsc) Gl.Down _ _
      | appState.ocrLanguageSelectIsOpen ->
          pure
            appState
              { ocrLanguageSelectIsOpen = False
              , hoveredSidebarTarget = Nothing
              }
    EventKey (SpecialKey KeyEsc) Gl.Down _ _ -> do
      pure $ appState{corners = []}
    _ ->
      pure appState


{-| Convert the window coordinates of an event
to the logical units the interface is laid out in.

Brillo reports positions and sizes in window coordinates,
which are physical pixels on Windows and X11.
Doing the conversion once here keeps every hit test and layout calculation
in the same coordinate system as 'makePicture'.
-}
toLogicalEvent :: Float -> Event -> Event
toLogicalEvent uiScale event =
  let
    toLogicalPoint (x, y) = (x / uiScale, y / uiScale)
    toLogicalLength value = P.round (fromIntegral value / uiScale)
  in
    case event of
      EventKey key keyState modifiers point ->
        EventKey key keyState modifiers (toLogicalPoint point)
      EventMotion point ->
        EventMotion (toLogicalPoint point)
      EventResize (windowWidth, windowHeight) ->
        EventResize
          ( toLogicalLength windowWidth
          , toLogicalLength windowHeight
          )
      EventDrop filePaths -> EventDrop filePaths
      EventPick filePaths -> EventPick filePaths


handleEvent :: IORef AppState -> Controller -> Event -> AppState -> IO AppState
handleEvent stateRef controller windowEvent appState = do
  let event = toLogicalEvent appState.uiScale windowEvent

  case event of
    -- Track the window size in every view,
    -- so that images loaded later are laid out for the current size
    EventResize (windowWidth, windowHeight) ->
      pure $
        calculateSizes $
          appState
            { appWidth = windowWidth
            , appHeight = windowHeight
            }
    _ -> case appState.currentView of
      HomeView -> handleHomeEvent stateRef controller event appState
      ImageView -> handleImageViewEvent stateRef controller event appState
      BannerView -> pure appState


{-| Correct the image with the export settings.
The image output is written to a file,
while the PDF output is returned as a page
to be written with the other pages of its PDF.
-}

-- TODO: Projection map output coordinates must be used and not recalculated
correctAndExport ::
  TransformBackend ->
  FilePath ->
  FilePath ->
  ProjMap ->
  ExportSettings ->
  Float ->
  Bool ->
  IO (Maybe PdfPage)
correctAndExport transformBackend inPath outPath ((bl, _), (tl, _), (tr, _), (br, _)) exportSettings rotation isFlipped = do
  case transformBackend of
    HipBackend -> do
      P.putText "ℹ️ Use Hip backend"
      pictureMetadataEither <- loadImage inPath
      case pictureMetadataEither of
        Left error -> do
          P.putText error
          pure Nothing
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

          case (exportSettings.style, exportSettings.outputFormat) of
            (ColorStyle, ImageFormat) -> writeImage outPath corrected
            _ ->
              P.putErrLn
                ( "⚠️  The Hip backend only supports color images. "
                    <> "Use the FlatCV backend for "
                    <> show exportSettings
                    <> "." ::
                    Text
                )
          pure Nothing
        --
        Right _ -> do
          P.putText "Unsupported image format"
          pure Nothing
    --
    FlatCVBackend -> do
      P.putText "ℹ️ Use FlatCV backend"
      pictureMetadataEither <- loadImage inPath
      -- Clear the orientation tag so viewers don't re-rotate already-upright pixels
      exifBytes <- fmap (fmap clearExifOrientation) (extractExifBytesFromFile inPath)
      case pictureMetadataEither of
        Left error -> do
          P.putText error
          pure Nothing
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

          srcCornersPtr <- new adjustedSrcCorners
          dstCornersPtr <- new dstCorners
          transMatPtr <- FCV.calculatePerspectiveTransformPtr dstCornersPtr srcCornersPtr
          free srcCornersPtr
          free dstCornersPtr

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
            free transMatPtr

            styledImgPtr <- case exportSettings.style of
              ColorStyle -> pure resultImg
              GrayStyle ->
                FCV.grayscaleStretchPtr
                  (fromIntegral outWidth)
                  (fromIntegral outHeight)
                  resultImg
              BlackWhiteStyle ->
                FCV.bwSmartPtr (fromIntegral outWidth) (fromIntegral outHeight) False resultImg
              BlackWhiteSmoothStyle ->
                FCV.bwSmartPtr (fromIntegral outWidth) (fromIntegral outHeight) True resultImg
            styledImgForeignPtr <- newForeignPtr_ (castPtr styledImgPtr)
            let img = imageFromUnsafePtr outWidth outHeight styledImgForeignPtr

            case exportSettings.outputFormat of
              ImageFormat -> do
                void $ savePngWithExif exifBytes (ImageRGBA8 img) pngOutPath
                putText "\n✅ Wrote file to:"
                P.putStrLn pngOutPath
                pure Nothing
              --
              PdfFormat -> do
                putText "ℹ️ Recognize text …"
                ocrWordsEither <- recognizeWords exportSettings.ocrLanguage img
                case ocrWordsEither of
                  Left error -> do
                    P.putErrText $ "❌ Failed to export the image: " <> error
                    pure Nothing
                  Right ocrWords ->
                    pure $ Just PdfPage{image = img, ocrWords}
        --
        Right _ -> do
          P.putText "Unsupported image format"
          pure Nothing


imagePath :: ImageData -> FilePath
imagePath = \case
  ImageToLoad{filePath} -> filePath
  ImageData{inputPath} -> inputPath


{-| Add images to the ones to correct.
Until an image is exported, all images are sorted anew,
so that images added in several batches are still corrected in sorted order.
Afterwards, they are sorted into the images after the current one.
If the current image changes, it must be loaded afterwards.
-}
queueImages :: AppState -> [FilePath] -> AppState
queueImages appState filePaths =
  let
    queuedPaths = appState.images <&> imagePath
    newPaths = filePaths & P.filter (`P.notElem` queuedPaths)
  in
    case appState.images of
      _ | P.null newPaths -> appState
      [] ->
        appState
          { currentView = ImageView
          , images = imagesToLoad newPaths
          , corners = []
          }
      currentImage : otherImages
        | appState.hasExported ->
            appState
              { images =
                  currentImage
                    : imagesToLoad ((otherImages <&> imagePath) <> newPaths)
              }
        | otherwise ->
            case imagesToLoad (queuedPaths <> newPaths) of
              firstImage : sortedImages
                | imagePath firstImage == imagePath currentImage ->
                    appState{images = currentImage : sortedImages}
              sortedImages -> appState{images = sortedImages, corners = []}


-- | Image that was loaded in the background
data LoadedImage = LoadedImage
  { path :: FilePath
  , loadedState :: AppState
  -- ^ State with the image loaded as the current one
  }


{-| Load the current image in the background, unless it's loaded already.
The result is only kept if the image is still the current one afterwards.
It's applied to the state by the main thread with 'applyLoadedImage',
as the event handlers would otherwise overwrite it.
-}
loadCurrentImageInBackground ::
  IORef AppState -> IORef Controller -> IORef (Maybe LoadedImage) -> IO ()
loadCurrentImageInBackground stateRef controllerRef loadedImageRef = do
  appState <- readIORef stateRef
  case appState.images of
    ImageToLoad{filePath} : _ -> void $ forkIO $ do
      loadedState <- loadFileIntoState appState
      currentState <- readIORef stateRef
      case currentState.images of
        ImageToLoad{filePath = currentPath} : _
          | currentPath == filePath -> do
              writeIORef loadedImageRef $
                Just LoadedImage{path = filePath, loadedState}
              controller <- readIORef controllerRef
              controllerSetRedraw controller
        _ -> pure ()
    _ -> pure ()


-- | Make the loaded image the current one, if it still is the current image
applyLoadedImage :: LoadedImage -> AppState -> AppState
applyLoadedImage loadedImage appState =
  case (appState.images, loadedImage.loadedState.images) of
    (ImageToLoad{filePath} : otherImages, image@ImageData{} : _)
      | filePath == loadedImage.path ->
          -- Recalculated, as the window might have been resized in between
          calculateSizes
            appState
              { currentView = ImageView
              , images = image : otherImages
              , corners = loadedImage.loadedState.corners
              , scaleFactor = loadedImage.loadedState.scaleFactor
              }
    _ -> appState


{-| Add the files macOS opened with the app,
e.g. by dropping them onto its icon in the Dock.
As they can arrive in several batches in quick succession,
images are loaded in the background,
so that a later batch can still change the current image without it flashing.
-}
handleOpenedFiles ::
  IORef AppState ->
  IORef Controller ->
  IORef (Maybe LoadedImage) ->
  [FilePath] ->
  IO ()
handleOpenedFiles stateRef controllerRef loadedImageRef filePaths = do
  appState <- readIORef stateRef
  writeIORef stateRef $ queueImages appState filePaths
  loadCurrentImageInBackground stateRef controllerRef loadedImageRef
  controller <- readIORef controllerRef
  controllerSetRedraw controller


loadAndStart :: Config -> Maybe [FilePath] -> IO ()
loadAndStart config filePathsMb = do
  -- Must happen before Brillo initializes GLFW
  isReceivingOpenedFiles <- installOpenFilesHandler
  P.unless isReceivingOpenedFiles $
    P.putErrText "Warning: Files opened via the app icon can't be received."

  fontPathMb <- resolveFontPath

  case fontPathMb of
    Just _ -> pure ()
    Nothing ->
      P.putErrText
        "Warning: No usable font found on this system. \
        \Install a font like DejaVu Sans to display text."

  uiScale <- resolveUiScale
  screenSize <- getScreenSize

  let
    isRegistered = config.licenseKey `elem` licenses
    (initAppWidth, initAppHeight) = calcInitAppSize screenSize uiScale
    stateDraft =
      initialState
        { transformBackend = config.transformBackendFlag
        , isRegistered = isRegistered
        , bannerIsVisible = False
        , fontPath = fontPathMb
        , uiScale = uiScale
        , appWidth = initAppWidth
        , appHeight = initAppHeight
        }

  putText "Starting the app …"

  loadedImageRef <- newIORef Nothing

  -- Create IORef to hold Controller once available
  controllerRef <-
    newIORef
      ( Controller
          { controllerSetRedraw = pure ()
          , controllerModifyViewPort = const (pure ())
          , controllerSetCursor = const (pure ())
          }
      )

  case filePathsMb of
    Nothing -> do
      stateRef <- newIORef stateDraft
      setOpenFilesCallback $
        handleOpenedFiles stateRef controllerRef loadedImageRef
      interactIO
        (appStateToWindow screenSize stateDraft)
        black
        stateRef
        (makePictureFromRef stateRef loadedImageRef)
        (handleEventWithRef stateRef controllerRef)
        (initController controllerRef)
      writeOpenPdfDocument stateRef
    Just filePaths -> do
      appState <- loadFileIntoState stateDraft{images = imagesToLoad filePaths}
      stateRef <- newIORef appState
      setOpenFilesCallback $
        handleOpenedFiles stateRef controllerRef loadedImageRef

      interactIO
        (appStateToWindow screenSize appState)
        black
        stateRef
        (makePictureFromRef stateRef loadedImageRef)
        (handleEventWithRef stateRef controllerRef)
        (initController controllerRef)
      writeOpenPdfDocument stateRef


{-| Write the pages of the PDF that was still open when the window was closed,
so they aren't lost
-}
writeOpenPdfDocument :: IORef AppState -> IO ()
writeOpenPdfDocument stateRef = do
  appState <- readIORef stateRef
  P.forM_ appState.pdfDocument writePdfDocument


{-| Wrapper to read state from IORef and render.

'makePicture' draws in logical units,
so the result is scaled to the window coordinates of the display here.
An image loaded in the background is applied first,
as this runs on the main thread like the event handlers.
-}
makePictureFromRef ::
  IORef AppState -> IORef (Maybe LoadedImage) -> IORef AppState -> IO Picture
makePictureFromRef stateRef loadedImageRef _ = do
  loadedImageMb <-
    atomicModifyIORef' loadedImageRef (\loadedImageMb -> (Nothing, loadedImageMb))
  P.forM_ loadedImageMb $ \loadedImage ->
    modifyIORef' stateRef (applyLoadedImage loadedImage)
  appState <- readIORef stateRef
  picture <- makePicture appState
  pure $ Scale appState.uiScale appState.uiScale picture


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
