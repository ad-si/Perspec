{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
module Utils where

import Protolude (
  Bool (..),
  Double,
  Either (..),
  FilePath,
  Float,
  IO,
  IOException,
  Int,
  Maybe (Just, Nothing),
  Monoid (mempty),
  Text,
  fmap,
  fromIntegral,
  fromMaybe,
  min,
  not,
  otherwise,
  pure,
  putText,
  realToFrac,
  round,
  show,
  swap,
  ($),
  (&),
  (&&),
  (*),
  (+),
  (-),
  (/),
  (<),
  (<$>),
  (<&>),
  (<=),
  (<>),
  (==),
  (>=),
  (>>=),
 )
import Protolude qualified as P

import Brillo (
  BitmapData (bitmapPointer, bitmapSize),
  Picture (Bitmap, Color, Rotate, Scale, TrueTypeText),
  Point,
  greyN,
 )
import Brillo.Juicy (loadJuicyWithMetadata)
import Codec.Picture.Metadata (Keys (Exif), Metadatas, lookup)
import Codec.Picture.Metadata.Exif (ExifData (ExifShort), ExifTag (..))
import Control.Arrow ((>>>))
import Data.Text qualified as T
import Foreign.ForeignPtr (castForeignPtr, withForeignPtr)
import Foreign.Ptr (castPtr)
import GHC.Float (int2Double)
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  getHomeDirectory,
  listDirectory,
 )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (
  replaceBaseName,
  takeBaseName,
  takeExtension,
  takeFileName,
  (</>),
 )
import System.Info (os)
import System.Process (readProcessWithExitCode)

import FlatCV (Corners (..), detectCornersPtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Storable (peek)
import PngExif (getOrientationFromPng)
import Types (AppState (..), Corner, ImageData (..), View (..))


{-| Well known font locations for the current OS.

Checked first, as hitting one avoids both spawning a process
and walking the font directories.
-}
fontCandidates :: [FilePath]
fontCandidates = case os of
  "darwin" ->
    [ "/System/Library/Fonts/Supplemental/Arial.ttf"
    , "/Library/Fonts/Arial.ttf"
    , "/System/Library/Fonts/Helvetica.ttc"
    ]
  "mingw32" ->
    [ "C:\\Windows\\Fonts\\arial.ttf"
    , "C:\\Windows\\Fonts\\segoeui.ttf"
    , "C:\\Windows\\Fonts\\tahoma.ttf"
    ]
  _ ->
    [ "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" -- Debian, Ubuntu
    , "/usr/share/fonts/dejavu-sans-fonts/DejaVuSans.ttf" -- Fedora
    , "/usr/share/fonts/dejavu/DejaVuSans.ttf" -- RHEL, older Fedora
    , "/usr/share/fonts/TTF/DejaVuSans.ttf" -- Arch
    , "/usr/share/fonts/truetype/DejaVuSans.ttf" -- openSUSE
    , "/usr/share/fonts/liberation-sans/LiberationSans-Regular.ttf" -- Fedora
    , "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf" -- Debian
    , "/usr/share/fonts/liberation/LiberationSans-Regular.ttf" -- RHEL
    , "/usr/share/fonts/noto/NotoSans-Regular.ttf" -- Arch
    , "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf" -- Debian
    , "/usr/share/fonts/gnu-free/FreeSans.otf" -- Fedora
    , "/usr/share/fonts/truetype/freefont/FreeSans.ttf" -- Debian
    ]


{-| Font file names to look for when scanning the font directories,
in order of preference.
-}
preferredFontFiles :: [FilePath]
preferredFontFiles =
  [ "DejaVuSans.ttf"
  , "LiberationSans-Regular.ttf"
  , "NotoSans-Regular.ttf"
  , "FreeSans.ttf"
  , "FreeSans.otf"
  , "Arial.ttf"
  , "Helvetica.ttc"
  ]


{-| Extensions of the scalable font formats FreeType can rasterize
at arbitrary pixel sizes. Bitmap formats like @.pcf@ are excluded,
as they only render at their built-in sizes.
-}
scalableFontExtensions :: [Text]
scalableFontExtensions = [".ttf", ".ttc", ".otf", ".otc"]


-- | How deep to descend into the font directories during a scan
fontScanDepth :: Int
fontScanDepth = 4


-- | Directories the OS and the user keep fonts in
fontDirectories :: IO [FilePath]
fontDirectories = do
  homeEither <- P.try getHomeDirectory
  let
    home = case homeEither :: Either IOException FilePath of
      Left _ -> Nothing
      Right dir -> Just dir
    inHome subDirs = case home of
      Nothing -> []
      Just dir -> subDirs <&> (dir </>)

  pure $ case os of
    "darwin" ->
      [ "/System/Library/Fonts"
      , "/Library/Fonts"
      ]
        <> inHome ["Library/Fonts"]
    "mingw32" ->
      ["C:\\Windows\\Fonts"]
    _ ->
      [ "/usr/share/fonts"
      , "/usr/local/share/fonts"
      ]
        <> inHome [".local/share/fonts", ".fonts"]


-- | Whether the path points to an existing font file Brillo can render with
isUsableFont :: FilePath -> IO Bool
isUsableFont path = do
  let extension = T.toLower $ T.pack $ takeExtension path
  if extension `P.notElem` scalableFontExtensions
    then pure False
    else doesFileExist path


-- | First of the paths that points to a usable font file
firstUsableFont :: [FilePath] -> IO (Maybe FilePath)
firstUsableFont = \case
  [] -> pure Nothing
  path : remainingPaths -> do
    isUsable <- isUsableFont path
    if isUsable
      then pure $ Just path
      else firstUsableFont remainingPaths


{-| Ask fontconfig for the system's default sans-serif font.

This is how GTK, Qt, and Cairo locate fonts and therefore
the authoritative answer on Linux and BSD.

Beware: Fontconfig never fails, it returns the closest match it can find
even for a request it can't satisfy. The result must be validated.
-}
fontFromFontconfig :: IO (Maybe FilePath)
fontFromFontconfig = do
  resultEither <-
    P.try $
      readProcessWithExitCode "fc-match" ["-f", "%{file}", "sans-serif"] ""

  case resultEither :: Either IOException (ExitCode, [P.Char], [P.Char]) of
    -- `fc-match` is not installed
    Left _ -> pure Nothing
    Right (ExitSuccess, stdOut, _) -> do
      let path = T.unpack $ T.strip $ T.pack stdOut
      firstUsableFont [path]
    Right _ -> pure Nothing


{-| All files below the directory, up to the given depth.

Unreadable directories are skipped instead of aborting the whole scan,
as font directories can contain entries the user may not descend into.
-}
listFilesRecursive :: Int -> FilePath -> IO [FilePath]
listFilesRecursive depth directory
  | depth < 0 = pure []
  | otherwise = do
      entriesEither <- P.try $ listDirectory directory
      case entriesEither :: Either IOException [FilePath] of
        Left _ -> pure []
        Right entries -> do
          let paths = entries <&> (directory </>)
          nestedFiles <- P.forM paths $ \path -> do
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then listFilesRecursive (depth - 1) path
              else pure [path]
          pure $ P.concat nestedFiles


{-| Search the given directories for a usable font,
regardless of how they lay their fonts out.
-}
fontInDirectories :: [FilePath] -> IO (Maybe FilePath)
fontInDirectories directories = do
  files <- P.concat <$> P.forM directories (listFilesRecursive fontScanDepth)

  let
    lowerName = takeFileName >>> T.pack >>> T.toLower
    sortedFiles = P.sort files
    byPreferredName preferredFile =
      sortedFiles
        & P.find (\file -> lowerName file == lowerName preferredFile)
    -- Any sans-serif looking font, for systems with none of the above
    sansSerifFiles =
      sortedFiles
        & P.filter (\file -> "sans" `T.isInfixOf` lowerName file)
        & P.filter
          (\file -> not $ "mono" `T.isInfixOf` lowerName file)

  usablePreferred <-
    firstUsableFont $ preferredFontFiles & P.mapMaybe byPreferredName
  case usablePreferred of
    Just path -> pure $ Just path
    Nothing -> firstUsableFont sansSerifFiles


{-| Search the system's font directories for a usable font.

Last resort for systems whose font layout isn't covered by 'fontCandidates'
and which don't ship the @fc-match@ binary.
-}
fontFromDirectoryScan :: IO (Maybe FilePath)
fontFromDirectoryScan =
  fontDirectories >>= fontInDirectories


{-| Path of the font to render the UI text with.

Tried in order:

1. Well known font paths for the current OS
2. Fontconfig's default sans-serif font (Linux and BSD)
3. A scan of the system's font directories

Hardcoding a single path doesn't work,
as the layout differs between distributions
(see https://github.com/ad-si/Perspec/issues/57).

'Nothing' means no usable font was found and text won't be rendered.
This must not be fatal, as it would otherwise take down the whole app
on the first frame.
-}
resolveFontPath :: IO (Maybe FilePath)
resolveFontPath = do
  candidateMb <- firstUsableFont fontCandidates
  case candidateMb of
    Just path -> pure $ Just path
    Nothing -> do
      fontconfigMb <- fontFromFontconfig
      case fontconfigMb of
        Just path -> pure $ Just path
        Nothing -> fontFromDirectoryScan


-- | Pixel height for button text
buttonTextHeight :: Int
buttonTextHeight = 16


-- | Render text using TrueType fonts
getTextPicture :: Maybe FilePath -> Text -> Picture
getTextPicture fontPathMb txt =
  case fontPathMb of
    Nothing -> mempty
    Just fontPath ->
      Color (greyN 0.9) $
        TrueTypeText fontPath buttonTextHeight txt


isInRect :: Point -> (Float, Float, Float, Float) -> Bool
isInRect (x, y) (x1, y1, x2, y2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2


getOutPath :: FilePath -> FilePath
getOutPath filePath = do
  let outName = takeBaseName filePath <> "-fixed"
  replaceBaseName filePath outName


calcInitWindowPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
calcInitWindowPos (screenWidth, screenHeight) (appWidth, appHeight) =
  let
    initialX =
      ((fromIntegral screenWidth :: Float) / 2)
        - (fromIntegral appWidth / 2)
    initialY =
      ((fromIntegral screenHeight :: Float) / 2)
        - (fromIntegral appHeight / 2)
    -- Clamp position to ensure window stays within viewport
    clampedX = P.max 0 initialX
    clampedY = P.max 0 initialY
  in
    (round clampedX, round clampedY)


-- | Transform from origin in center to origin in top left
transToOrigTopLeft :: Int -> Int -> [Point] -> [Point]
transToOrigTopLeft width height =
  fmap
    ( \(x, y) ->
        ( x + (fromIntegral width / 2.0)
        , -(y - (fromIntegral height / 2.0))
        )
    )


-- | Transform from origin in top left to origin in center
transToOrigAtCenter :: Int -> Int -> [Point] -> [Point]
transToOrigAtCenter width height =
  fmap
    ( \(x, y) ->
        ( -((fromIntegral width / 2.0) - x)
        , (fromIntegral height / 2.0) - y
        )
    )


scalePoints :: Float -> [Point] -> [Point]
scalePoints scaleFac = fmap $
  \(x, y) -> (x / scaleFac, y / scaleFac)


getCorners :: AppState -> [Point]
getCorners appState =
  case appState.images of
    [] -> []
    image : _otherImages -> do
      scalePoints appState.scaleFactor $
        transToOrigTopLeft
          image.widthTarget
          image.heightTarget
          -- Reverse corners,
          -- because AppState.corners stores them in reverse order of addition.
          -- This converts them to clockwise from top-left for transformation.
          (P.reverse $ corners appState)


{-| Calculate the target image size, the scale factor,
and the corner positions for current image
-}
calculateSizes :: AppState -> AppState
calculateSizes appState =
  case appState.images of
    [] -> appState
    image : otherImages -> case image of
      ImageToLoad _ -> appState
      ImageData{} -> do
        let
          imgViewWidth = appState.appWidth - appState.sidebarWidth
          imgViewHeight = appState.appHeight

          imgWidthFrac = fromIntegral image.width
          imgHeightFrac = fromIntegral image.height

          scaleFactorX = fromIntegral imgViewWidth / imgWidthFrac
          scaleFactorY = fromIntegral imgViewHeight / imgHeightFrac

          scaleFactor = min scaleFactorX scaleFactorY
          imgWidthTrgt = round $ scaleFactor * imgWidthFrac
          imgHeightTrgt = round $ scaleFactor * imgHeightFrac

        appState
          { images =
              image
                { widthTarget = imgWidthTrgt
                , heightTarget = imgHeightTrgt
                }
                : otherImages
          , scaleFactor
          , corners =
              -- Re-reverse to restore the "reverse order of addition" storage format,
              -- since getCorners already reversed them to clockwise-from-top-left order.
              P.reverse $
                transToOrigTopLeft (-imgWidthTrgt) imgHeightTrgt $
                  scalePoints (1 / scaleFactor) (getCorners appState)
          }


{-| Rotation angle in degrees and horizontal flip of each EXIF orientation.
The rotation value represents how many degrees clockwise the image needs
to be rotated to display correctly (used with `Rotate rotation`).
-}
imgOrientToRotAndFlip :: ExifData -> (Float, Bool)
imgOrientToRotAndFlip = \case
  ExifShort 1 -> (0, False)
  ExifShort 2 -> (0, True) -- Horizontal flip
  ExifShort 3 -> (180, False)
  ExifShort 4 -> (180, True) -- Vertical flip = 180° + horizontal flip
  ExifShort 5 -> (90, True) -- Transpose = 90° CW + horizontal flip
  ExifShort 6 -> (90, False) -- Rotate 90° CW
  ExifShort 7 -> (-90, True) -- Transverse = 90° CCW + horizontal flip
  ExifShort 8 -> (-90, False) -- Rotate 90° CCW (270° CW)
  _ -> (0, False)


{-| Map user-selected corner coordinates
(taken from the auto-rotated/flipped display image)
back into the original un-rotated bitmap coordinate system
before computing the perspective transform.
-}
applyRotationToCorners :: Corners -> Int -> Int -> Float -> Bool -> Corners
applyRotationToCorners srcCorners srcWidth srcHeight rotation isFlipped =
  let
    w = int2Double srcWidth
    h = int2Double srcHeight

    -- Inverse of 90° CW rotation (EXIF 6)
    -- Forward: raw (x,y) → displayed (rawHeight - 1 - y, x)
    -- Inverse: displayed (x,y) → raw (y, rawHeight - 1 - x) = (y, w - 1 - x)
    -- Note: w = srcWidth = rawHeight (displayed width)
    rotatePoint90 :: (Double, Double) -> (Double, Double)
    rotatePoint90 (x, y) = (y, w - 1 - x)

    rotatePoint180 :: (Double, Double) -> (Double, Double)
    rotatePoint180 (x, y) = (w - 1 - x, h - 1 - y)

    -- Inverse of 90° CCW rotation (EXIF 8)
    -- Forward: raw (x,y) → displayed (y, rawWidth - 1 - x)
    -- Inverse: displayed (x,y) → raw (rawWidth - 1 - y, x) = (h - 1 - y, x)
    -- Note: h = srcHeight = rawWidth (displayed height)
    rotatePoint270 :: (Double, Double) -> (Double, Double)
    rotatePoint270 (x, y) = (h - 1 - y, x)

    -- Get the width of the displayed (rotated) image for flip calculation.
    -- srcWidth/srcHeight are already swapped for 90°/-90° rotations when passed in,
    -- so displayW is always just w (srcWidth = displayed width).
    displayW = w

    -- Horizontal flip: mirror x coordinates only (don't swap corners)
    -- This is applied first (inverse order of display transformation)
    flipHorizontal :: Corners -> Corners
    flipHorizontal c =
      Corners
        { tlX = displayW - 1 - c.tlX
        , tlY = c.tlY
        , trX = displayW - 1 - c.trX
        , trY = c.trY
        , brX = displayW - 1 - c.brX
        , brY = c.brY
        , blX = displayW - 1 - c.blX
        , blY = c.blY
        }

    -- If flipped, unflip first before rotating back
    unflippedCorners =
      if isFlipped
        then flipHorizontal srcCorners
        else srcCorners
  in
    case P.round rotation :: Int of
      90 ->
        let
          -- Inverse of 90° CW: Displayed TL→Raw BL, TR→TL, BR→TR, BL→BR
          (blX, blY) = rotatePoint90 (unflippedCorners.tlX, unflippedCorners.tlY)
          (tlX, tlY) = rotatePoint90 (unflippedCorners.trX, unflippedCorners.trY)
          (trX, trY) = rotatePoint90 (unflippedCorners.brX, unflippedCorners.brY)
          (brX, brY) = rotatePoint90 (unflippedCorners.blX, unflippedCorners.blY)
        in
          Corners{..}
      -270 ->
        let
          (blX, blY) = rotatePoint90 (unflippedCorners.tlX, unflippedCorners.tlY)
          (tlX, tlY) = rotatePoint90 (unflippedCorners.trX, unflippedCorners.trY)
          (trX, trY) = rotatePoint90 (unflippedCorners.brX, unflippedCorners.brY)
          (brX, brY) = rotatePoint90 (unflippedCorners.blX, unflippedCorners.blY)
        in
          Corners{..}
      -90 ->
        let
          -- Inverse of 90° CCW: Displayed TL→Raw TR, TR→BR, BR→BL, BL→TL
          (trX, trY) = rotatePoint270 (unflippedCorners.tlX, unflippedCorners.tlY)
          (brX, brY) = rotatePoint270 (unflippedCorners.trX, unflippedCorners.trY)
          (blX, blY) = rotatePoint270 (unflippedCorners.brX, unflippedCorners.brY)
          (tlX, tlY) = rotatePoint270 (unflippedCorners.blX, unflippedCorners.blY)
        in
          Corners{..}
      270 ->
        let
          (trX, trY) = rotatePoint270 (unflippedCorners.tlX, unflippedCorners.tlY)
          (brX, brY) = rotatePoint270 (unflippedCorners.trX, unflippedCorners.trY)
          (blX, blY) = rotatePoint270 (unflippedCorners.brX, unflippedCorners.brY)
          (tlX, tlY) = rotatePoint270 (unflippedCorners.blX, unflippedCorners.blY)
        in
          Corners{..}
      180 ->
        let
          (tlX, tlY) = rotatePoint180 (unflippedCorners.brX, unflippedCorners.brY)
          (trX, trY) = rotatePoint180 (unflippedCorners.blX, unflippedCorners.blY)
          (brX, brY) = rotatePoint180 (unflippedCorners.tlX, unflippedCorners.tlY)
          (blX, blY) = rotatePoint180 (unflippedCorners.trX, unflippedCorners.trY)
        in
          Corners{..}
      -180 ->
        let
          (tlX, tlY) = rotatePoint180 (unflippedCorners.brX, unflippedCorners.brY)
          (trX, trY) = rotatePoint180 (unflippedCorners.blX, unflippedCorners.blY)
          (brX, brY) = rotatePoint180 (unflippedCorners.tlX, unflippedCorners.tlY)
          (blX, blY) = rotatePoint180 (unflippedCorners.trX, unflippedCorners.trY)
        in
          Corners{..}
      _ -> unflippedCorners


-- | File extensions Perspec recognizes as loadable images.
imageExtensions :: [FilePath]
imageExtensions =
  [ ".avif"
  , ".bmp"
  , ".gif"
  , ".hdr"
  , ".jpeg"
  , ".jpg"
  , ".png"
  , ".ppm"
  , ".tiff"
  , ".webp"
  ]


-- | Check (case-insensitively) whether a file path has an image extension.
isImageFile :: FilePath -> Bool
isImageFile filePath =
  let extLower = takeExtension filePath & T.pack & T.toLower & T.unpack
  in  P.elem extLower imageExtensions


loadImage :: FilePath -> IO (Either Text (Picture, Metadatas))
loadImage filePath = do
  picMetaMaybe <- loadJuicyWithMetadata filePath

  let
    fileExtension = takeExtension filePath

  case picMetaMaybe of
    Nothing -> do
      if P.elem fileExtension imageExtensions
        then pure $ Left "Error: Image couldn't be loaded"
        else
          pure $
            Left $
              "Error: File extension \""
                <> T.pack fileExtension
                <> "\" is not supported"
    Just (picture, metadata) ->
      pure $ Right (picture, metadata)


-- | Get initial corner positions using FlatCV corner detection
getInitialCorners :: AppState -> BitmapData -> IO [Corner]
getInitialCorners appState bitmapData = do
  case appState.images of
    [] -> pure []
    image : _otherImages -> do
      let
        (rawWidth, rawHeight) = bitmapSize bitmapData

      -- Use FlatCV corner detection on the bitmap data
      detectedCorners <- withForeignPtr (castForeignPtr (bitmapPointer bitmapData)) $ \ptr -> do
        cornersPtr <-
          detectCornersPtr (castPtr ptr) (fromIntegral rawWidth) (fromIntegral rawHeight)
        corners <- peek cornersPtr
        free cornersPtr
        pure corners

      -- Rotate and flip detected corners to match the displayed image.
      -- FlatCV detects corners on the original un-rotated bitmap,
      -- but we display the image rotated and flipped according to EXIF orientation.
      let
        transformedCorners =
          rotateDetectedCorners
            detectedCorners
            rawWidth
            rawHeight
            image.rotation
            image.isFlipped

        -- Convert FlatCV corners to the expected format
        cornersList =
          [ (realToFrac transformedCorners.tlX, realToFrac transformedCorners.tlY)
          , (realToFrac transformedCorners.trX, realToFrac transformedCorners.trY)
          , (realToFrac transformedCorners.brX, realToFrac transformedCorners.brY)
          , (realToFrac transformedCorners.blX, realToFrac transformedCorners.blY)
          ]

      pure $
        cornersList
          & transToOrigAtCenter image.width image.height
          & scalePoints (1 / appState.scaleFactor)
          & P.reverse


{-| Rotate and flip detected corners from original bitmap coordinates
to match the displayed rotated/flipped image coordinates.
This is the inverse of applyRotationToCorners.
-}
rotateDetectedCorners :: Corners -> Int -> Int -> Float -> Bool -> Corners
rotateDetectedCorners srcCorners srcWidth srcHeight rotation isFlipped =
  let
    w = int2Double srcWidth
    h = int2Double srcHeight

    -- Rotate point 90° counter-clockwise (for -90° / EXIF 8)
    -- Original (W x H) becomes (H x W) after rotation
    -- Formula: (x, y) → (y, W - 1 - x) to keep within bounds
    rotatePoint90CCW :: (Double, Double) -> (Double, Double)
    rotatePoint90CCW (x, y) = (y, w - 1 - x)

    -- Rotate point 90° clockwise (for 90° / EXIF 6)
    -- Original (W x H) becomes (H x W) after rotation
    -- Formula: (x, y) → (H - 1 - y, x) to keep within bounds
    rotatePoint90CW :: (Double, Double) -> (Double, Double)
    rotatePoint90CW (x, y) = (h - 1 - y, x)

    rotatePoint180 :: (Double, Double) -> (Double, Double)
    rotatePoint180 (x, y) = (w - x, h - y)

    -- First apply rotation
    rotatedCorners = case P.round rotation :: Int of
      -- rotation=90 means image displayed with Rotate(90), i.e. 90° CW (EXIF 6)
      -- So we rotate corners 90° CW to match the display
      90 ->
        let
          (tlX, tlY) = rotatePoint90CW (srcCorners.blX, srcCorners.blY)
          (trX, trY) = rotatePoint90CW (srcCorners.tlX, srcCorners.tlY)
          (brX, brY) = rotatePoint90CW (srcCorners.trX, srcCorners.trY)
          (blX, blY) = rotatePoint90CW (srcCorners.brX, srcCorners.brY)
        in
          Corners{..}
      -- rotation=-90 means image displayed with Rotate(-90), i.e. 90° CCW (EXIF 8)
      -- So we rotate corners 90° CCW to match the display
      -90 ->
        let
          (tlX, tlY) = rotatePoint90CCW (srcCorners.trX, srcCorners.trY)
          (trX, trY) = rotatePoint90CCW (srcCorners.brX, srcCorners.brY)
          (brX, brY) = rotatePoint90CCW (srcCorners.blX, srcCorners.blY)
          (blX, blY) = rotatePoint90CCW (srcCorners.tlX, srcCorners.tlY)
        in
          Corners{..}
      180 ->
        let
          (tlX, tlY) = rotatePoint180 (srcCorners.brX, srcCorners.brY)
          (trX, trY) = rotatePoint180 (srcCorners.blX, srcCorners.blY)
          (brX, brY) = rotatePoint180 (srcCorners.tlX, srcCorners.tlY)
          (blX, blY) = rotatePoint180 (srcCorners.trX, srcCorners.trY)
        in
          Corners{..}
      -180 ->
        let
          (tlX, tlY) = rotatePoint180 (srcCorners.brX, srcCorners.brY)
          (trX, trY) = rotatePoint180 (srcCorners.blX, srcCorners.blY)
          (brX, brY) = rotatePoint180 (srcCorners.tlX, srcCorners.tlY)
          (blX, blY) = rotatePoint180 (srcCorners.trX, srcCorners.trY)
        in
          Corners{..}
      _ -> srcCorners

    -- Get the dimensions of the displayed (rotated) image for flip calculation.
    -- After 90° or -90° rotation, width and height are swapped.
    (displayW, _displayH) = case P.round rotation :: Int of
      90 -> (h, w)
      -90 -> (h, w)
      _ -> (w, h)

    -- Horizontal flip: mirror x coordinates across the center of the displayed image.
    -- For a horizontal flip, left and right swap positions:
    -- TL ↔ TR, BL ↔ BR (swapping x coordinates)
    flipHorizontal :: Corners -> Corners
    flipHorizontal c =
      let
        newTlX = displayW - 1 - c.trX
        newTrX = displayW - 1 - c.tlX
        newBrX = displayW - 1 - c.blX
        newBlX = displayW - 1 - c.brX
      in
        Corners
          { tlX = newTlX
          , tlY = c.trY
          , trX = newTrX
          , trY = c.tlY
          , brX = newBrX
          , brY = c.blY
          , blX = newBlX
          , blY = c.brY
          }
  in
    if isFlipped
      then flipHorizontal rotatedCorners
      else rotatedCorners


loadFileIntoState :: AppState -> IO AppState
loadFileIntoState appState = do
  case appState.images of
    [] -> pure appState
    image : otherImages -> do
      case image of
        ImageData{} -> do
          putText "Error: Image was already loaded"
          pure appState
        ImageToLoad filePath -> do
          pictureMetadataEither <- loadImage filePath

          case pictureMetadataEither of
            Left error -> do
              putText error
              pure appState
            Right (picture@(Bitmap bitmapData), metadata) -> do
              -- Try JuicyPixels metadata first, fall back to PNG eXIf chunk parsing
              let juicyOrientation = lookup (Exif TagOrientation) metadata
              orientationData <- case juicyOrientation of
                Just _ -> pure juicyOrientation
                Nothing -> getOrientationFromPng filePath

              let
                (rotation, isFlipped) =
                  orientationData
                    <&> imgOrientToRotAndFlip
                    & fromMaybe (0, False)
                sizeTuple = bitmapSize bitmapData
                (imgWdth, imgHgt) = case rotation of
                  90 -> swap sizeTuple
                  -90 -> swap sizeTuple
                  _ -> sizeTuple
                -- Apply rotation first, then flip horizontally if needed
                rotatedPic = Rotate rotation picture
                finalPic =
                  if isFlipped
                    then Scale (-1) 1 rotatedPic
                    else rotatedPic

              putText $
                "Loaded file "
                  <> T.pack filePath
                  <> " "
                  <> show (imgWdth, imgHgt)
                  <> " "
                  <> "with a rotation of "
                  <> show rotation
                  <> " degrees"
                  <> (if isFlipped then " (flipped)" else "")
                  <> "."

              let
                stateWithSizes =
                  calculateSizes $
                    appState
                      { currentView = ImageView
                      , images =
                          ImageData
                            { inputPath = filePath
                            , outputPath = getOutPath filePath
                            , width = imgWdth
                            , height = imgHgt
                            , widthTarget = 0 -- calculateSizes will set it
                            , heightTarget = 0 -- calculateSizes will set it
                            , rotation = rotation
                            , isFlipped = isFlipped
                            , content = finalPic
                            }
                            : otherImages
                      }

              corners <- getInitialCorners stateWithSizes bitmapData
              let stateWithCorners = stateWithSizes{corners = corners}

              pure stateWithCorners
            Right _ -> do
              putText $
                "Error: Loaded file is not a Bitmap image. "
                  <> "This error should not be possible."
              pure appState


prettyPrintArray :: (P.Show a) => a -> IO ()
prettyPrintArray =
  show
    >>> T.replace "[" "\n[ "
    >>> T.replace "]" "\n] "
    >>> T.replace "," "\n, "
    >>> P.putText


prettyPrintRecord :: (P.Show a) => a -> IO ()
prettyPrintRecord =
  show
    >>> T.replace "{" "\n{ "
    >>> T.replace "}" "\n} "
    >>> T.replace "," "\n,"
    >>> P.putText
