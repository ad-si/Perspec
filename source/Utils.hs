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
  Int,
  Maybe (Just, Nothing),
  Text,
  fmap,
  fromIntegral,
  fromMaybe,
  min,
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
  (<&>),
  (<=),
  (<>),
  (>=),
 )
import Protolude qualified as P

import Brillo (
  BitmapData (bitmapPointer, bitmapSize),
  Picture (Bitmap, Rotate),
  Point,
  color,
  greyN,
  truetypeText,
 )
import Brillo.Juicy (loadJuicyWithMetadata)
import Codec.Picture.Metadata (Keys (Exif), Metadatas, lookup)
import Codec.Picture.Metadata.Exif (ExifData (ExifShort), ExifTag (..))
import Control.Arrow ((>>>))
import Data.Text qualified as T
import Foreign.ForeignPtr (castForeignPtr, withForeignPtr)
import Foreign.Ptr (castPtr)
import GHC.Float (int2Double)
import System.FilePath (replaceBaseName, takeBaseName, takeExtension)

import FlatCV (Corners (..), fcvDetectCorners)
import Types (AppState (..), Corner, ImageData (..), View (..))


-- | Font path for TrueType text rendering
defaultFontPath :: FilePath
defaultFontPath = "/System/Library/Fonts/Supplemental/Arial.ttf"


-- | Pixel height for button text
buttonTextHeight :: Int
buttonTextHeight = 16


-- | Render text using TrueType fonts
getTextPicture :: Text -> Picture
getTextPicture txt =
  color (greyN 0.9) $
    truetypeText defaultFontPath buttonTextHeight txt


isInRect :: Point -> (Float, Float, Float, Float) -> Bool
isInRect (x, y) (x1, y1, x2, y2) =
  x >= x1 && x <= x2 && y >= y1 && y <= y2


getOutPath :: FilePath -> FilePath
getOutPath filePath = do
  let outName = takeBaseName filePath <> "-fixed"
  replaceBaseName filePath outName


calcInitWindowPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
calcInitWindowPos (screenWidth, screenHeight) (appWidth, appHeight) = do
  let
    initialX =
      ((fromIntegral screenWidth :: Float) / 2)
        - (fromIntegral appWidth / 2)
    initialY =
      ((fromIntegral screenHeight :: Float) / 2)
        - (fromIntegral appHeight / 2)

  (round initialX, round initialY)


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
              transToOrigTopLeft (-imgWidthTrgt) imgHeightTrgt $
                scalePoints (1 / scaleFactor) (getCorners appState)
          }


-- | Rotation angle in degrees of each EXIF orientation
imgOrientToRot :: ExifData -> Float
imgOrientToRot = \case
  ExifShort 1 -> 0
  ExifShort 2 -> 0 -- TODO: + horizontally flipped
  ExifShort 3 -> 180
  ExifShort 4 -> 180 -- TODO: + horizontally flipped
  ExifShort 5 -> 90 -- TODO: + horizontally flipped
  ExifShort 6 -> -90
  ExifShort 7 -> -90 -- TODO: + horizontally flipped
  ExifShort 8 -> 90
  _ -> 0


{-| Map user-selected corner coordinates
(taken from the auto-rotated display image)
back into the original un-rotated bitmap coordinate system
before computing the perspective transform.
-}
applyRotationToCorners :: Corners -> Int -> Int -> Float -> Corners
applyRotationToCorners srcCorners srcWidth srcHeight rotation =
  let
    w = int2Double srcWidth
    h = int2Double srcHeight

    rotatePoint90 :: (Double, Double) -> (Double, Double)
    rotatePoint90 (x, y) = (h - y, x)

    rotatePoint180 :: (Double, Double) -> (Double, Double)
    rotatePoint180 (x, y) = (w - x, h - y)

    rotatePoint270 :: (Double, Double) -> (Double, Double)
    rotatePoint270 (x, y) = (y, w - x)
  in
    case P.round rotation :: Int of
      90 ->
        let
          (tl_x, tl_y) = rotatePoint90 (srcCorners.bl_x, srcCorners.bl_y)
          (tr_x, tr_y) = rotatePoint90 (srcCorners.tl_x, srcCorners.tl_y)
          (br_x, br_y) = rotatePoint90 (srcCorners.tr_x, srcCorners.tr_y)
          (bl_x, bl_y) = rotatePoint90 (srcCorners.br_x, srcCorners.br_y)
        in
          Corners{..}
      -270 ->
        let
          (tl_x, tl_y) = rotatePoint90 (srcCorners.bl_x, srcCorners.bl_y)
          (tr_x, tr_y) = rotatePoint90 (srcCorners.tl_x, srcCorners.tl_y)
          (br_x, br_y) = rotatePoint90 (srcCorners.tr_x, srcCorners.tr_y)
          (bl_x, bl_y) = rotatePoint90 (srcCorners.br_x, srcCorners.br_y)
        in
          Corners{..}
      -90 ->
        let
          (tl_x, tl_y) = rotatePoint270 (srcCorners.tr_x, srcCorners.tr_y)
          (tr_x, tr_y) = rotatePoint270 (srcCorners.br_x, srcCorners.br_y)
          (br_x, br_y) = rotatePoint270 (srcCorners.bl_x, srcCorners.bl_y)
          (bl_x, bl_y) = rotatePoint270 (srcCorners.tl_x, srcCorners.tl_y)
        in
          Corners{..}
      270 ->
        let
          (tl_x, tl_y) = rotatePoint270 (srcCorners.tr_x, srcCorners.tr_y)
          (tr_x, tr_y) = rotatePoint270 (srcCorners.br_x, srcCorners.br_y)
          (br_x, br_y) = rotatePoint270 (srcCorners.bl_x, srcCorners.bl_y)
          (bl_x, bl_y) = rotatePoint270 (srcCorners.tl_x, srcCorners.tl_y)
        in
          Corners{..}
      180 ->
        let
          (tl_x, tl_y) = rotatePoint180 (srcCorners.br_x, srcCorners.br_y)
          (tr_x, tr_y) = rotatePoint180 (srcCorners.bl_x, srcCorners.bl_y)
          (br_x, br_y) = rotatePoint180 (srcCorners.tl_x, srcCorners.tl_y)
          (bl_x, bl_y) = rotatePoint180 (srcCorners.tr_x, srcCorners.tr_y)
        in
          Corners{..}
      -180 ->
        let
          (tl_x, tl_y) = rotatePoint180 (srcCorners.br_x, srcCorners.br_y)
          (tr_x, tr_y) = rotatePoint180 (srcCorners.bl_x, srcCorners.bl_y)
          (br_x, br_y) = rotatePoint180 (srcCorners.tl_x, srcCorners.tl_y)
          (bl_x, bl_y) = rotatePoint180 (srcCorners.tr_x, srcCorners.tr_y)
        in
          Corners{..}
      _ -> srcCorners


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
      if P.elem fileExtension allowedExtensions
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
        fcvDetectCorners (castPtr ptr) rawWidth rawHeight

      -- Convert FlatCV corners to the expected format
      let
        cornersList =
          [ (realToFrac detectedCorners.tl_x, realToFrac detectedCorners.tl_y)
          , (realToFrac detectedCorners.tr_x, realToFrac detectedCorners.tr_y)
          , (realToFrac detectedCorners.br_x, realToFrac detectedCorners.br_y)
          , (realToFrac detectedCorners.bl_x, realToFrac detectedCorners.bl_y)
          ]

      pure $
        cornersList
          & transToOrigAtCenter image.width image.height
          & scalePoints (1 / appState.scaleFactor)
          & P.reverse


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
              let
                rotation =
                  lookup (Exif TagOrientation) metadata
                    <&> imgOrientToRot
                    & fromMaybe 0
                sizeTuple = bitmapSize bitmapData
                (imgWdth, imgHgt) = case rotation of
                  90 -> swap sizeTuple
                  -90 -> swap sizeTuple
                  _ -> sizeTuple

              putText $
                "Loaded file "
                  <> T.pack filePath
                  <> " "
                  <> show (imgWdth, imgHgt)
                  <> " "
                  <> "with a rotation of "
                  <> show rotation
                  <> " degrees."

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
                            , content = Rotate (-rotation) picture
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
