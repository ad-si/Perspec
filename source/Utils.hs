{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
module Utils where

import Protolude (
  Bool (..),
  ByteString,
  Either (..),
  FilePath,
  Float,
  IO,
  Int,
  Maybe (Just, Nothing),
  Monad ((>>=)),
  Monoid (mempty),
  Text,
  const,
  either,
  fmap,
  fromIntegral,
  fromMaybe,
  min,
  pure,
  putText,
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
  BitmapData (bitmapSize),
  Picture (Bitmap, BitmapSection, Rotate),
  Point,
  Rectangle (Rectangle, rectPos, rectSize),
 )
import Brillo.Juicy (fromDynamicImage, loadJuicyWithMetadata)
import Codec.Picture (decodePng)
import Codec.Picture.Metadata (Keys (Exif), Metadatas, lookup)
import Codec.Picture.Metadata.Exif (ExifData (ExifShort), ExifTag (..))
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import System.FilePath (replaceBaseName, takeBaseName, takeExtension)
import Types (AppState (..), View (..))


wordsSprite :: ByteString
wordsSprite = $(embedFile "images/words.png")


wordsPic :: Picture
wordsPic =
  fromMaybe
    mempty
    ( either (const Nothing) Just (decodePng wordsSprite)
        >>= fromDynamicImage
    )


getWordSprite :: Text -> Picture
getWordSprite spriteText =
  case wordsPic of
    Bitmap bitmapData -> case spriteText of
      "Save" ->
        BitmapSection
          Rectangle{rectPos = (0, 40), rectSize = (90, 20)}
          bitmapData
      "Save BW" ->
        BitmapSection
          Rectangle{rectPos = (0, 60), rectSize = (90, 20)}
          bitmapData
      "Save Gray" ->
        BitmapSection
          Rectangle{rectPos = (0, 80), rectSize = (90, 20)}
          bitmapData
      "Select Files" ->
        BitmapSection
          Rectangle{rectPos = (0, 140), rectSize = (100, 20)}
          bitmapData
      _ -> mempty
    _ -> mempty


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
originTopLeft :: Int -> Int -> [Point] -> [Point]
originTopLeft width height =
  fmap
    ( \(x, y) ->
        ( x + (fromIntegral width / 2.0)
        , -(y - (fromIntegral height / 2.0))
        )
    )


scalePoints :: Float -> [Point] -> [Point]
scalePoints scaleFac = fmap $
  \(x, y) -> (x / scaleFac, y / scaleFac)


getCorners :: AppState -> [Point]
getCorners appState =
  scalePoints appState.scaleFactor $
    originTopLeft
      appState.imgWidthTrgt
      appState.imgHeightTrgt
      (P.reverse $ corners appState)


calculateSizes :: AppState -> AppState
calculateSizes appState =
  let
    imgViewWidth = appState.appWidth - appState.sidebarWidth
    imgViewHeight = appState.appHeight

    imgWidthFrac = fromIntegral appState.imgWidthOrig
    imgHeightFrac = fromIntegral appState.imgHeightOrig

    scaleFactorX = fromIntegral imgViewWidth / imgWidthFrac
    scaleFactorY = fromIntegral imgViewHeight / imgHeightFrac

    scaleFactor = min scaleFactorX scaleFactorY
    imgWidthTrgt = round $ scaleFactor * imgWidthFrac
    imgHeightTrgt = round $ scaleFactor * imgHeightFrac
  in
    appState
      { imgWidthTrgt
      , imgHeightTrgt
      , scaleFactor
      , corners =
          originTopLeft (-imgWidthTrgt) imgHeightTrgt $
            scalePoints (1 / scaleFactor) (getCorners appState)
      }


imgOrientToRot :: ExifData -> Float
imgOrientToRot = \case
  ExifShort 6 -> -90
  ExifShort 1 -> 0
  ExifShort 8 -> 90
  ExifShort 3 -> 180
  -- TODO: Also apply mirroring to image
  ExifShort 5 -> -90
  ExifShort 2 -> 0
  ExifShort 7 -> 90
  ExifShort 4 -> 180
  _ -> 0


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


loadFileIntoState :: AppState -> FilePath -> IO AppState
loadFileIntoState appState filePath = do
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

        stateWithSizes =
          calculateSizes $
            appState
              { currentView = ImageView
              , imgWidthOrig = imgWdth
              , imgHeightOrig = imgHgt
              , rotation = rotation
              , image = Rotate (-rotation) picture
              , inputPath = Just filePath
              , outputPath = Just $ getOutPath filePath
              }

      putText $
        "Loaded file " <> T.pack filePath <> " " <> show (imgWdth, imgHgt)
      putText $
        "with a rotation of " <> show rotation <> " degrees."

      pure stateWithSizes
    Right _ -> do
      putText $
        "Error: Loaded file is not a Bitmap image. "
          <> "This error should not be possible."
      pure appState
