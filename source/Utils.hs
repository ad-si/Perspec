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
  (==),
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
import Control.Arrow ((>>>))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TSE
import System.Directory (getCurrentDirectory)
import System.FilePath (replaceBaseName, takeBaseName, takeExtension, (</>))
import System.Info (os)
import System.Process (readProcessWithExitCode)

import Brillo.Data.Picture (Picture (Scale))
import Types (AppState (..), Coordinate (..), Corner, ImageData (..), View (..))


-- | Embed the words sprite image with a scale factor of 2
wordsSprite :: (ByteString, Float)
wordsSprite = ($(embedFile "images/words@2x.png"), 2)


wordsPic :: Picture
wordsPic =
  fromMaybe mempty $
    either (const Nothing) Just (decodePng $ P.fst wordsSprite)
      >>= fromDynamicImage


{-| `rectPos` is the position of the content
`rectSize` ist the size of the content
-}
getWordSprite :: Text -> Picture
getWordSprite spriteText = do
  let
    scaleFactor = 1 / P.snd wordsSprite
    scaleVal = fromIntegral >>> (* P.snd wordsSprite) >>> round
    scaleRect rect =
      Rectangle
        { rectPos = bimap scaleVal scaleVal rect.rectPos
        , rectSize = bimap scaleVal scaleVal rect.rectSize
        }
  case wordsPic of
    Bitmap bitmapData ->
      Scale scaleFactor scaleFactor $ case spriteText of
        "Save" ->
          BitmapSection
            (scaleRect Rectangle{rectPos = (0, 40), rectSize = (40, 20)})
            bitmapData
        "Save BW" ->
          BitmapSection
            (scaleRect Rectangle{rectPos = (0, 60), rectSize = (74, 20)})
            bitmapData
        "Save Gray" ->
          BitmapSection
            (scaleRect Rectangle{rectPos = (0, 80), rectSize = (84, 20)})
            bitmapData
        "Select Files" ->
          BitmapSection
            (scaleRect Rectangle{rectPos = (0, 140), rectSize = (92, 20)})
            bitmapData
        "Save BW Smooth" ->
          BitmapSection
            (scaleRect Rectangle{rectPos = (0, 160), rectSize = (140, 20)})
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
    image : otherImages -> do
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


-- | Get initial corner positions by shelling out to a Python script
getInitialCorners :: AppState -> FilePath -> IO [Corner]
getInitialCorners appState inPath = do
  case appState.images of
    [] -> pure []
    image : _otherImages -> do
      currentDir <- getCurrentDirectory

      let
        wdthFrac = fromIntegral image.width
        hgtFrac = fromIntegral image.height

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
              & transToOrigAtCenter image.width image.height
              & scalePoints (1 / appState.scaleFactor)
              & P.reverse
        else do
          P.putErrLn stderr

          let
            -- Initial distance of the corners from the image border
            distance = 0.1

          pure $
            transToOrigTopLeft (-image.widthTarget) image.heightTarget $
              scalePoints (1 / appState.scaleFactor) $
                P.reverse
                  [ (wdthFrac * distance, hgtFrac * distance)
                  , (wdthFrac * (1 - distance), hgtFrac * distance)
                  , (wdthFrac * (1 - distance), hgtFrac * (1 - distance))
                  , (wdthFrac * distance, hgtFrac * (1 - distance))
                  ]


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

              corners <- getInitialCorners stateWithSizes filePath
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
