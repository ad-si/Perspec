module Rotate where

import Protolude (
  Either (Left, Right),
  FilePath,
  IO,
  Int,
  Maybe (Just),
  Semigroup ((<>)),
  die,
  even,
  fromIntegral,
  null,
  pure,
  putErrText,
  putText,
  when,
  zip,
  ($),
  (&),
  (==),
 )
import Protolude qualified as P

import Codec.Picture (
  Image (Image, imageData, imageHeight, imageWidth),
  PixelRGBA8,
  convertRGBA8,
  readImage,
  writePng,
 )
import Codec.Picture.Metadata.Exif (ExifData (ExifShort))
import Data.List (sort)
import Data.Text (pack)
import Data.Vector.Storable (Vector)
import Data.Word (Word32, Word8)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import FlatCV qualified as FCV
import PngExif (getOrientationFromPng)
import Types (RotationDirection (Clockwise, CounterClockwise))


{-| The rotation direction for a page number.
Odd page numbers rotate counter-clockwise, even page numbers clockwise,
matching how the pages of a book are oriented when photographed face-up.
-}
rotationForNumber :: Int -> RotationDirection
rotationForNumber number =
  if even number
    then Clockwise
    else CounterClockwise


{-| Bake an EXIF orientation into the RGBA pixel buffer so the stored pixels
match what an EXIF-aware viewer would display.
Returns the upright width, height, and buffer.

PNG eXIf orientation support isn't universal among viewers, so the rotate
sub-command normalizes the pixels here and writes the output without an
orientation tag, instead of relying on the tag to be honored.

FlatCV's primitives map directly onto the eight EXIF orientations.
-}
bakeOrientation ::
  Maybe ExifData ->
  Word32 ->
  Word32 ->
  Vector Word8 ->
  IO (Word32, Word32, Vector Word8)
bakeOrientation orientation width height buffer =
  let keep buf = pure (width, height, buf)
      swap buf = pure (height, width, buf)
  in  case orientation of
        Just (ExifShort 2) -> FCV.flipX width height buffer P.>>= keep
        Just (ExifShort 3) -> FCV.rotate180 width height buffer P.>>= keep
        Just (ExifShort 4) -> FCV.flipY width height buffer P.>>= keep
        Just (ExifShort 5) -> FCV.transpose width height buffer P.>>= swap
        Just (ExifShort 6) -> FCV.rotate90CW width height buffer P.>>= swap
        Just (ExifShort 7) -> FCV.transverse width height buffer P.>>= swap
        Just (ExifShort 8) -> FCV.rotate270CW width height buffer P.>>= swap
        _ -> keep buffer


{-| Rotate the PNGs in a directory in place, treating the alphabetically
sorted list of files as the page sequence: the first file is page 1, the
second page 2, and so on. Odd pages are rotated 90° counter-clockwise,
even pages 90° clockwise.

Any existing EXIF orientation is first baked into the pixels, then the page
rotation is applied to the pixels, and the result is written without an
orientation tag so it displays correctly regardless of EXIF support.
Exits with an error if the directory contains no PNGs.
-}
rotatePages :: FilePath -> IO ()
rotatePages directory = do
  allFiles <- listDirectory directory

  let
    pngFiles =
      allFiles
        & P.filter (\file -> takeExtension file == ".png")
        & sort

  when (null pngFiles) $
    die "No PNG files found"

  zip [1 :: Int ..] pngFiles
    & P.mapM_
      ( \(number, file) -> do
          let path = directory </> file

          dynImageEither <- readImage path

          case dynImageEither of
            Left err ->
              putErrText $ "Skip " <> pack file <> ": " <> pack err
            Right dynImage -> do
              orientation <- getOrientationFromPng path

              let
                rgba = convertRGBA8 dynImage
                rawWidth = fromIntegral (imageWidth rgba)
                rawHeight = fromIntegral (imageHeight rgba)

              (uprightWidth, uprightHeight, uprightBuffer) <-
                bakeOrientation orientation rawWidth rawHeight (imageData rgba)

              let
                (label, rotate) =
                  case rotationForNumber number of
                    Clockwise -> ("clockwise", FCV.rotate90CW)
                    CounterClockwise -> ("counter-clockwise", FCV.rotate270CW)

              putText $ "Rotate " <> pack file <> " 90° " <> label

              -- Both rotations swap the dimensions (w × h becomes h × w).
              rotated <- rotate uprightWidth uprightHeight uprightBuffer

              let
                outImage :: Image PixelRGBA8
                outImage =
                  Image
                    (fromIntegral uprightHeight)
                    (fromIntegral uprightWidth)
                    rotated

              writePng path outImage
      )
