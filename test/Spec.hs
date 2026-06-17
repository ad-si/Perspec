import Test.Hspec (
  describe,
  expectationFailure,
  hspec,
  it,
  shouldBe,
  shouldContain,
  shouldSatisfy,
 )

import Protolude (
  Bool (False, True),
  Either (Left, Right),
  IO,
  Maybe (Just, Nothing),
  Semigroup ((<>)),
  pure,
  show,
  ($),
  (&&),
  (==),
  (>),
  (||),
 )
import Protolude qualified as P

import Foreign (castForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Ptr (castPtr)

import Brillo (
  BitmapFormat (BitmapFormat),
  Picture (Bitmap),
  PixelFormat (PxRGBA),
  RowOrder (TopToBottom),
 )
import Brillo.Rendering (BitmapData (..), bitmapOfForeignPtr)

import Codec.Picture (
  DynamicImage (ImageRGBA8),
  PixelRGBA8 (..),
  convertRGBA8,
  generateImage,
  imageHeight,
  imageWidth,
  readImage,
 )
import Codec.Picture.Metadata.Exif (ExifData (ExifShort))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import FlatCV (otsuThresholdPtr)
import PngExif (
  clearExifOrientation,
  extractExifBytesFromFile,
  extractExifBytesFromPng,
  extractExifFromPng,
  getExifOrientationFromPng,
  writePngWithExif,
 )
import Rename (getRenamingBatches)
import Rotate (rotatePages, rotationForNumber)
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  getTemporaryDirectory,
  removePathForcibly,
 )
import System.FilePath ((</>))
import Types (
  RenameMode (Even, Odd, Sequential),
  RotationDirection (Clockwise, CounterClockwise),
  SortOrder (Ascending, Descending),
 )
import Utils (loadImage)
import UtilsSpec qualified


main :: IO ()
main = hspec $ do
  describe "Perspec" $ do
    describe "Lib" $ do
      it "Applies EXIF rotation to JPEGs" $ do
        pictureMetadataEither <- loadImage "images/examples/doc_rotated.jpg"

        case pictureMetadataEither of
          Right (Bitmap bitmapData, metadata) -> do
            bitmapSize bitmapData `shouldBe` (880, 1500)

            -- Does not provide an Eq instance => Misuse show
            let metadataText = show metadata

            metadataText `shouldContain` "TagOrientation :=> ExifShort 6"
            metadataText `shouldContain` "(TagUnknown 40962) :=> ExifLong 880"
            metadataText `shouldContain` "(TagUnknown 40963) :=> ExifLong 1500"
          _ -> expectationFailure "File should have been loaded"

      it "Applies EXIF rotation to PNGs" $ do
        -- Test that our PngExif module can extract orientation from PNG eXIf chunk
        -- (JuicyPixels doesn't support this: https://github.com/Twinside/Juicy.Pixels/issues/204)
        orientation <- getExifOrientationFromPng "images/rotated.png"
        orientation `shouldBe` Just (ExifShort 6)

        -- Also verify the image loads correctly
        pictureMetadataEither <- loadImage "images/rotated.png"
        case pictureMetadataEither of
          Right (Bitmap bitmapData, _) -> do
            bitmapSize bitmapData `shouldBe` (1800, 1280)
          _ -> expectationFailure "File should have been loaded"

      it "extracts raw EXIF bytes from JPEG files" $ do
        exifBytes <- extractExifBytesFromFile "images/examples/doc_rotated.jpg"
        case exifBytes of
          Just bytes -> do
            -- EXIF data should start with byte order marker "MM" (big-endian) or "II" (little-endian)
            let firstTwoBytes = BS.take 2 bytes
            (firstTwoBytes == "MM" || firstTwoBytes == "II") `shouldBe` True
            -- Should have reasonable size (at least TIFF header + some IFD entries)
            BS.length bytes `shouldSatisfy` (> 8)
          Nothing -> expectationFailure "Should have extracted EXIF bytes from JPEG"

      it "extracts raw EXIF bytes from PNG eXIf chunk" $ do
        exifBytes <- extractExifBytesFromFile "images/rotated.png"
        case exifBytes of
          Just bytes -> do
            -- EXIF data should start with byte order marker
            let firstTwoBytes = BS.take 2 bytes
            (firstTwoBytes == "MM" || firstTwoBytes == "II") `shouldBe` True
          Nothing -> expectationFailure "Should have extracted EXIF bytes from PNG"

      it "resets EXIF orientation to 1 via clearExifOrientation" $ do
        exifBytes <- extractExifBytesFromFile "images/rotated.png"
        case exifBytes of
          Just bytes -> do
            let cleared = clearExifOrientation bytes
            -- Same length (value is patched in place, not removed)
            BS.length cleared `shouldBe` BS.length bytes
            -- Wrap as minimal PNG-like chunk to reuse extractExifFromPng is
            -- overkill; instead validate via raw re-parse using ExifFromPng:
            -- Write a PNG with the cleared bytes and re-read orientation
            let testImage =
                  ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 255) 2 2
            case writePngWithExif (Just cleared) testImage of
              Left err -> expectationFailure $ "writePngWithExif: " <> err
              Right pngBytes ->
                extractExifFromPng (BL.toStrict pngBytes)
                  `shouldBe` Just (ExifShort 1)
          Nothing -> expectationFailure "Should have extracted EXIF bytes"

      it "preserves EXIF data when writing PNG with eXIf chunk" $ do
        -- Extract EXIF from source JPEG
        exifBytes <- extractExifBytesFromFile "images/examples/doc_rotated.jpg"
        exifBytes `shouldSatisfy` P.isJust

        -- Create a simple test image
        let testImage = ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 255 0 0 255) 10 10

        -- Write PNG with EXIF
        case writePngWithExif exifBytes testImage of
          Left err -> expectationFailure $ "Failed to write PNG: " <> err
          Right pngBytes -> do
            -- Read back the EXIF from the generated PNG
            let pngStrict = BL.toStrict pngBytes
            let extractedExif = extractExifBytesFromPng pngStrict
            extractedExif `shouldBe` exifBytes

            -- Verify we can parse orientation from the written PNG
            let orientation = extractExifFromPng pngStrict
            orientation `shouldBe` Just (ExifShort 6)

      it "converts an RGBA image to binary" $ do
        pictureMetadataEither <- loadImage "./images/examples/doc.jpg"

        _ <- pictureMetadataEither `shouldSatisfy` P.isRight

        case pictureMetadataEither of
          Left _ -> pure ()
          Right (Bitmap bitmapData, _metadata) -> do
            let
              width = P.fst bitmapData.bitmapSize
              height = P.snd bitmapData.bitmapSize
            withForeignPtr (castForeignPtr bitmapData.bitmapPointer) $
              \ptr -> do
                resutlImg <-
                  otsuThresholdPtr (P.fromIntegral width) (P.fromIntegral height) False ptr
                resultImgForeignPtr <- newForeignPtr_ (castPtr resutlImg)
                let binaryPic =
                      bitmapOfForeignPtr
                        width
                        height
                        (BitmapFormat TopToBottom PxRGBA)
                        resultImgForeignPtr
                        True

                binaryPic
                  `shouldSatisfy` \case
                    Bitmap bmpData ->
                      P.fst bmpData.bitmapSize == width
                        && P.snd bmpData.bitmapSize == height
                    _ -> False
          Right _ ->
            P.putText "Unsupported image format"

    describe "Rename" $ do
      it "renames files according to natural sort and avoids collisions" $ do
        let
          files = ["1.txt", "10.txt", "2.txt"]
          batches =
            [
              [ ("1.txt", "0.txt")
              , ("2.txt", "_perspec_temp_1.txt")
              , ("10.txt", "_perspec_temp_2.txt")
              ]
            ,
              [ ("_perspec_temp_1.txt", "1.txt")
              , ("_perspec_temp_2.txt", "2.txt")
              ]
            ]

        getRenamingBatches Nothing 0 Sequential Ascending files
          `shouldBe` batches

      describe "Renaming files in descending order" $ do
        it "automatically sets first page number" $ do
          let
            files = ["1.txt", "10.txt", "2.txt"]
            batches =
              [
                [ ("1.txt", "_perspec_temp_2.txt")
                , ("2.txt", "_perspec_temp_1.txt")
                , ("10.txt", "0.txt")
                ]
              ,
                [ ("_perspec_temp_2.txt", "2.txt")
                , ("_perspec_temp_1.txt", "1.txt")
                ]
              ]

          getRenamingBatches Nothing 0 Sequential Descending files
            `shouldBe` batches

        it "allows explicitly setting first page number" $ do
          let
            files = ["1.txt", "10.txt", "2.txt"]
            batches =
              [
                [ ("1.txt", "_perspec_temp_2.txt")
                , ("2.txt", "_perspec_temp_1.txt")
                , ("10.txt", "0.txt")
                ]
              ,
                [ ("_perspec_temp_2.txt", "2.txt")
                , ("_perspec_temp_1.txt", "1.txt")
                ]
              ]

          getRenamingBatches (Just 2) 0 Sequential Descending files
            `shouldBe` batches

      describe "Renaming files with even page numbers" $ do
        let
          files = ["a.txt", "c.txt", "e.txt"]
          batchesStartingZero =
            [
              [ ("a.txt", "0.txt")
              , ("c.txt", "2.txt")
              , ("e.txt", "4.txt")
              ]
            ]

        it "automatically sets first page number" $ do
          getRenamingBatches Nothing 0 Even Ascending files
            `shouldBe` batchesStartingZero

        it "automatically sets first page number with descending order" $ do
          let
            numericFiles = ["8.txt", "10.txt", "9.txt"]
            batches =
              [
                [ ("8.txt", "4.txt")
                , ("9.txt", "2.txt")
                , ("10.txt", "0.txt")
                ]
              ]

          getRenamingBatches Nothing 0 Even Descending numericFiles
            `shouldBe` batches

        it "allows explicitly setting first page number" $ do
          getRenamingBatches (Just 0) 0 Even Ascending files
            `shouldBe` batchesStartingZero

        it "rounds to next even page number" $ do
          let batches =
                [
                  [ ("a.txt", "2.txt")
                  , ("c.txt", "4.txt")
                  , ("e.txt", "6.txt")
                  ]
                ]

          getRenamingBatches (Just 1) 0 Even Ascending files
            `shouldBe` batches

      describe "Renaming files with odd page numbers" $ do
        it "correctly sets first page number" $ do
          let
            files = ["b.txt", "d.txt", "f.txt"]
            batches =
              [
                [ ("b.txt", "1.txt")
                , ("d.txt", "3.txt")
                , ("f.txt", "5.txt")
                ]
              ]

          getRenamingBatches Nothing 0 Odd Ascending files `shouldBe` batches
          getRenamingBatches (Just 0) 0 Odd Ascending files `shouldBe` batches
          getRenamingBatches (Just 1) 0 Odd Ascending files `shouldBe` batches

        it "works with descending order and automatically sets page number" $ do
          let
            files = ["8.txt", "10.txt", "9.txt"]
            batches =
              [
                [ ("8.txt", "5.txt")
                , ("9.txt", "3.txt")
                , ("10.txt", "1.txt")
                ]
              ]

          getRenamingBatches Nothing 0 Odd Descending files `shouldBe` batches

        it "works with descending order and explicit page number" $ do
          let
            files = ["8.txt", "10.txt", "9.txt"]
            batches =
              [
                [ ("8.txt", "7.txt")
                , ("9.txt", "5.txt")
                , ("10.txt", "3.txt")
                ]
              ]

          getRenamingBatches (Just 7) 0 Odd Descending files `shouldBe` batches
          getRenamingBatches (Just 8) 0 Odd Descending files `shouldBe` batches

      it "prefixes pages with negative numbers with \"_todo_\"" $ do
        let
          files = ["8.txt", "10.txt", "9.txt"]
          batches =
            [
              [ ("8.txt", "1.txt")
              , ("9.txt", "_todo_-1.txt")
              , ("10.txt", "_todo_-3.txt")
              ]
            ]

        getRenamingBatches (Just 1) 0 Odd Descending files `shouldBe` batches

      describe "Padded output filenames" $ do
        it "pads output numbers to the given width" $ do
          let
            files = ["a.txt", "b.txt", "c.txt"]
            batches =
              [
                [ ("a.txt", "01.txt")
                , ("b.txt", "02.txt")
                , ("c.txt", "03.txt")
                ]
              ]

          getRenamingBatches (Just 1) 2 Sequential Ascending files
            `shouldBe` batches

        it "leaves wider numbers untouched when they exceed the width" $ do
          let
            files = ["a.txt", "b.txt", "c.txt"]
            batches =
              [
                [ ("a.txt", "09.txt")
                , ("b.txt", "10.txt")
                , ("c.txt", "11.txt")
                ]
              ]

          getRenamingBatches (Just 9) 2 Sequential Ascending files
            `shouldBe` batches

        it "pads negative _todo_ pages after the dash" $ do
          let
            files = ["8.txt", "10.txt", "9.txt"]
            batches =
              [
                [ ("8.txt", "01.txt")
                , ("9.txt", "_todo_-01.txt")
                , ("10.txt", "_todo_-03.txt")
                ]
              ]

          getRenamingBatches (Just 1) 2 Odd Descending files
            `shouldBe` batches

    describe "Rotate" $ do
      it "rotates even pages clockwise and odd pages counter-clockwise" $ do
        rotationForNumber 0 `shouldBe` Clockwise
        rotationForNumber 1 `shouldBe` CounterClockwise
        rotationForNumber 2 `shouldBe` Clockwise
        rotationForNumber 11 `shouldBe` CounterClockwise

      it "bakes EXIF orientation into pixels and clears the tag" $ do
        -- rotated.png stores raw 1800×1280 pixels with EXIF orientation 6,
        -- so it displays as an upright 1280×1800 portrait.
        tmpDir <- getTemporaryDirectory
        let workDir = tmpDir </> "perspec-rotate-test"
        removePathForcibly workDir
        createDirectoryIfMissing True workDir
        copyFile "images/rotated.png" (workDir </> "1.png")

        rotatePages workDir

        let outPath = workDir </> "1.png"
        -- Page 1 is odd, so the upright 1280×1800 image is rotated 90° CCW,
        -- yielding raw 1800×1280 pixels (not the orientation-blind 1280×1800).
        rotatedEither <- readImage outPath
        case rotatedEither of
          Left err -> expectationFailure err
          Right dynImage -> do
            let rgba = convertRGBA8 dynImage
            (imageWidth rgba, imageHeight rgba) `shouldBe` (1800, 1280)

        -- The output must not carry an orientation tag anymore.
        orientation <- getExifOrientationFromPng outPath
        orientation `shouldBe` Nothing

        removePathForcibly workDir

  UtilsSpec.spec
