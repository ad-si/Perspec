import Test.Hspec (
  describe,
  expectationFailure,
  hspec,
  it,
  pendingWith,
  shouldBe,
  shouldContain,
  shouldSatisfy,
 )

import Protolude (
  Bool (False, True),
  Either (Left, Right),
  IO,
  Maybe (Just, Nothing),
  pure,
  show,
  ($),
  (&&),
  (==),
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

import FlatCV (fcv_otsu_threshold_rgba)
import Rename (getRenamingBatches)
import Types (
  RenameMode (Even, Odd, Sequential),
  SortOrder (Ascending, Descending),
 )
import Utils (loadImage)
import UtilsSpec qualified


main :: IO ()
main = hspec $ do
  describe "Perspec" $ do
    describe "Lib" $ do
      it "Applies EXIF rotation to JPEGs" $ do
        pictureMetadataEither <- loadImage "images/doc_rotated.jpg"

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
        pictureMetadataEither <- loadImage "images/rotated.png"

        case pictureMetadataEither of
          Right (Bitmap bitmapData {- metadata -}, _) -> do
            bitmapSize bitmapData `shouldBe` (1800, 1280)

            pendingWith "Needs to be implemented upstream in Juicy.Pixels first"
          -- https://github.com/Twinside/Juicy.Pixels/issues/204
          -- or in hsexif: https://github.com/emmanueltouzery/hsexif/issues/19

          _ -> expectationFailure "File should have been loaded"

      it "converts an RGBA image to binary" $ do
        pictureMetadataEither <- loadImage "./images/doc.jpg"

        _ <- pictureMetadataEither `shouldSatisfy` P.isRight

        case pictureMetadataEither of
          Left _ -> pure ()
          Right (Bitmap bitmapData, _metadata) -> do
            let
              width = P.fst bitmapData.bitmapSize
              height = P.snd bitmapData.bitmapSize
            withForeignPtr (castForeignPtr bitmapData.bitmapPointer) $
              \ptr -> do
                resutlImg <- fcv_otsu_threshold_rgba width height False ptr
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

        getRenamingBatches Nothing Sequential Ascending files
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

          getRenamingBatches Nothing Sequential Descending files
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

          getRenamingBatches (Just 2) Sequential Descending files
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
          getRenamingBatches Nothing Even Ascending files
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

          getRenamingBatches Nothing Even Descending numericFiles
            `shouldBe` batches

        it "allows explicitly setting first page number" $ do
          getRenamingBatches (Just 0) Even Ascending files
            `shouldBe` batchesStartingZero

        it "rounds to next even page number" $ do
          let batches =
                [
                  [ ("a.txt", "2.txt")
                  , ("c.txt", "4.txt")
                  , ("e.txt", "6.txt")
                  ]
                ]

          getRenamingBatches (Just 1) Even Ascending files
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

          getRenamingBatches Nothing Odd Ascending files `shouldBe` batches
          getRenamingBatches (Just 0) Odd Ascending files `shouldBe` batches
          getRenamingBatches (Just 1) Odd Ascending files `shouldBe` batches

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

          getRenamingBatches Nothing Odd Descending files `shouldBe` batches

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

          getRenamingBatches (Just 7) Odd Descending files `shouldBe` batches
          getRenamingBatches (Just 8) Odd Descending files `shouldBe` batches

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

        getRenamingBatches (Just 1) Odd Descending files `shouldBe` batches

  UtilsSpec.spec
