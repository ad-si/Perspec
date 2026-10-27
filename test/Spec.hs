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
  (+),
  (-),
  (/),
  (<),
  (<&>),
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
  Point,
  RowOrder (TopToBottom),
 )
import Brillo.Interface.IO.Game (
  Event (EventDrop, EventKey, EventMotion, EventResize),
  Key (Char, MouseButton),
  KeyState (Down, Up),
  Modifiers (..),
  MouseButton (LeftButton, RightButton),
 )
import Brillo.Interface.IO.Interact (Controller (..))
import Brillo.Rendering (BitmapData (..), bitmapOfForeignPtr)
import Data.IORef (newIORef)

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
import Data.List.NonEmpty (NonEmpty ((:|)))
import FlatCV (otsuThresholdPtr)
import Lib (
  LoadedImage (..),
  PlacedComponent (..),
  applyLoadedImage,
  completedPdfDocument,
  defaultPdfAction,
  findUiTargetAt,
  handleEvent,
  imagePath,
  isValidLicenseKey,
  pdfActionText,
  pdfDocumentForImage,
  placeComponents,
  queueImages,
  sha256Hex,
  toLogicalEvent,
  zoomLevelText,
 )
import Ocr (recognizeWords)
import Pdf (writeSearchablePdf)
import PngExif (
  clearExifOrientation,
  extractExifBytesFromFile,
  extractExifBytesFromPng,
  extractExifFromPng,
  extractXmpOrientationFromPng,
  getOrientationFromPng,
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
import System.IO.Temp (withSystemTempDirectory)
import Types (
  AppState (..),
  ExportSettings (..),
  ExportStyle (..),
  ImageData (..),
  OcrLanguage (..),
  OcrWord (..),
  OutputFormat (..),
  PdfAction (..),
  PdfDocument (..),
  PdfPage (..),
  RenameMode (Even, Odd, Sequential),
  RotationDirection (Clockwise, CounterClockwise),
  SortOrder (Ascending, Descending),
  UiTarget (..),
  ZoomControl (..),
  initialState,
 )
import Utils (
  getCorners,
  imageViewToImage,
  imagesToLoad,
  loadFileIntoState,
  loadImage,
  maxZoom,
  minZoom,
  panImageView,
  panOffsetInView,
  resetImageView,
  zoomImageView,
 )
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
        orientation <- getOrientationFromPng "images/rotated.png"
        orientation `shouldBe` Just (ExifShort 6)

      it "reads orientation from a compressed XMP raw profile (attribute form)" $ do
        -- ImageMagick stores XMP as a zlib-compressed "Raw profile type xmp"
        -- zTXt chunk with tiff:Orientation as an XML attribute. `-auto-orient`
        -- ignores this, which is why these files export sideways.
        orientation <- getOrientationFromPng "images/xmp_rotated.png"
        orientation `shouldBe` Just (ExifShort 6)

      it "reads orientation from an uncompressed XMP iTXt (element form)" $ do
        orientation <- getOrientationFromPng "images/xmp_itxt_rotated.png"
        orientation `shouldBe` Just (ExifShort 6)

      it "extractXmpOrientationFromPng returns Nothing when no XMP is present" $ do
        pngBytes <- BS.readFile "images/no_xmp.png"
        extractXmpOrientationFromPng pngBytes `shouldBe` Nothing

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

      describe "OCR" $ do
        it "recognizes the words of a document" $ do
          imageEither <- readImage "images/examples/package_slip_bw.png"
          case imageEither of
            Left err -> expectationFailure err
            Right dynImage -> do
              wordsEither <- recognizeWords German (convertRGBA8 dynImage)
              case wordsEither of
                Left err -> expectationFailure $ P.toS err
                Right ocrWords -> do
                  (ocrWords <&> (.text)) `shouldContain` ["Max", "Mustermann"]
                  (ocrWords <&> (.text)) `shouldContain` ["Musterstraße", "12"]
                  ocrWords
                    `shouldSatisfy` P.all
                      ( \word ->
                          word.left P.< word.right
                            && word.top P.< word.bottom
                            && word.right P.<= 1256
                            && word.bottom P.<= 887
                      )

        it "recognizes Spanish text" $ do
          imageEither <- readImage "images/spanish_text.png"
          case imageEither of
            Left err -> expectationFailure err
            Right dynImage -> do
              wordsEither <- recognizeWords Spanish (convertRGBA8 dynImage)
              case wordsEither of
                Left err -> expectationFailure $ P.toS err
                Right ocrWords ->
                  (ocrWords <&> (.text))
                    `shouldBe` [ "El"
                               , "niño"
                               , "comió"
                               , "una"
                               , "piña"
                               , "en"
                               , "el"
                               , "jardín."
                               , "¿Dónde"
                               , "está"
                               , "la"
                               , "estación"
                               , "de"
                               , "autobús?"
                               ]

        it "writes a PDF with the image and the recognized text" $ do
          imageEither <- readImage "images/examples/package_slip_bw.png"
          case imageEither of
            Left err -> expectationFailure err
            Right dynImage -> do
              let image = convertRGBA8 dynImage
              wordsEither <- recognizeWords German image
              case wordsEither of
                Left err -> expectationFailure $ P.toS err
                Right ocrWords ->
                  withSystemTempDirectory "perspec-pdf" $ \tempDir -> do
                    let pdfPath = tempDir </> "package_slip.pdf"
                    writeResult <-
                      writeSearchablePdf pdfPath (PdfPage{image, ocrWords} :| [])
                    writeResult `shouldBe` Right ()
                    pdfBytes <- BS.readFile pdfPath
                    pdfBytes `shouldSatisfy` BS.isPrefixOf "%PDF-"
                    pdfBytes `shouldSatisfy` BS.isInfixOf "/Helvetica"
                    pdfBytes `shouldSatisfy` BS.isInfixOf "/Width 1256"

        it "writes a PDF with a page of its own size for each image" $ do
          let
            whiteImage width height =
              generateImage (\_ _ -> PixelRGBA8 255 255 255 255) width height
            pages =
              PdfPage{image = whiteImage 300 600, ocrWords = []}
                :| [PdfPage{image = whiteImage 600 300, ocrWords = []}]
          withSystemTempDirectory "perspec-pdf" $ \tempDir -> do
            let pdfPath = tempDir </> "two_pages.pdf"
            writeResult <- writeSearchablePdf pdfPath pages
            writeResult `shouldBe` Right ()
            pdfBytes <- BS.readFile pdfPath
            pdfBytes `shouldSatisfy` BS.isInfixOf "/Count 2"
            pdfBytes `shouldSatisfy` BS.isInfixOf "/Width 300"
            pdfBytes `shouldSatisfy` BS.isInfixOf "/Width 600"

      describe "placeComponents" $ do
        it "gives labels and separators smaller slots" $ do
          (placeComponents initialState.uiComponents <&> (.topOffset))
            `shouldBe` [50, 82, 122, 154, 194, 226, 266, 286, 326]

      describe "findUiTargetAt" $ do
        -- The sidebar of the initial state spans x from 380 to 640
        -- and its components x from 390 to 630
        let
          pdfState =
            initialState
              { exportSettings =
                  initialState.exportSettings{outputFormat = PdfFormat}
              }
          pdfStateWithDocument = pdfState{pdfDocument = Just firstPdf}

        it "finds the segments of the style select" $ do
          findUiTargetAt initialState (395, 383)
            `shouldBe` Just (StyleOption ColorStyle)
          findUiTargetAt initialState (625, 383)
            `shouldBe` Just (StyleOption BlackWhiteSmoothStyle)

        it "finds the segments of the output format select" $ do
          findUiTargetAt initialState (395, 311)
            `shouldBe` Just (OutputFormatOption ImageFormat)
          findUiTargetAt initialState (625, 311)
            `shouldBe` Just (OutputFormatOption PdfFormat)

        it "disables the language select for the image output" $ do
          let ocrSelectPoint = (510, 239)
          findUiTargetAt initialState ocrSelectPoint
            `shouldBe` Nothing
          findUiTargetAt pdfState ocrSelectPoint
            `shouldBe` Just OcrLanguageSelectTarget

        it "finds the language options only when the select is open" $ do
          -- Center of the second option below the select
          let germanOptionPoint = (510, 179)
          findUiTargetAt pdfState germanOptionPoint
            `shouldBe` Just (PdfActionTarget NewPdf)
          findUiTargetAt
            pdfState{ocrLanguageSelectIsOpen = True}
            germanOptionPoint
            `shouldBe` Just (OcrLanguageOption German)

        it "finds the save button below the separator" $ do
          findUiTargetAt initialState (510, 179)
            `shouldBe` Just SaveTarget

        it "shows the add to PDF button below while a PDF is open" $ do
          let
            savePoint = (510, 179)
            addToPdfPoint = (510, 139)
          findUiTargetAt pdfState savePoint
            `shouldBe` Just (PdfActionTarget NewPdf)
          findUiTargetAt pdfState addToPdfPoint
            `shouldBe` Nothing
          findUiTargetAt pdfStateWithDocument savePoint
            `shouldBe` Just (PdfActionTarget NewPdf)
          findUiTargetAt pdfStateWithDocument addToPdfPoint
            `shouldBe` Just (PdfActionTarget AddToPdf)
          findUiTargetAt
            initialState{pdfDocument = Just firstPdf}
            addToPdfPoint
            `shouldBe` Nothing

      describe "PDF documents" $ do
        let
          pdfState =
            initialState
              { exportSettings =
                  initialState.exportSettings{outputFormat = PdfFormat}
              }
          pdfStateWithDocument = pdfState{pdfDocument = Just firstPdf}

        it "starts the first PDF with the first image" $ do
          let pdfDocument = pdfDocumentForImage pdfState NewPdf "dir/IMG_1.jpeg"
          pdfDocument.number `shouldBe` 1
          pdfDocument.outputPath `shouldBe` "dir/IMG_1.pdf"
          P.length pdfDocument.pages `shouldBe` 0
          defaultPdfAction pdfState `shouldBe` NewPdf
          pdfActionText pdfState NewPdf `shouldBe` "Start PDF 1"

        it "adds the following images to the open PDF" $ do
          let
            pdfDocument =
              pdfDocumentForImage pdfStateWithDocument AddToPdf "dir/IMG_3.jpeg"
          pdfDocument.number `shouldBe` 1
          pdfDocument.outputPath `shouldBe` "dir/IMG_1.pdf"
          P.length pdfDocument.pages `shouldBe` 2
          defaultPdfAction pdfStateWithDocument `shouldBe` AddToPdf
          pdfActionText pdfStateWithDocument AddToPdf
            `shouldBe` "Add page 3 to PDF 1"

        it "starts a new PDF" $ do
          let
            pdfDocument =
              pdfDocumentForImage pdfStateWithDocument NewPdf "dir/IMG_3.jpeg"
          pdfDocument.number `shouldBe` 2
          pdfDocument.outputPath `shouldBe` "dir/IMG_3.pdf"
          P.length pdfDocument.pages `shouldBe` 0
          pdfActionText pdfStateWithDocument NewPdf `shouldBe` "Start PDF 2"

        it "completes the open PDF when a new one is started" $ do
          P.isJust (completedPdfDocument pdfStateWithDocument AddToPdf)
            `shouldBe` False
          (completedPdfDocument pdfStateWithDocument NewPdf <&> (.number))
            `shouldBe` Just 1

        it "completes the open PDF when an image is exported" $ do
          let imageState = initialState{pdfDocument = Just firstPdf}
          (completedPdfDocument imageState AddToPdf <&> (.number))
            `shouldBe` Just 1

      describe "queueImages" $ do
        let
          isLoaded = \case
            ImageToLoad{} -> False
            ImageData{} -> True
          laterBatch =
            ["images/examples/doc.jpg", "images/examples/doc-large.jpg"]
          loadedState =
            loadFileIntoState
              initialState
                { images = imagesToLoad ["images/examples/doc-marking.jpg"]
                }

        it "sorts all images anew until an image was exported" $ do
          appState <- loadedState
          let newState = queueImages appState laterBatch
          (newState.images <&> imagePath)
            `shouldBe` [ "images/examples/doc-large.jpg"
                       , "images/examples/doc-marking.jpg"
                       , "images/examples/doc.jpg"
                       ]
          -- The new current image must be loaded first
          (newState.images <&> isLoaded) `shouldBe` [False, False, False]
          newState.corners `shouldBe` []

        it "keeps the current image after an image was exported" $ do
          appState <- loadedState
          let newState = queueImages appState{hasExported = True} laterBatch
          (newState.images <&> imagePath)
            `shouldBe` [ "images/examples/doc-marking.jpg"
                       , "images/examples/doc-large.jpg"
                       , "images/examples/doc.jpg"
                       ]
          (newState.images <&> isLoaded) `shouldBe` [True, False, False]

        it "ignores images that are already queued" $ do
          appState <- loadedState
          let
            newState =
              queueImages appState ["images/examples/doc-marking.jpg"]
          (newState.images <&> imagePath)
            `shouldBe` ["images/examples/doc-marking.jpg"]

        it "applies an image loaded in the background" $ do
          queuedState <- loadedState <&> \appState -> queueImages appState laterBatch
          loadedImageState <- loadFileIntoState queuedState
          let
            loadedImage =
              LoadedImage
                { path = "images/examples/doc-large.jpg"
                , loadedState = loadedImageState
                }
            newState = applyLoadedImage loadedImage queuedState
          (newState.images <&> isLoaded) `shouldBe` [True, False, False]
          P.length newState.corners `shouldBe` 4

        it "discards an image that is no longer the current one" $ do
          queuedState <- loadedState <&> \appState -> queueImages appState laterBatch
          loadedImageState <- loadFileIntoState queuedState
          let
            otherState =
              queuedState{images = imagesToLoad ["images/examples/doc.jpg"]}
            loadedImage =
              LoadedImage
                { path = "images/examples/doc-large.jpg"
                , loadedState = loadedImageState
                }
            newState = applyLoadedImage loadedImage otherState
          (newState.images <&> isLoaded) `shouldBe` [False]

      describe "license keys" $ do
        it "hashes license keys with SHA-256" $ do
          sha256Hex "abc"
            `shouldBe` "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

        it "rejects empty and unknown license keys" $ do
          isValidLicenseKey "" `shouldBe` False
          isValidLicenseKey "not-a-license-key" `shouldBe` False

      describe "export settings" $ do
        it "default to a color image with English OCR" $ do
          initialState.exportSettings
            `shouldBe` ExportSettings
              { style = ColorStyle
              , outputFormat = ImageFormat
              , ocrLanguage = English
              }

      describe "toLogicalEvent" $ do
        it "leaves coordinates untouched on a display without scaling" $ do
          toLogicalEvent 1 (EventMotion (100, -50))
            `shouldBe` EventMotion (100, -50)

        it "converts mouse positions to logical units" $ do
          toLogicalEvent 2 (EventMotion (100, -50))
            `shouldBe` EventMotion (50, -25)

        it "converts the window size to logical units" $ do
          toLogicalEvent 1.75 (EventResize (2240, 1680))
            `shouldBe` EventResize (1280, 960)

        it "passes events without coordinates through" $ do
          toLogicalEvent 2 (EventDrop ["doc.jpg"])
            `shouldBe` EventDrop ["doc.jpg"]

      describe "image view zoom" $ do
        let
          loadedState =
            loadFileIntoState
              initialState
                { images = imagesToLoad ["images/examples/doc-marking.jpg"]
                }
          -- Rounding the size of the shown image shifts points slightly
          arePointsClose :: [Point] -> [Point] -> Bool
          arePointsClose points otherPoints =
            P.length points == P.length otherPoints
              && P.and
                ( P.zipWith
                    ( \(x, y) (otherX, otherY) ->
                        P.abs (x - otherX) < 1 && P.abs (y - otherY) < 1
                    )
                    points
                    otherPoints
                )
          noController =
            Controller
              { controllerSetRedraw = pure ()
              , controllerModifyViewPort = P.const (pure ())
              , controllerSetCursor = P.const (pure ())
              }
          noModifiers = Modifiers{shift = Up, ctrl = Up, alt = Up}
          sendEvent event appState = do
            stateRef <- newIORef appState
            handleEvent stateRef noController event appState

        it "keeps the corners on the same pixels of the image" $ do
          appState <- loadedState
          let
            zoomedState = zoomImageView 2.5 (120, -80) appState
            pannedState = panImageView (30, 40) zoomedState
          zoomedState.zoom `shouldBe` 2.5
          getCorners zoomedState `shouldSatisfy` arePointsClose (getCorners appState)
          getCorners pannedState `shouldSatisfy` arePointsClose (getCorners appState)
          getCorners (resetImageView pannedState)
            `shouldSatisfy` arePointsClose (getCorners appState)

        it "keeps the pixel under the mouse at its position" $ do
          appState <- loadedState
          let
            anchor = (120, -80)
            zoomedState = zoomImageView 3 anchor appState
          imageViewToImage zoomedState [anchor]
            `shouldSatisfy` arePointsClose (imageViewToImage appState [anchor])

        it "limits the zoom and keeps the image in the image view" $ do
          appState <- loadedState
          let
            zoomedOutState = zoomImageView 0.001 (0, 0) appState
            pannedState = panImageView (100000, -100000) appState
          zoomedOutState.zoom `shouldBe` minZoom
          (zoomImageView 1000 (0, 0) appState).zoom `shouldBe` maxZoom
          pannedState.panOffset `shouldBe` (1535 / 2, -(947 / 2))

        it "resets the zoom and the pan offset" $ do
          appState <- loadedState
          let
            newState =
              resetImageView $ panImageView (30, 40) $ zoomImageView 2 (0, 0) appState
          newState.zoom `shouldBe` 1
          newState.panOffset `shouldBe` (0, 0)
          newState.scaleFactor `shouldBe` appState.scaleFactor

        it "keeps the zoom when the window is resized" $ do
          appState <- loadedState
          newState <-
            sendEvent (EventResize (800, 600)) (zoomImageView 2 (0, 0) appState)
          newState.zoom `shouldBe` 2
          getCorners newState `shouldSatisfy` arePointsClose (getCorners appState)

        it "places the zoom controls in the bottom right corner" $ do
          appState <- loadedState
          -- The image view ends at x = 1280 / 2 - 260 = 380 and y = -960 / 2
          findUiTargetAt appState (286, -460)
            `shouldBe` Just (ZoomControlTarget ZoomOut)
          findUiTargetAt appState (323, -460)
            `shouldBe` Just (ZoomControlTarget ZoomReset)
          findUiTargetAt appState (360, -460)
            `shouldBe` Just (ZoomControlTarget ZoomIn)
          findUiTargetAt appState (360, -440) `shouldBe` Nothing

        it "zooms with the zoom controls and the keyboard" $ do
          appState <- loadedState
          let click point = EventKey (MouseButton LeftButton) Down noModifiers point
          zoomedOutState <- sendEvent (click (286, -460)) appState
          zoomedInState <- sendEvent (click (360, -460)) appState
          resetState <- sendEvent (click (323, -460)) zoomedInState
          keyZoomedInState <-
            sendEvent (EventKey (Char '+') Down noModifiers (0, 0)) appState
          keyResetState <-
            sendEvent (EventKey (Char '0') Down noModifiers (0, 0)) keyZoomedInState
          zoomedOutState.zoom `shouldSatisfy` (< 1)
          zoomedInState.zoom `shouldSatisfy` (> 1)
          resetState.zoom `shouldBe` 1
          keyZoomedInState.zoom `shouldSatisfy` (> 1)
          keyResetState.zoom `shouldBe` 1
          -- Clicking the controls doesn't add a corner
          zoomedInState.corners `shouldBe` (zoomImageView 1.2 (0, 0) appState).corners
          zoomLevelText zoomedInState `shouldBe` "120 %"

        it "moves the image while it's dragged with the right mouse button" $ do
          appState <- loadedState
          let zoomedState = zoomImageView 2 (0, 0) appState
          draggingState <-
            sendEvent
              (EventKey (MouseButton RightButton) Down noModifiers (0, 0))
              zoomedState
          movedState <- sendEvent (EventMotion (40, -20)) draggingState
          releasedState <-
            sendEvent
              (EventKey (MouseButton RightButton) Up noModifiers (40, -20))
              movedState
          panOffsetInView movedState `shouldSatisfy` \(x, y) ->
            P.abs (x - 40) < 0.001 && P.abs (y + 20) < 0.001
          releasedState.panDragged `shouldBe` Nothing

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
        orientation <- getOrientationFromPng outPath
        orientation `shouldBe` Nothing

        removePathForcibly workDir

  UtilsSpec.spec


-- | PDF with two pages, as it is after exporting two images
firstPdf :: PdfDocument
firstPdf =
  let
    page =
      PdfPage
        { image = generateImage (\_ _ -> PixelRGBA8 0 0 0 255) 1 1
        , ocrWords = []
        }
  in
    PdfDocument
      { number = 1
      , outputPath = "dir/IMG_1.pdf"
      , pages = [page, page]
      }
