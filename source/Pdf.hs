-- | Export of searchable PDFs
module Pdf (
  writeSearchablePdf,
) where

import Protolude (
  Applicative (pure),
  Bool (False, True),
  Double,
  Either (Left, Right),
  FilePath,
  Fractional ((/)),
  IO,
  Int,
  Maybe (Just),
  Num ((*), (-)),
  Ord ((<=)),
  Text,
  forM_,
  fromIntegral,
  show,
  ($),
  (<>),
 )

import Codec.Picture (Image (..), PixelRGB8, PixelRGBA8, pixelMap)
import Codec.Picture.Types (dropTransparency)
import Data.ByteString.Internal (fromForeignPtr0)
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Vector.Storable qualified as VS
import Graphics.PDF (
  FontName (Helvetica),
  Matrix (Matrix),
  PDFDocumentInfo (author, compressed),
  PDFFilter (NoFilter),
  PDFFont (PDFFont),
  PDFRect (PDFRect),
  TextMode (InvisibleText),
  addPage,
  applyMatrix,
  createPDFRawImageFromByteString,
  displayText,
  drawText,
  drawWithPage,
  drawXObject,
  mkStdFont,
  renderMode,
  runPdf,
  scale,
  setFont,
  setTextMatrix,
  standardDocInfo,
  textWidth,
  withNewContext,
 )

import Ocr (ocrResolution)
import Types (OcrWord (..), PdfPage (..))


{-| Write a PDF with a page for each image
and the recognized words as invisible text on top of it,
so the text can be searched, selected, and copied.
Each page has the size of its image.

The text layer uses the standard Helvetica font,
which only covers characters of the Mac Roman encoding.
-}
writeSearchablePdf :: FilePath -> NonEmpty PdfPage -> IO (Either Text ())
writeSearchablePdf outPath pages = do
  fontEither <- mkStdFont Helvetica
  case fontEither of
    Left error -> pure $ Left $ "Failed to load the PDF font: " <> show error
    Right font -> do
      let
        pointsPerPixel :: Double
        pointsPerPixel = 72 / fromIntegral ocrResolution

        toPoints :: Int -> Double
        toPoints pixels = fromIntegral pixels * pointsPerPixel

        pageRect :: Image PixelRGBA8 -> PDFRect
        pageRect image =
          PDFRect 0 0 (toPoints image.imageWidth) (toPoints image.imageHeight)

        -- Scale each word horizontally to cover its bounding box
        -- and place it on its baseline
        drawWord unitFont pageHeight word =
          let
            unitWidth = textWidth unitFont word.text
            fontSize = toPoints (word.bottom - word.top)
            horizontalScale = toPoints (word.right - word.left) / unitWidth
          in
            if unitWidth <= 0
              then pure ()
              else do
                setTextMatrix $
                  Matrix
                    horizontalScale
                    0
                    0
                    fontSize
                    (toPoints word.left)
                    (pageHeight - toPoints word.baseline)
                displayText word.text

        addSearchablePage pdfPage = do
          let
            image = pdfPage.image

            rgbImage :: Image PixelRGB8
            rgbImage = pixelMap dropTransparency image

            rgbBytes =
              let (pixelsPtr, size) = VS.unsafeToForeignPtr0 rgbImage.imageData
              in  BL.fromStrict $ fromForeignPtr0 pixelsPtr size

          pdfImage <-
            createPDFRawImageFromByteString
              image.imageWidth
              image.imageHeight
              False
              NoFilter
              rgbBytes
          page <- addPage $ Just $ pageRect image
          drawWithPage page $ do
            withNewContext $ do
              applyMatrix $ scale pointsPerPixel pointsPerPixel
              drawXObject pdfImage
            drawText $ do
              let unitFont = PDFFont font 1
              renderMode InvisibleText
              setFont unitFont
              forM_
                pdfPage.ocrWords
                (drawWord unitFont (toPoints image.imageHeight))

        docInfo = standardDocInfo{author = "Perspec", compressed = True}

      runPdf outPath docInfo (pageRect (NE.head pages).image) $
        forM_ pages addSearchablePage

      pure $ Right ()
