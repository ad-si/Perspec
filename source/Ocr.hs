{-| Optical character recognition with the embedded Tesseract library.

The English, German, and Spanish language models are embedded into the binary,
so no Tesseract installation or @tessdata@ directory is needed.
Only the model of the selected 'OcrLanguage' is used for the recognition.
-}
module Ocr (
  OcrWord (..),
  ocrResolution,
  recognizeWords,
) where

import Protolude (
  Applicative (pure),
  Bool (False, True),
  Either (Left, Right),
  Eq ((/=), (==)),
  FilePath,
  IO,
  IOException,
  Int,
  Maybe (Just, Nothing),
  Ord ((<=), (>)),
  Text,
  bracket,
  fromIntegral,
  realToFrac,
  show,
  try,
  unless,
  ($),
  (&&),
  (*),
  (<$>),
  (<>),
 )

import Codec.Picture (Image (..), PixelRGBA8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Directory (
  XdgDirectory (XdgCache),
  createDirectoryIfMissing,
  doesFileExist,
  getXdgDirectory,
  renameFile,
 )
import System.FilePath ((</>))

import Types (OcrLanguage (..), OcrWord (..))


-- | Resolution in DPI the images are assumed to have
ocrResolution :: Int
ocrResolution = 300


-- | A Tesseract language model
data TrainedData = TrainedData
  { language :: Text
  -- ^ ISO 639-2 code Tesseract uses as the name of the model
  , content :: ByteString
  }


{-| Tesseract's model for the language in the fast integer variant
from https://github.com/tesseract-ocr/tessdata_fast.
-}
trainedDataFor :: OcrLanguage -> TrainedData
trainedDataFor = \case
  English -> TrainedData "eng" $(embedFile "tessdata/eng.traineddata")
  German -> TrainedData "deu" $(embedFile "tessdata/deu.traineddata")
  Spanish -> TrainedData "spa" $(embedFile "tessdata/spa.traineddata")


{-| Write the embedded model to the cache directory
and return the path of the directory.

Tesseract's C API loads models only from a directory.
A model that is already up to date isn't written again.
-}
installTrainedData :: TrainedData -> IO (Either Text FilePath)
installTrainedData model = do
  result <- try $ do
    tessdataDir <- getXdgDirectory XdgCache ("perspec" </> "tessdata")
    createDirectoryIfMissing True tessdataDir
    let modelPath = tessdataDir </> T.unpack model.language <> ".traineddata"
    modelExists <- doesFileExist modelPath
    isUpToDate <-
      if modelExists
        then (== model.content) <$> BS.readFile modelPath
        else pure False
    unless isUpToDate $ do
      -- Write to a temporary file first,
      -- so an interrupted write doesn't leave a broken model behind
      let tempPath = modelPath <> ".tmp"
      BS.writeFile tempPath model.content
      renameFile tempPath modelPath
    pure tessdataDir
  pure $ case result of
    Left (error :: IOException) ->
      Left $ "Failed to write the OCR model: " <> show error
    Right tessdataDir -> Right tessdataDir


data TessBaseAPI
data TessResultIterator
data TessPageIterator


-- | @OEM_LSTM_ONLY@, as the fast models only contain the LSTM recognizer
oemLstmOnly :: CInt
oemLstmOnly = 1


-- | @RIL_WORD@
rilWord :: CInt
rilWord = 3


foreign import ccall unsafe "tesseract/capi.h TessBaseAPICreate"
  tessBaseAPICreate :: IO (Ptr TessBaseAPI)


foreign import ccall unsafe "tesseract/capi.h TessBaseAPIDelete"
  tessBaseAPIDelete :: Ptr TessBaseAPI -> IO ()


foreign import ccall safe "tesseract/capi.h TessBaseAPIInit2"
  tessBaseAPIInit2 ::
    Ptr TessBaseAPI ->
    CString -> -- datapath
    CString -> -- language
    CInt -> -- oem
    IO CInt


foreign import ccall unsafe "tesseract/capi.h TessBaseAPISetImage"
  tessBaseAPISetImage ::
    Ptr TessBaseAPI ->
    Ptr Word8 -> -- imagedata
    CInt -> -- width
    CInt -> -- height
    CInt -> -- bytes_per_pixel
    CInt -> -- bytes_per_line
    IO ()


foreign import ccall unsafe "tesseract/capi.h TessBaseAPISetSourceResolution"
  tessBaseAPISetSourceResolution :: Ptr TessBaseAPI -> CInt -> IO ()


foreign import ccall safe "tesseract/capi.h TessBaseAPIRecognize"
  tessBaseAPIRecognize :: Ptr TessBaseAPI -> Ptr () -> IO CInt


foreign import ccall unsafe "tesseract/capi.h TessBaseAPIGetIterator"
  tessBaseAPIGetIterator :: Ptr TessBaseAPI -> IO (Ptr TessResultIterator)


foreign import ccall unsafe "tesseract/capi.h TessBaseAPIEnd"
  tessBaseAPIEnd :: Ptr TessBaseAPI -> IO ()


foreign import ccall unsafe "tesseract/capi.h TessResultIteratorDelete"
  tessResultIteratorDelete :: Ptr TessResultIterator -> IO ()


foreign import ccall unsafe "tesseract/capi.h TessResultIteratorNext"
  tessResultIteratorNext :: Ptr TessResultIterator -> CInt -> IO CInt


foreign import ccall unsafe "tesseract/capi.h TessResultIteratorGetUTF8Text"
  tessResultIteratorGetUTF8Text :: Ptr TessResultIterator -> CInt -> IO CString


foreign import ccall unsafe "tesseract/capi.h TessResultIteratorConfidence"
  tessResultIteratorConfidence :: Ptr TessResultIterator -> CInt -> IO CFloat


foreign import ccall unsafe "tesseract/capi.h TessResultIteratorGetPageIterator"
  tessResultIteratorGetPageIterator ::
    Ptr TessResultIterator -> IO (Ptr TessPageIterator)


foreign import ccall unsafe "tesseract/capi.h TessPageIteratorBoundingBox"
  tessPageIteratorBoundingBox ::
    Ptr TessPageIterator ->
    CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    IO CInt


foreign import ccall unsafe "tesseract/capi.h TessPageIteratorBaseline"
  tessPageIteratorBaseline ::
    Ptr TessPageIterator ->
    CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    IO CInt


foreign import ccall unsafe "tesseract/capi.h TessDeleteText"
  tessDeleteText :: CString -> IO ()


-- | Call the function with 4 output parameters and return their values
withQuad ::
  (Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt) ->
  IO (Maybe (Int, Int, Int, Int))
withQuad getQuad =
  alloca $ \aPtr ->
    alloca $ \bPtr ->
      alloca $ \cPtr ->
        alloca $ \dPtr -> do
          isAvailable <- getQuad aPtr bPtr cPtr dPtr
          if isAvailable == 0
            then pure Nothing
            else do
              a <- peek aPtr
              b <- peek bPtr
              c <- peek cPtr
              d <- peek dPtr
              pure $
                Just
                  ( fromIntegral a
                  , fromIntegral b
                  , fromIntegral c
                  , fromIntegral d
                  )


-- | Read the word the iterator currently points to
readWord :: Ptr TessResultIterator -> IO (Maybe OcrWord)
readWord resultIter = do
  pageIter <- tessResultIteratorGetPageIterator resultIter
  textPtr <- tessResultIteratorGetUTF8Text resultIter rilWord
  if textPtr == nullPtr
    then pure Nothing
    else do
      textBytes <- BS.packCString textPtr
      tessDeleteText textPtr
      confidence <- tessResultIteratorConfidence resultIter rilWord
      boxMb <- withQuad $ tessPageIteratorBoundingBox pageIter rilWord
      baselineMb <- withQuad $ tessPageIteratorBaseline pageIter rilWord

      let wordText = T.strip $ decodeUtf8Lenient textBytes

      pure $ case boxMb of
        Just (left, top, right, bottom)
          | wordText /= "" ->
              Just
                OcrWord
                  { text = wordText
                  , left = left
                  , top = top
                  , right = right
                  , bottom = bottom
                  , baseline = case baselineMb of
                      -- The baseline belongs to the whole text line,
                      -- which can be off for words of a different size
                      Just (_, y1, _, _) | y1 > top && y1 <= bottom -> y1
                      _ -> bottom
                  , confidence = realToFrac confidence
                  }
        _ -> Nothing


-- | Collect all words starting at the current position of the iterator
readWords :: Ptr TessResultIterator -> IO [OcrWord]
readWords resultIter = do
  wordMb <- readWord resultIter
  hasNext <- tessResultIteratorNext resultIter rilWord
  otherWords <-
    if hasNext == 0
      then pure []
      else readWords resultIter
  pure $ case wordMb of
    Just word -> word : otherWords
    Nothing -> otherWords


-- | Recognize the words of the given language in an image
recognizeWords :: OcrLanguage -> Image PixelRGBA8 -> IO (Either Text [OcrWord])
recognizeWords ocrLanguage image = do
  let model = trainedDataFor ocrLanguage
  tessdataDirEither <- installTrainedData model
  case tessdataDirEither of
    Left error -> pure $ Left error
    Right tessdataDir -> recognizeWordsWith tessdataDir model.language image


-- | Recognize the words with the named model in the given directory
recognizeWordsWith ::
  FilePath -> Text -> Image PixelRGBA8 -> IO (Either Text [OcrWord])
recognizeWordsWith tessdataDir language image =
  bracket tessBaseAPICreate tessBaseAPIDelete $ \api -> do
    initResult <-
      withCString tessdataDir $ \datapathPtr ->
        withCString (T.unpack language) $ \languagePtr ->
          tessBaseAPIInit2 api datapathPtr languagePtr oemLstmOnly

    if initResult /= 0
      then pure $ Left "Failed to initialize Tesseract"
      else do
        let
          width = imageWidth image
          height = imageHeight image
          bytesPerPixel = 4

        recognizeResult <-
          VS.unsafeWith (imageData image) $ \pixelsPtr -> do
            tessBaseAPISetImage
              api
              pixelsPtr
              (fromIntegral width)
              (fromIntegral height)
              bytesPerPixel
              (fromIntegral $ width * 4)
            tessBaseAPISetSourceResolution api (fromIntegral ocrResolution)
            tessBaseAPIRecognize api nullPtr

        result <-
          if recognizeResult /= 0
            then pure $ Left "Tesseract failed to recognize the text"
            else
              bracket
                (tessBaseAPIGetIterator api)
                ( \resultIter ->
                    if resultIter == nullPtr
                      then pure ()
                      else tessResultIteratorDelete resultIter
                )
                ( \resultIter ->
                    if resultIter == nullPtr
                      then pure $ Right []
                      else Right <$> readWords resultIter
                )

        tessBaseAPIEnd api
        pure result
