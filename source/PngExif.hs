{-| Extract and preserve EXIF metadata for PNG and JPEG files.

PNG files can contain EXIF data in an "eXIf" chunk (standardized in 2017).
The eXIf chunk contains raw EXIF/TIFF data starting with the byte order marker
("II" for little-endian or "MM" for big-endian), followed by the TIFF magic
number (42), and then IFD entries.

JPEG files store EXIF data in APP1 markers (0xFFE1) with "Exif\0\0" prefix.

This module provides:
- Pure Haskell parsing of the eXIf chunk to extract the orientation tag
- Extraction of raw EXIF bytes from JPEG files
- Writing PNG files with eXIf chunks to preserve EXIF metadata

Reference: https://ftp-osl.osuosl.org/pub/libpng/documents/pngext-1.5.0.html
-}
module PngExif (
  getExifOrientationFromPng,
  extractExifFromPng,
  extractExifBytesFromJpeg,
  extractExifBytesFromPng,
  extractExifBytesFromFile,
  writePngWithExif,
  savePngWithExif,
) where

import Protolude (
  Bool (..),
  Char,
  Either (..),
  Eq ((/=), (==)),
  FilePath,
  IO,
  Int,
  Maybe (Just, Nothing),
  Num ((*), (+), (-)),
  Ord ((<=), (>), (>=)),
  Semigroup ((<>)),
  Word16,
  Word32,
  Word8,
  fmap,
  fromIntegral,
  otherwise,
  pure,
  ($),
  (&&),
  (<),
  (||),
 )

import Codec.Picture (DynamicImage, encodeDynamicPng)
import Codec.Picture.Metadata.Exif (ExifData (..))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.List (foldl')
import Data.Vector.Unboxed qualified as V
import System.FilePath (takeExtension)


-- | PNG chunk structure
data PngChunk = PngChunk
  { chunkType :: BS.ByteString
  , chunkData :: BS.ByteString
  }


-- | Byte order for TIFF/EXIF parsing
data ByteOrder = LittleEndian | BigEndian


-- | Read a 32-bit big-endian unsigned integer from ByteString at offset
getWord32BE :: BS.ByteString -> Int -> Maybe Word32
getWord32BE bs offset
  | BS.length bs < offset + 4 = Nothing
  | otherwise =
      let b0 = fromIntegral (BS.index bs offset) :: Word32
          b1 = fromIntegral (BS.index bs (offset + 1)) :: Word32
          b2 = fromIntegral (BS.index bs (offset + 2)) :: Word32
          b3 = fromIntegral (BS.index bs (offset + 3)) :: Word32
      in  Just $ (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3


-- | Read a 16-bit unsigned integer with specified byte order
getWord16 :: ByteOrder -> BS.ByteString -> Int -> Maybe Word16
getWord16 byteOrder bs offset
  | BS.length bs < offset + 2 = Nothing
  | otherwise =
      let b0 = fromIntegral (BS.index bs offset) :: Word16
          b1 = fromIntegral (BS.index bs (offset + 1)) :: Word16
      in  Just $ case byteOrder of
            LittleEndian -> b0 .|. (b1 `shiftL` 8)
            BigEndian -> (b0 `shiftL` 8) .|. b1


-- | Read a 32-bit unsigned integer with specified byte order
getWord32 :: ByteOrder -> BS.ByteString -> Int -> Maybe Word32
getWord32 byteOrder bs offset
  | BS.length bs < offset + 4 = Nothing
  | otherwise =
      let b0 = fromIntegral (BS.index bs offset) :: Word32
          b1 = fromIntegral (BS.index bs (offset + 1)) :: Word32
          b2 = fromIntegral (BS.index bs (offset + 2)) :: Word32
          b3 = fromIntegral (BS.index bs (offset + 3)) :: Word32
      in  Just $ case byteOrder of
            LittleEndian -> b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
            BigEndian -> (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3


-- | PNG signature (8 bytes)
pngSignature :: BS.ByteString
pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]


-- | Parse PNG chunks from raw bytes (after signature)
parseChunks :: BS.ByteString -> [PngChunk]
parseChunks bs
  | BS.length bs < 12 = []
  | otherwise =
      case getWord32BE bs 0 of
        Nothing -> []
        Just len ->
          let chunkLen = fromIntegral len :: Int
              chunkType = BS.take 4 (BS.drop 4 bs)
              chunkData = BS.take chunkLen (BS.drop 8 bs)
              -- Skip: length (4) + type (4) + data (chunkLen) + CRC (4)
              remaining = BS.drop (12 + chunkLen) bs
              chunk = PngChunk{chunkType, chunkData}
          in  if BS.length bs >= 12 + chunkLen
                then chunk : parseChunks remaining
                else [chunk]


-- | Find the eXIf chunk in a list of PNG chunks
findExifChunk :: [PngChunk] -> Maybe BS.ByteString
findExifChunk [] = Nothing
findExifChunk (chunk : rest)
  | chunk.chunkType == "eXIf" = Just chunk.chunkData
  | otherwise = findExifChunk rest


-- | Parse the byte order from TIFF header
parseByteOrder :: BS.ByteString -> Maybe ByteOrder
parseByteOrder bs
  | BS.length bs < 2 = Nothing
  | otherwise =
      let b0 = BS.index bs 0
          b1 = BS.index bs 1
      in  case (b0, b1) of
            (0x49, 0x49) -> Just LittleEndian -- "II"
            (0x4D, 0x4D) -> Just BigEndian -- "MM"
            _ -> Nothing


-- | Validate TIFF magic number (42)
validateTiffMagic :: ByteOrder -> BS.ByteString -> Bool
validateTiffMagic byteOrder bs =
  case getWord16 byteOrder bs 2 of
    Just 42 -> True
    _ -> False


-- | EXIF tag for orientation (0x0112 = 274)
orientationTag :: Word16
orientationTag = 0x0112


{-| Parse IFD entries to find orientation
IFD entry format: tag (2) + type (2) + count (4) + value/offset (4) = 12 bytes
-}
findOrientationInIfd :: ByteOrder -> BS.ByteString -> Int -> Int -> Maybe Word16
findOrientationInIfd byteOrder bs ifdOffset numEntries = go 0
  where
    go :: Int -> Maybe Word16
    go idx
      | idx >= numEntries = Nothing
      | otherwise =
          let entryOffset = ifdOffset + 2 + (idx * 12) -- Skip entry count (2 bytes)
          in  case getWord16 byteOrder bs entryOffset of
                Just tag
                  | tag == orientationTag ->
                      -- Orientation is type SHORT (3), count 1
                      -- Value is stored directly in the value field (offset + 8)
                      getWord16 byteOrder bs (entryOffset + 8)
                _ -> go (idx + 1)


{-| Extract EXIF orientation from eXIf chunk data
The eXIf chunk contains raw TIFF/EXIF data starting with byte order marker
-}
parseExifOrientation :: BS.ByteString -> Maybe ExifData
parseExifOrientation exifData = do
  -- Parse byte order (first 2 bytes: "II" or "MM")
  byteOrder <- parseByteOrder exifData

  -- Validate TIFF magic number at offset 2
  if validateTiffMagic byteOrder exifData
    then do
      -- Get IFD0 offset (at offset 4, relative to start of TIFF header)
      ifd0Offset <- getWord32 byteOrder exifData 4
      let ifd0OffsetInt = fromIntegral ifd0Offset :: Int

      -- Get number of IFD entries
      numEntries <- getWord16 byteOrder exifData ifd0OffsetInt
      let numEntriesInt = fromIntegral numEntries :: Int

      -- Find orientation tag in IFD0
      orientation <-
        findOrientationInIfd byteOrder exifData ifd0OffsetInt numEntriesInt
      Just $ ExifShort orientation
    else Nothing


{-| Extract EXIF orientation from a PNG file
Returns the orientation as ExifData if found
-}
extractExifFromPng :: BS.ByteString -> Maybe ExifData
extractExifFromPng pngBytes
  | BS.length pngBytes < 8 = Nothing
  | BS.take 8 pngBytes == pngSignature =
      let chunks = parseChunks (BS.drop 8 pngBytes)
      in  case findExifChunk chunks of
            Just exifData -> parseExifOrientation exifData
            Nothing -> Nothing
  | otherwise = Nothing


-- | Convenience function to read a PNG file and extract orientation
getExifOrientationFromPng :: FilePath -> IO (Maybe ExifData)
getExifOrientationFromPng path = do
  contents <- BL.readFile path
  pure $ extractExifFromPng (BL.toStrict contents)


-- ============================================================================
-- JPEG EXIF Extraction
-- ============================================================================

-- | JPEG markers
jpegSOI :: Word8
jpegSOI = 0xD8 -- Start of Image


jpegAPP1 :: Word8
jpegAPP1 = 0xE1 -- APP1 marker (EXIF data)


-- | EXIF header in APP1 segment: "Exif\0\0"
exifHeader :: BS.ByteString
exifHeader = BS.pack [0x45, 0x78, 0x69, 0x66, 0x00, 0x00] -- "Exif\0\0"


{-| Extract raw EXIF bytes from a JPEG file.
Returns the TIFF data (after "Exif\0\0" header) that can be used in PNG eXIf chunk.
-}
extractExifBytesFromJpeg :: BS.ByteString -> Maybe BS.ByteString
extractExifBytesFromJpeg jpegBytes
  | BS.length jpegBytes < 4 = Nothing
  | BS.index jpegBytes 0 /= 0xFF = Nothing
  | BS.index jpegBytes 1 /= jpegSOI = Nothing
  | otherwise = findApp1Marker 2
  where
    findApp1Marker :: Int -> Maybe BS.ByteString
    findApp1Marker offset
      | offset + 4 > BS.length jpegBytes = Nothing
      | BS.index jpegBytes offset /= 0xFF = Nothing
      | otherwise =
          let marker = BS.index jpegBytes (offset + 1)
              segmentLen = getWord16BE' jpegBytes (offset + 2)
          in  if marker == jpegAPP1
                then extractExifFromApp1 (offset + 2) segmentLen
                else
                  if marker >= 0xE0 && marker <= 0xEF
                    then -- Skip this APP marker
                      findApp1Marker (offset + 2 + fromIntegral segmentLen)
                    else
                      if marker == 0xDB || marker == 0xC0 || marker == 0xC2 || marker == 0xC4
                        then -- Skip DQT, SOF0, SOF2, DHT markers
                          findApp1Marker (offset + 2 + fromIntegral segmentLen)
                        else Nothing -- Unknown marker, stop searching
    extractExifFromApp1 :: Int -> Word16 -> Maybe BS.ByteString
    extractExifFromApp1 lenOffset segmentLen =
      let dataOffset = lenOffset + 2 -- Skip length field
          dataLen = fromIntegral segmentLen - 2
      in  if dataOffset + 6 <= BS.length jpegBytes
            && BS.take 6 (BS.drop dataOffset jpegBytes) == exifHeader
            then
              let exifOffset = dataOffset + 6
                  exifLen = dataLen - 6
              in  if exifOffset + exifLen <= BS.length jpegBytes
                    then Just $ BS.take exifLen (BS.drop exifOffset jpegBytes)
                    else Nothing
            else Nothing

    getWord16BE' :: BS.ByteString -> Int -> Word16
    getWord16BE' bs off =
      let b0 = fromIntegral (BS.index bs off) :: Word16
          b1 = fromIntegral (BS.index bs (off + 1)) :: Word16
      in  (b0 `shiftL` 8) .|. b1


{-| Extract raw EXIF bytes from a file (JPEG or PNG).
For JPEG, extracts from APP1 marker.
For PNG, extracts from eXIf chunk.
-}
extractExifBytesFromFile :: FilePath -> IO (Maybe BS.ByteString)
extractExifBytesFromFile path = do
  contents <- BL.readFile path
  let bs = BL.toStrict contents
      ext = fmap toLower (takeExtension path)
  pure $ case ext of
    ".jpg" -> extractExifBytesFromJpeg bs
    ".jpeg" -> extractExifBytesFromJpeg bs
    ".png" -> extractExifBytesFromPng bs
    _ -> Nothing


-- | Extract raw EXIF bytes from PNG eXIf chunk
extractExifBytesFromPng :: BS.ByteString -> Maybe BS.ByteString
extractExifBytesFromPng pngBytes
  | BS.length pngBytes < 8 = Nothing
  | BS.take 8 pngBytes == pngSignature =
      let chunks = parseChunks (BS.drop 8 pngBytes)
      in  findExifChunk chunks
  | otherwise = Nothing


-- ============================================================================
-- PNG Writing with EXIF
-- ============================================================================

-- | CRC32 lookup table (IEEE 802.3 polynomial)
crc32Table :: V.Vector Word32
crc32Table = V.generate 256 makeEntry
  where
    makeEntry :: Int -> Word32
    makeEntry n =
      let c0 = fromIntegral n :: Word32
      in  foldl' updateBit c0 [0 :: Int .. 7]

    updateBit :: Word32 -> Int -> Word32
    updateBit c _
      | c .&. 1 == 1 = 0xEDB88320 `xor` (c `shiftR` 1)
      | otherwise = c `shiftR` 1


-- | Compute CRC32 checksum for PNG chunk
crc32 :: BS.ByteString -> Word32
crc32 bs = xor 0xFFFFFFFF (BS.foldl' updateCrc 0xFFFFFFFF bs)
  where
    updateCrc :: Word32 -> Word8 -> Word32
    updateCrc crc byte =
      let idx = fromIntegral ((crc `xor` fromIntegral byte) .&. 0xFF)
      in  (crc32Table V.! idx) `xor` (crc `shiftR` 8)


-- | Encode a 32-bit word as big-endian bytes
word32ToBE :: Word32 -> BS.ByteString
word32ToBE w =
  BS.pack
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]


-- | Create a PNG chunk with type and data
makePngChunk :: BS.ByteString -> BS.ByteString -> BS.ByteString
makePngChunk chunkType chunkData =
  let len = word32ToBE (fromIntegral (BS.length chunkData))
      typeAndData = chunkType <> chunkData
      checksum = word32ToBE (crc32 typeAndData)
  in  len <> typeAndData <> checksum


-- | Create an eXIf chunk from raw EXIF/TIFF data
makeExifChunk :: BS.ByteString -> BS.ByteString
makeExifChunk exifData = makePngChunk "eXIf" exifData


{-| Insert eXIf chunk into PNG data.
The eXIf chunk is inserted after IHDR but before IDAT chunks.
-}
insertExifChunk :: BS.ByteString -> BS.ByteString -> BS.ByteString
insertExifChunk pngData exifData =
  let exifChunk = makeExifChunk exifData
      -- PNG structure: signature (8) + IHDR chunk + other chunks
      -- We insert eXIf after IHDR
      signature = BS.take 8 pngData
      afterSig = BS.drop 8 pngData
      -- Find end of IHDR chunk: length (4) + "IHDR" (4) + data (13) + CRC (4) = 25 bytes
      ihdrLen = case getWord32BE afterSig 0 of
        Just len -> fromIntegral len + 12 -- length field + type + CRC
        Nothing -> 25 -- Default IHDR size
      ihdrChunk = BS.take ihdrLen afterSig
      restChunks = BS.drop ihdrLen afterSig
  in  signature <> ihdrChunk <> exifChunk <> restChunks


{-| Encode a PNG image with EXIF metadata.
Takes the raw EXIF/TIFF bytes and a DynamicImage, returns PNG bytes with eXIf chunk.
-}
writePngWithExif ::
  Maybe BS.ByteString -> DynamicImage -> Either [Char] BL.ByteString
writePngWithExif maybeExif image =
  case encodeDynamicPng image of
    Left err -> Left err
    Right pngBytes ->
      let pngStrict = BL.toStrict pngBytes
      in  Right $ case maybeExif of
            Nothing -> pngBytes
            Just exifData -> BL.fromStrict (insertExifChunk pngStrict exifData)


{-| Save a PNG image with EXIF metadata to a file.
Takes the raw EXIF/TIFF bytes, DynamicImage, and output path.
-}
savePngWithExif ::
  Maybe BS.ByteString -> DynamicImage -> FilePath -> IO (Either [Char] ())
savePngWithExif maybeExif image outPath =
  case writePngWithExif maybeExif image of
    Left err -> pure (Left err)
    Right pngBytes -> do
      BL.writeFile outPath pngBytes
      pure (Right ())
