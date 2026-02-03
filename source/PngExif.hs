{-| Extract EXIF metadata from PNG eXIf chunks.

PNG files can contain EXIF data in an "eXIf" chunk (standardized in 2017).
The eXIf chunk contains raw EXIF/TIFF data starting with the byte order marker
("II" for little-endian or "MM" for big-endian), followed by the TIFF magic
number (42), and then IFD entries.

This module provides pure Haskell parsing of the eXIf chunk to extract
the orientation tag, which is commonly needed for correct image display.

Reference: https://ftp-osl.osuosl.org/pub/libpng/documents/pngext-1.5.0.html
-}
module PngExif (
  getExifOrientationFromPng,
  extractExifFromPng,
) where

import Protolude (
  Bool (..),
  Eq ((==)),
  FilePath,
  IO,
  Int,
  Maybe (Just, Nothing),
  Num ((*), (+)),
  Word16,
  Word32,
  fromIntegral,
  otherwise,
  pure,
  ($),
  (<),
  (>=),
 )

import Codec.Picture.Metadata.Exif (ExifData (..))
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL


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
