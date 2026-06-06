module Rotate where

import Protolude (
  FilePath,
  IO,
  Int,
  Maybe (Just, Nothing),
  Semigroup ((<>)),
  all,
  die,
  even,
  not,
  null,
  putText,
  when,
  ($),
  (&),
  (&&),
  (==),
 )
import Protolude qualified as P

import Data.Char (isDigit)
import Data.Text (pack)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Read (readMaybe)

import Graphics.Image (readImageRGBA, rotate270, rotate90, writeImage)
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


{-| Parse the page number from a filename's base name.
Returns 'Nothing' if the base name is empty or not purely numeric
(e.g. @"cover.png"@ or @"1.2.png"@).
-}
pageNumber :: FilePath -> Maybe Int
pageNumber file =
  let base = takeBaseName file
  in  if not (null base) && all isDigit base
        then readMaybe base
        else Nothing


{-| Rotate sequentially numbered PNGs in a directory, in place:
odd page numbers 90° counter-clockwise, even page numbers 90° clockwise.
Non-numeric filenames are skipped.
Exits with an error if the directory contains no PNGs.
-}
rotatePages :: FilePath -> IO ()
rotatePages directory = do
  allFiles <- listDirectory directory

  let
    pngFiles =
      allFiles
        & P.filter (\file -> takeExtension file == ".png")

  when (null pngFiles) $
    die "No PNG files found"

  pngFiles
    & P.mapM_
      ( \file ->
          case pageNumber file of
            Nothing ->
              putText $ "Skip " <> pack file <> " (not a numeric filename)"
            Just number -> do
              let path = directory </> file

              image <- readImageRGBA path

              case rotationForNumber number of
                Clockwise -> do
                  putText $ "Rotate " <> pack file <> " 90° clockwise"
                  writeImage path (rotate90 image)
                CounterClockwise -> do
                  putText $ "Rotate " <> pack file <> " 90° counter-clockwise"
                  writeImage path (rotate270 image)
      )
