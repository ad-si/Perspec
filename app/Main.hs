{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.Environment
import Protolude as P
import Data.Text as T


load :: FilePath -> IO ()
load filePath = do
  picMaybe <- loadJuicy filePath

  let
    width = 640
    height = 320

  case picMaybe of
    Nothing -> putText "Error: Image couldn't be loaded"
    Just picture -> display
      (InWindow filePath (width, height) (10,  10))
      black
      picture


frame :: Int -> Int -> Picture -> Float -> Picture
frame width height picture t =
  Color (greyN (abs $ sin (t * 2)))
    $ Pictures [ rectangleSolid (fromIntegral width) (fromIntegral height)
               , picture
               ]


-- | Displays uncompressed 24/32 bit BMP images.
main :: IO ()
main = do
  args <- getArgs

  case args of
    [filePath] ->
           if "bmp" `T.isSuffixOf` (T.pack filePath) then load filePath
      else if "jpg" `T.isSuffixOf` (T.pack filePath) then load filePath
      else putText "This file extension is not supported"

    _ ->
      putText $ T.unlines
        [ "usage: bitmap <file.bmp>"
        , "  file.bmp should be a 24 or 32-bit uncompressed BMP file"
        ]
