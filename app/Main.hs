{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Protolude (
  Bool (True, False),
  Char,
  Either (Left, Right),
  IO,
  Maybe (Just, Nothing),
  Monad ((>>=)),
  die,
  getArgs,
  otherwise,
  reads,
  when,
  writeFile,
  ($),
  (&),
  (<&>),
 )
import Protolude qualified as P

import Data.Text as T (isInfixOf, pack, unpack)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Console.Docopt as Docopt (
  Arguments,
  Docopt,
  Option,
  argument,
  command,
  docoptFile,
  getAllArgs,
  getArg,
  getArgOrExitWith,
  isPresent,
  longOption,
  parseArgsOrExit,
 )
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  getXdgDirectory,
  listDirectory,
  makeAbsolute,
  renameFile,
 )
import System.FilePath ((</>))

import Brillo (
  BitmapFormat (BitmapFormat),
  Picture (Bitmap),
  PixelFormat (PxRGBA),
  RowOrder (TopToBottom),
 )
import Brillo.Rendering (BitmapData(..), bitmapOfForeignPtr)
import Brillo.Data.Color (makeColor)
import Brillo.Data.Display (Display (InWindow))
import Brillo.Interface.Pure.Display (display)
import Foreign (newForeignPtr_, withForeignPtr, castForeignPtr)
import Foreign.Ptr (castPtr)
import Lib (loadAndStart)
import Rename (getRenamingBatches)
import SimpleCV (otsu_threshold_rgba)
import Types (
  Config (transformBackendFlag),
  RenameMode (Even, Odd, Sequential),
  SortOrder (Ascending, Descending),
  TransformBackend (HipBackend),
 )
import Utils (loadImage)


patterns :: Docopt
patterns = [docoptFile|usage.txt|]


getArgOrExit :: Arguments -> Docopt.Option -> IO [Char]
getArgOrExit = getArgOrExitWith patterns


execWithArgs :: Config -> [[Char]] -> IO ()
execWithArgs config cliArgs = do
  args <- parseArgsOrExit patterns cliArgs

  when (args `isPresent` command "test") $ do
    pictureMetadataEither <- loadImage "/Users/adrian/Dropbox/Projects/Perspec/images/doc.jpg"

    case pictureMetadataEither of
      Left error -> do
        P.putText error
      Right (Bitmap bitmapData, metadata) -> do
        P.print ("metadata" :: P.Text, metadata)
        let
          width = P.fst bitmapData.bitmapSize
          height = P.snd bitmapData.bitmapSize
        withForeignPtr (castForeignPtr bitmapData.bitmapPointer) $ \ptr -> do
          -- resutlImg <- grayscale width height ptr
          resutlImg <- otsu_threshold_rgba width height False ptr
          resultImgForeignPtr <- newForeignPtr_ (castPtr resutlImg)
          let grayscalePicture =
                bitmapOfForeignPtr
                  width
                  height
                  (BitmapFormat TopToBottom PxRGBA)
                  resultImgForeignPtr
                  True
          display
            (InWindow "Grayscale" (width, height) (10, 10))
            (makeColor 0 0 0 0)
            grayscalePicture
      Right _ ->
        P.putText "Unsupported image format"

  when (args `isPresent` command "gui") $ do
    loadAndStart config Nothing

  when (args `isPresent` command "fastfix") $ do
    let files = args `getAllArgs` argument "file"
    filesAbs <- files & P.mapM makeAbsolute

    loadAndStart (config{transformBackendFlag = HipBackend}) (Just filesAbs)

  when (args `isPresent` command "fix") $ do
    let files = args `getAllArgs` argument "file"
    filesAbs <- files & P.mapM makeAbsolute

    loadAndStart config (Just filesAbs)

  when (args `isPresent` command "rename") $ do
    directory <- args `getArgOrExit` argument "directory"

    let
      startNumberMb =
        args
          `getArg` longOption "start-with"
          <&> reads
          & ( \case
                Just [(int, _)] -> Just int
                _ -> Nothing
            )

      renameMode
        | args `isPresent` longOption "even" = Even
        | args `isPresent` longOption "odd" = Odd
        | otherwise = Sequential

      sortOrder =
        if args `isPresent` longOption "descending"
          then Descending
          else Ascending

    files <- listDirectory directory

    let
      renamingBatches =
        getRenamingBatches
          startNumberMb
          renameMode
          sortOrder
          (files <&> pack)

    renamingBatches
      & P.mapM_
        ( \renamings ->
            renamings
              & P.mapM_
                ( \(file, target) ->
                    renameFile
                      (directory </> unpack file)
                      (directory </> unpack target)
                )
        )


main :: IO ()
main = do
  let appName = "Perspec"

  configDirectory <- getXdgDirectory XdgConfig appName
  createDirectoryIfMissing True configDirectory

  let configPath = configDirectory </> "config.yaml"

  configResult <- decodeFileEither configPath

  case configResult of
    Left error -> do
      if "file not found"
        `T.isInfixOf` T.pack (prettyPrintParseException error)
        then do
          writeFile configPath "licenseKey:\n"
          configResult2 <- decodeFileEither configPath

          case configResult2 of
            Left error2 -> die $ T.pack $ prettyPrintParseException error2
            Right config -> do
              getArgs >>= execWithArgs config
        else die $ T.pack $ prettyPrintParseException error
    Right config -> do
      getArgs >>= execWithArgs config
