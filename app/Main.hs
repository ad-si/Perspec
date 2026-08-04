{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Main where

import Protolude (
  Char,
  Eq ((==)),
  IO,
  Maybe (Just, Nothing),
  Monad ((>>=)),
  dropWhile,
  getArgs,
  length,
  null,
  otherwise,
  reads,
  when,
  ($),
  (&),
  (&&),
  (<&>),
 )
import Protolude qualified as P

import Data.Text (pack, unpack)
import Data.Text qualified as T
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
  listDirectory,
  makeAbsolute,
  renameFile,
 )
import System.FilePath ((</>))
import System.IO (hSetEncoding, stderr, stdout, utf8)
import System.Info (os)

import ConfigLoader (loadConfig)
import Control.Arrow ((>>>))
import Lib (loadAndStart)
import Rename (getRenamingBatches)
import Rotate (rotatePages)
import Types (
  Config,
  RenameMode (Even, Odd, Sequential),
  SortOrder (Ascending, Descending),
  TransformBackend (FlatCVBackend, HipBackend),
  transformBackendFlag,
 )
import Utils (isImageFile)


patterns :: Docopt
patterns = [docoptFile|usage.txt|]


getArgOrExit :: Arguments -> Docopt.Option -> IO [Char]
getArgOrExit = getArgOrExitWith patterns


execWithArgs :: Config -> [[Char]] -> IO ()
execWithArgs confFromFile cliArgs = do
  -- On Windows, no arguments (e.g. the exe was double-clicked) starts the GUI.
  -- On other platforms the GUI is started via the app bundle,
  -- so a bare `perspec` should print the usage text.
  let effectiveArgs =
        if null cliArgs && os == "mingw32"
          then ["gui"]
          else cliArgs
  args <- parseArgsOrExit patterns effectiveArgs

  let config = case args `getArg` longOption "backend" of
        Nothing -> confFromFile
        Just backend ->
          confFromFile
            { transformBackendFlag =
                backend
                  & ( T.pack
                        >>> T.toLower
                        >>> \case
                          "hip" -> HipBackend
                          _ -> FlatCVBackend
                    )
            }

  when (args `isPresent` command "gui") $ do
    loadAndStart config Nothing

  when (args `isPresent` command "fix") $ do
    let files = args `getAllArgs` argument "file"
    filesAbs <- files & P.mapM makeAbsolute

    loadAndStart config (Just filesAbs)

  when (args `isPresent` command "rename") $ do
    directory <- args `getArgOrExit` argument "directory"

    let
      startWithStrMb = args `getArg` longOption "start-with"

      startNumberMb =
        startWithStrMb
          <&> reads
          & ( \case
                Just [(int, "")] -> Just int
                _ -> Nothing
            )

      -- Padding width is the digit count of the raw --start-with string
      -- (excluding any leading sign), so `--start-with=01` pads to 2 digits.
      padding = case (startWithStrMb, startNumberMb) of
        (Just s, Just _) -> length (dropWhile (== '-') s)
        _ -> 0

      renameMode
        | args `isPresent` longOption "even" = Even
        | args `isPresent` longOption "odd" = Odd
        | otherwise = Sequential

      sortOrder =
        if args `isPresent` longOption "descending"
          then Descending
          else Ascending

    allFiles <- listDirectory directory

    let
      -- Only rename image files, skipping directories, dotfiles
      -- (e.g. .DS_Store), AppleDouble files (._*), and other formats
      files =
        allFiles
          & P.filter isImageFile

      renamingBatches =
        getRenamingBatches
          startNumberMb
          padding
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

  when (args `isPresent` command "rotate") $ do
    directory <- args `getArgOrExit` argument "directory"
    rotatePages directory


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  config <- loadConfig
  getArgs >>= execWithArgs config
