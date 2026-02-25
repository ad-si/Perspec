{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Main where

import Protolude (
  Bool (True),
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

import Data.Text (pack, unpack)
import Data.Text qualified as T
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
import System.IO (hSetEncoding, stderr, stdout, utf8)

import Control.Arrow ((>>>))
import Lib (loadAndStart)
import Rename (getRenamingBatches)
import Types (
  Config,
  RenameMode (Even, Odd, Sequential),
  SortOrder (Ascending, Descending),
  TransformBackend (FlatCVBackend, HipBackend, ImageMagickBackend),
  transformBackendFlag,
 )


patterns :: Docopt
patterns = [docoptFile|usage.txt|]


getArgOrExit :: Arguments -> Docopt.Option -> IO [Char]
getArgOrExit = getArgOrExitWith patterns


execWithArgs :: Config -> [[Char]] -> IO ()
execWithArgs confFromFile cliArgs = do
  args <- parseArgsOrExit patterns cliArgs

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
                          "imagemagick" -> ImageMagickBackend
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
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

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
