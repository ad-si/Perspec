{-# LANGUAGE QuasiQuotes #-}

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

import Lib (loadAndStart)
import Rename (getRenamingBatches)
import Types (
  Config (transformAppFlag),
  RenameMode (Even, Odd, Sequential),
  SortOrder (Ascending, Descending),
  TransformApp (Hip),
 )


patterns :: Docopt
patterns = [docoptFile|usage.txt|]


getArgOrExit :: Arguments -> Docopt.Option -> IO [Char]
getArgOrExit = getArgOrExitWith patterns


execWithArgs :: Config -> [[Char]] -> IO ()
execWithArgs config cliArgs = do
  args <- parseArgsOrExit patterns cliArgs

  when (args `isPresent` command "gui") $ do
    loadAndStart config Nothing

  when (args `isPresent` command "fastfix") $ do
    let files = args `getAllArgs` argument "file"

    filesAbs <- files & P.mapM makeAbsolute

    let
      file = case filesAbs of
        [x] -> x
        x : _ -> x
        _ -> "This branch should not be reachable"

    loadAndStart (config{transformAppFlag = Hip}) (Just file)

  when (args `isPresent` command "fix") $ do
    let files = args `getAllArgs` argument "file"

    filesAbs <- files & P.mapM makeAbsolute

    filesAbs <&> Just & P.mapM_ (loadAndStart config)

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
