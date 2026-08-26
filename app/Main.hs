{-# LANGUAGE CPP #-}
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
  pure,
  reads,
  when,
  ($),
  (&),
  (&&),
  (<&>),
 )
import Protolude qualified as P


#if defined(mingw32_HOST_OS)
import Data.Word (Word32)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
#endif

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

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "windows.h GetConsoleProcessList"
  c_GetConsoleProcessList :: Ptr Word32 -> Word32 -> IO Word32
#endif


{-| Whether the process is the only one attached to its console.

That's the case when the exe was double-clicked in the Explorer,
because Windows then creates a console just for it,
and not when it was started from an already running terminal.
Always 'P.False' on platforms without the concept of owning a console.
-}
ownsItsConsole :: IO P.Bool
#if defined(mingw32_HOST_OS)
ownsItsConsole =
  -- Room for two process IDs is enough to tell "just us" from "more than us"
  allocaArray 2 $ \processIds -> do
    processCount <- c_GetConsoleProcessList processIds 2
    -- A count of 0 means there is no console at all
    pure (processCount == 1)
#else
ownsItsConsole = pure P.False
#endif


getArgOrExit :: Arguments -> Docopt.Option -> IO [Char]
getArgOrExit = getArgOrExitWith patterns


execWithArgs :: Config -> [[Char]] -> IO ()
execWithArgs confFromFile cliArgs = do
  -- Double-clicking the exe in the Explorer runs it without arguments in a
  -- console of its own, which should start the GUI rather than flash the
  -- usage text. Running it without arguments in a terminal is a plain CLI
  -- invocation, though, and prints the usage text as any other tool would.
  wasDoubleClicked <-
    if null cliArgs
      then ownsItsConsole
      else pure P.False

  let effectiveArgs =
        if wasDoubleClicked
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
