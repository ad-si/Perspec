module Main where

import Protolude (
  Bool (True),
  Either (Left, Right),
  FilePath,
  IO,
  Maybe (Just, Nothing),
  fst,
  pure,
  putErrText,
  ($),
  (&),
  (<>),
  (==),
 )

import Brillo.Interface.IO.MessageBox (
  IconType (Error),
  OK (OK),
  messageBox,
 )
import Control.Exception (
  SomeException,
  catch,
  displayException,
  fromException,
  throwIO,
  try,
 )
import Data.Text (breakOn, pack, strip)
import GHC.IO.Handle (hDuplicateTo)
import System.Directory (
  XdgDirectory (XdgCache),
  createDirectoryIfMissing,
  getXdgDirectory,
 )
import System.Exit (ExitCode, exitFailure)
import System.FilePath ((</>))
import System.IO (
  BufferMode (LineBuffering),
  IOMode (WriteMode),
  hSetBuffering,
  hSetEncoding,
  openFile,
  stderr,
  stdout,
  utf8,
 )
import System.Info (os)

import ConfigLoader (loadConfig)
import Lib (loadAndStart)


{-| Redirect the standard handles to a log file and return its path.

In a GUI-subsystem binary the standard handles aren't connected to a console,
so any write to them would fail. Point them at a log file instead of
discarding them, so that warnings (e.g. from the OpenGL backend)
stay diagnosable after the fact.
-}
redirectOutputToLogFile :: IO FilePath
redirectOutputToLogFile = do
  cacheDirectory <- getXdgDirectory XdgCache "Perspec"
  createDirectoryIfMissing True cacheDirectory

  let logPath = cacheDirectory </> "perspec.log"
  logHandle <- openFile logPath WriteMode
  hSetEncoding logHandle utf8
  hSetBuffering logHandle LineBuffering
  hDuplicateTo logHandle stdout
  hDuplicateTo logHandle stderr

  pure logPath


{-| Set up the log file, tolerating a failure to do so.

Returns 'Nothing' if the log file couldn't be opened,
as an unwritable cache directory must not keep the app from starting.
-}
setUpLogFile :: IO (Maybe FilePath)
setUpLogFile = do
  logPathOrError <-
    try redirectOutputToLogFile :: IO (Either SomeException FilePath)
  pure $ case logPathOrError of
    Left _ -> Nothing
    Right logPath -> Just logPath


{-| Show a fatal error in a dialog and exit.

Without a console an uncaught exception would be invisible
and the app would seem to simply not start at all.
-}
reportFatalError :: Maybe FilePath -> SomeException -> IO ()
reportFatalError logPathMb exception =
  case fromException exception :: Maybe ExitCode of
    -- Don't intercept a regular `exitWith`
    Just exitCode -> throwIO exitCode
    Nothing -> do
      let
        errorMessage = pack $ displayException exception

        -- The call stack is only noise in a dialog, but stays in the log
        errorSummary =
          errorMessage
            & breakOn "HasCallStack backtrace:"
            & fst
            & strip

        logHint = case logPathMb of
          Nothing -> ""
          Just logPath -> "\n\nThe full log is available at:\n" <> pack logPath

      putErrText errorMessage
      _ <-
        messageBox
          "Perspec"
          ("Perspec could not be started:\n\n" <> errorSummary <> logHint)
          Error
          OK
      exitFailure


{-| Windowed GUI launcher (the `pythonw.exe` pattern).
On Windows it's linked with `-mwindows` (GUI subsystem),
so double-clicking it doesn't open a console window next to the app.
-}
main :: IO ()
main = do
  let startGui = do
        config <- loadConfig
        loadAndStart config Nothing

  if os == "mingw32"
    then do
      logPathMb <- setUpLogFile
      startGui `catch` reportFatalError logPathMb
    else do
      hSetEncoding stdout utf8
      hSetEncoding stderr utf8
      startGui
