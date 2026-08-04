module Main where

import Protolude (IO, Maybe (Nothing), ($), (==))

import GHC.IO.Handle (hDuplicateTo)
import System.IO (
  IOMode (WriteMode),
  hSetEncoding,
  openFile,
  stderr,
  stdout,
  utf8,
 )
import System.Info (os)

import ConfigLoader (loadConfig)
import Lib (loadAndStart)


{-| Windowed GUI launcher (the `pythonw.exe` pattern).
On Windows it's linked with `-mwindows` (GUI subsystem),
so double-clicking it doesn't open a console window next to the app.
-}
main :: IO ()
main = do
  if os == "mingw32"
    then do
      -- In a GUI-subsystem binary the standard handles aren't connected
      -- to a console, so any write to them would fail. Route them to NUL.
      nul <- openFile "NUL" WriteMode
      hDuplicateTo nul stdout
      hDuplicateTo nul stderr
    else do
      hSetEncoding stdout utf8
      hSetEncoding stderr utf8

  config <- loadConfig
  loadAndStart config Nothing
