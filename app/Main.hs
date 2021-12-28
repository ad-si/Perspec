{-# LANGUAGE QuasiQuotes #-}

module Main where

import Protolude as P

import System.Console.Docopt as Docopt
import System.Directory (listDirectory, renameFile)
import System.FilePath ((</>))
import Data.Text (pack, unpack)

import Lib
import Rename
import Types


patterns :: Docopt
patterns = [docoptFile|usage.txt|]


getArgOrExit :: Arguments -> Docopt.Option -> IO [Char]
getArgOrExit = getArgOrExitWith patterns


execWithArgs :: [[Char]] -> IO ()
execWithArgs cliArgs = do
  args <- parseArgsOrExit patterns cliArgs

  when (args `isPresent` (command "fix")) $ do
    let files = args `getAllArgs` (argument "file")

    sequence_ $ files <&> loadAndStart


  when (args `isPresent` (command "rename")) $ do
    directory <- args `getArgOrExit` (argument "directory")

    let
      startNumber = args `getArg` (longOption "start-with")
        <&> reads
        <&> \case { [(int, _)] -> int; _ -> 0 }
        & fromMaybe 0

    let
      renameMode =
        if args `isPresent` (longOption "even")
        then Even
        else
          if args `isPresent` (longOption "odd")
          then Odd
          else Sequential

    files <- listDirectory directory

    let
      renamingBatches = getRenamingBatches
        startNumber
        renameMode
        (files <&> pack)

    sequence_ $ renamingBatches
      <&> (\renamings -> do
            sequence_ $ renamings
              <&> (\(file, target) ->
                      renameFile
                        (directory </> unpack file)
                        (directory </> unpack target)
                  )
          )


main :: IO ()
main = do
  getArgs >>= execWithArgs
