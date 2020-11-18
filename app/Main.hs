module Main where

import Protolude as P

import Lib


main :: IO ()
main = do
  args <- getArgs

  case args of
    [filePath] -> loadAndStart filePath
    _          -> putText helpMessage
