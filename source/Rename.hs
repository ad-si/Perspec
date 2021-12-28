module Rename where

import Protolude as P

import Algorithms.NaturalSort as NaturalSort
import Data.Text (pack, unpack)
import System.FilePath (takeExtension)

import Types


mapWithIndex :: Int -> RenameMode -> (a -> Int -> b) -> [a] -> [b]
mapWithIndex startNum renameMode function elements =
  let
    realStartNum = case renameMode of
      Sequential -> startNum
      Even -> ((startNum - 1) .|. 1) + 1
      Odd -> startNum .|. 1

    nextNum = case renameMode of
      Sequential -> realStartNum + 1
      Even -> realStartNum + 2
      Odd -> realStartNum + 2
  in
    zipWith function elements [realStartNum, nextNum..]


getRenamingBatches :: Int -> RenameMode -> [Text] -> [[(Text, Text)]]
getRenamingBatches startNumber renameMode files =
  let
    filesSorted :: [Text]
    filesSorted = files
      <&> unpack
      & sortBy NaturalSort.compare
      <&> pack

    renamings :: [(Text, Text)]
    renamings = filesSorted
      & mapWithIndex startNumber renameMode (\file index ->
          (file, show index <> (pack $ takeExtension $ unpack file))
        )

    renamingsWithTemp :: [(Text, Maybe Text, Text)]
    renamingsWithTemp = renamings
      <&> (\(file, target) ->
              ( file
              , if target `elem` files
                  then Just $ "_perspec_temp_" <> target
                  else Nothing
              , target
              )
          )

    renamingsBatch1 = renamingsWithTemp
      <&> (\(file, tempTargetMb, target) ->
              if tempTargetMb == Nothing
              then (file, target)
              else (file, fromMaybe "" tempTargetMb)
          )

    renamingsBatch2 = renamingsWithTemp
      & filter (\(_, tempTargetMb, _) -> tempTargetMb /= Nothing)
      <&> (\(_, tempTargetMb, target) -> (fromMaybe "" tempTargetMb, target))

  in
    [renamingsBatch1] <> (
        if null renamingsBatch2
        then []
        else [renamingsBatch2]
      )
