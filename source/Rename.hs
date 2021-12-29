module Rename where

import Protolude as P

import Algorithms.NaturalSort as NaturalSort
import Data.Text (pack, unpack)
import System.FilePath (takeExtension)

import Types


mapWithIndex :: Int -> RenameMode -> SortOrder -> (a -> Int -> b) -> [a] -> [b]
mapWithIndex startNum renameMode sortOrder function elements =
  let
    realStartNum =
      case (renameMode, sortOrder) of
        (Sequential, _) -> startNum

        (Even, Ascending) -> ((startNum - 1) .|. 1) + 1
        (Even, Descending) -> ((startNum + 1) .|. 1) - 1

        (Odd, Ascending) -> startNum .|. 1
        (Odd, Descending) -> (startNum - 1) .|. 1

    nextNum =
      case (renameMode, sortOrder) of
        (Sequential, Ascending) -> realStartNum + 1
        (Sequential, Descending) -> realStartNum - 1
        (Even, Ascending) -> realStartNum + 2
        (Even, Descending) -> realStartNum - 2
        (Odd, Ascending) -> realStartNum + 2
        (Odd, Descending) -> realStartNum - 2

    mappings =
      zipWith function elements [realStartNum, nextNum..]
  in
    mappings


getRenamingBatches
  :: Int
  -> RenameMode
  -> SortOrder
  -> [Text]
  -> [[(Text, Text)]]
getRenamingBatches startNumber renameMode sortOrder files =
  let
    filesSorted :: [Text]
    filesSorted = files
      <&> unpack
      & sortBy NaturalSort.compare
      <&> pack

    renamings :: [(Text, Text)]
    renamings = filesSorted
      & mapWithIndex startNumber renameMode sortOrder (\file index ->
          ( file
          , (if index < 0 then "_todo_" else "")
            <> show index
            <> (pack $ takeExtension $ unpack file)
          )
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
