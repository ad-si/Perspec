{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}

module Rename where

import Protolude as P (
  Bits ((.|.)),
  Foldable (elem, length, null),
  Int,
  Maybe (..),
  Num (abs, (*), (+), (-)),
  Ord (max, (<)),
  Semigroup ((<>)),
  Text,
  filter,
  fromMaybe,
  isJust,
  isNothing,
  replicate,
  show,
  sortBy,
  zipWith,
  ($),
  (&),
  (<&>),
 )

import Algorithms.NaturalSort (compare)
import Data.Text (pack, unpack)
import Data.Text qualified as T
import System.FilePath (takeExtension)

import Types (RenameMode (..), SortOrder (..))


{-| Format a number with leading zeros so the digit count is at least
@width@. Negative numbers retain a leading dash before the padded digits.
A width of @0@ (or any width not larger than the natural representation)
leaves the number unchanged.
-}
padNum :: Int -> Int -> Text
padNum width n =
  let
    absStr = show (abs n)
    padCount = max 0 (width - T.length absStr)
    paddedAbs = pack (replicate padCount '0') <> absStr
  in
    if n < 0 then "-" <> paddedAbs else paddedAbs


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
      zipWith function elements [realStartNum, nextNum ..]
  in
    mappings


getRenamingBatches ::
  Maybe Int ->
  Int ->
  RenameMode ->
  SortOrder ->
  [Text] ->
  [[(Text, Text)]]
getRenamingBatches startNumberMb padding renameMode sortOrder files =
  let
    filesSorted :: [Text]
    filesSorted =
      files
        <&> unpack
        & sortBy Algorithms.NaturalSort.compare
          <&> pack

    startNumber :: Int
    startNumber =
      case (startNumberMb, sortOrder, renameMode) of
        (Just val, _, _) -> val
        (_, Ascending, _) -> 0
        (_, Descending, Sequential) -> length files - 1
        (_, Descending, Even) -> (length files * 2) - 2
        (_, Descending, Odd) -> (length files * 2) - 1

    renamings :: [(Text, Text)]
    renamings =
      filesSorted
        & mapWithIndex
          startNumber
          renameMode
          sortOrder
          ( \file index ->
              ( file
              , (if index < 0 then "_todo_" else "")
                  <> padNum padding index
                  <> pack (takeExtension $ unpack file)
              )
          )

    renamingsWithTemp :: [(Text, Maybe Text, Text)]
    renamingsWithTemp =
      renamings
        <&> ( \(file, target) ->
                ( file
                , if target `elem` files
                    then Just $ "_perspec_temp_" <> target
                    else Nothing
                , target
                )
            )

    renamingsBatch1 =
      renamingsWithTemp
        <&> ( \(file, tempTargetMb, target) ->
                if P.isNothing tempTargetMb
                  then (file, target)
                  else (file, fromMaybe "" tempTargetMb)
            )

    renamingsBatch2 =
      renamingsWithTemp
        & filter (\(_, tempTargetMb, _) -> P.isJust tempTargetMb)
          <&> (\(_, tempTargetMb, target) -> (fromMaybe "" tempTargetMb, target))
  in
    [renamingsBatch1]
      <> ( if null renamingsBatch2
             then []
             else [renamingsBatch2]
         )
