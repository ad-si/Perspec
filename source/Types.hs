module Types where

import Protolude as P

import Graphics.Gloss


type Corner = Point

type CornersTup = (Corner, Corner, Corner, Corner)


-- | Projection map from corner to corner
type ProjMap =
  ( (Corner, Corner)
  , (Corner, Corner)
  , (Corner, Corner)
  , (Corner, Corner)
  )


-- | Not used at the moment
-- rotateProjMap :: Float -> ProjMap -> ProjMap
-- rotateProjMap rotation pMap@((f1,t1), (f2,t2), (f3,t3), (f4,t4)) =
--   case rotation of
--     -90 -> ((f1,t4), (f2,t1), (f3,t2), (f4,t3))
--     90  -> ((f1,t2), (f1,t3), (f1,t4), (f1,t1))
--     180 -> ((f1,t3), (f1,t4), (f1,t1), (f1,t2))
--     _   -> pMap


data ConversionMode
  = CallConversion
  | SpawnConversion


-- | State of app (list of corners is reversed to order of addition)
data AppState = AppState
  { corners :: [Corner]
  , image :: Picture

  , imgViewWidth :: Int
  , imgViewHeight :: Int

  , imgWidthOrig :: Int
  , imgHeightOrig :: Int

  , imgWidthTrgt :: Int
  , imgHeightTrgt :: Int

  , rotation :: Float

  , inputPath :: FilePath
  , outputPath :: FilePath
  , scaleFactor :: Float
  }
  deriving Show


initialState :: AppState
initialState = AppState
  { corners = []
  , image = Blank

  , imgViewWidth = 1280
  , imgViewHeight = 960

  , imgWidthOrig = 0
  , imgHeightOrig = 0

  , imgWidthTrgt = 0
  , imgHeightTrgt = 0

  , rotation = 0

  , inputPath = ""
  , outputPath = ""
  , scaleFactor = 1
  }
