module Types where

import Protolude as P

import Data.Aeson
import Graphics.Gloss


data Config = Config
  { licenseKey :: Text
  }
  deriving (Generic, Show)


-- | Necessary to make fields optional without using a Maybe type
instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    licenseKey <- o .:? "licenseKey" .!= ""
    pure $ Config {..}


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


data RenameMode
  = Sequential
  | Even
  | Odd
  deriving (Show)


data SortOrder
  = Ascending
  | Descending


data ExportMode
  = UnmodifiedExport
  | GrayscaleExport
  | BlackWhiteExport


data UiComponent
  = Button
      { text :: Text
      , width :: Int
      , height :: Int
      , bgColor :: Int
      }
  | Select
  deriving (Show)


-- | State of app
data AppState = AppState
  { tickCounter :: Int

  , corners :: [Corner] -- ^ Reversed to order of addition
  , cornerDragged :: Maybe Corner  -- ^ Currently dragged corner
  , image :: Picture

  , appWidth :: Int
  , appHeight :: Int

  , imgWidthOrig :: Int
  , imgHeightOrig :: Int

  , imgWidthTrgt :: Int
  , imgHeightTrgt :: Int

  , rotation :: Float

  , inputPath :: FilePath
  , outputPath :: FilePath
  , scaleFactor :: Float

  , isRegistered :: Bool
  , bannerIsVisible :: Bool

  , sidebarWidth :: Int
  , sidebarColor :: Int

  , uiComponents :: [UiComponent]
  }
  deriving Show


appInitialWidth, appInitialHeight, sidebarInitialWidth :: Int
appInitialWidth = 1280
appInitialHeight = 960
sidebarInitialWidth = 150


initialState :: AppState
initialState = AppState
  { tickCounter = 0

  , corners = []
  , cornerDragged = Nothing
  , image = Blank

  , appWidth = appInitialWidth
  , appHeight = appInitialHeight

  , imgWidthOrig = 0
  , imgHeightOrig = 0

  , imgWidthTrgt = 0
  , imgHeightTrgt = 0

  , rotation = 0

  , inputPath = ""
  , outputPath = ""
  , scaleFactor = 1

  , isRegistered = False
  , bannerIsVisible = False

  , sidebarWidth = sidebarInitialWidth
  , sidebarColor = 0

  , uiComponents =
      [ Button
          { text = "Save"
          , width = 110
          , height = 30
          , bgColor = 0
          }
      , Button
          { text = "Save Gray"
          , width = 110
          , height = 30
          , bgColor = 0
          }
      , Button
          { text = "Save BW"
          , width = 110
          , height = 30
          , bgColor = 0
          }
      ]
  }
