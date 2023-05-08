module Types where

import Protolude as P (
  Applicative (pure),
  Bool (False),
  Eq,
  FilePath,
  Float,
  Generic,
  Int,
  Maybe (Nothing),
  Monad (return),
  Show,
  Text,
  fst,
  snd,
  ($),
 )

import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Graphics.Gloss (Picture (Blank), Point)


data Config = Config
  { licenseKey :: Text
  , transformAppFlag :: TransformApp
  }
  deriving (Generic, Show)


-- | Necessary to make fields optional without using a Maybe type
instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    licenseKey <- o .:? "licenseKey" .!= ""
    transformAppFlag <- o .:? "transformAppFlag" .!= ImageMagick
    pure $ Config{..}


type Corner = Point


type CornersTup = (Corner, Corner, Corner, Corner)


-- | Projection map from corner to corner
type ProjMap =
  ( (Corner, Corner)
  , (Corner, Corner)
  , (Corner, Corner)
  , (Corner, Corner)
  )


data Coordinate = Coordinate
  { x :: Float
  , y :: Float
  }
  deriving (Show, Eq)


instance FromJSON Coordinate where
  parseJSON = withObject "Coordinate" $ \o -> do
    x <- o .: "x"
    y <- o .: "y"
    pure $ Coordinate{..}


coordToCornersTup :: [Coordinate] -> CornersTup
coordToCornersTup coordinates =
  case coordinates of
    [c1, c2, c3, c4] ->
      ( (c1.x, c1.y)
      , (c2.x, c2.y)
      , (c3.x, c3.y)
      , (c4.x, c4.y)
      )
    _ -> ((0, 0), (0, 0), (0, 0), (0, 0))


cornersTupToCoord :: CornersTup -> [Coordinate]
cornersTupToCoord (c1, c2, c3, c4) =
  [ Coordinate (P.fst c1) (P.snd c1)
  , Coordinate (P.fst c2) (P.snd c2)
  , Coordinate (P.fst c3) (P.snd c3)
  , Coordinate (P.fst c4) (P.snd c4)
  ]


{-| Not used at the moment
rotateProjMap :: Float -> ProjMap -> ProjMap
rotateProjMap rotation pMap@((f1,t1), (f2,t2), (f3,t3), (f4,t4)) =
  case rotation of
    -90 -> ((f1,t4), (f2,t1), (f3,t2), (f4,t3))
    90  -> ((f1,t2), (f1,t3), (f1,t4), (f1,t1))
    180 -> ((f1,t3), (f1,t4), (f1,t1), (f1,t2))
    _   -> pMap
-}
data ConversionMode
  = CallConversion
  | SpawnConversion


data TransformApp
  = ImageMagick
  | Hip
  deriving (Show, Eq)


instance FromJSON TransformApp where
  parseJSON = withText "TransformApp" $ \case
    "Hip" -> return Hip
    _ -> return ImageMagick


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
  , corners :: [Corner]
  -- ^ Reversed to order of addition
  -- ^ (0, 0) is center of coordinate system
  , cornerDragged :: Maybe Corner
  -- ^ Currently dragged corner
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
  , scaleFactor :: Float -- TODO: Should be smaller than 1
  , transformApp :: TransformApp
  , isRegistered :: Bool
  , bannerIsVisible :: Bool
  , sidebarWidth :: Int
  , sidebarColor :: Int
  , uiComponents :: [UiComponent]
  }
  deriving (Show)


appInitialWidth, appInitialHeight, sidebarInitialWidth :: Int
appInitialWidth = 1280
appInitialHeight = 960
sidebarInitialWidth = 150


initialState :: AppState
initialState =
  AppState
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
    , transformApp = ImageMagick
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
