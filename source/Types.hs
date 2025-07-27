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

import Brillo (Picture, Point)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )


data Config = Config
  { licenseKey :: Text
  , transformBackendFlag :: TransformBackend
  }
  deriving (Generic, Show)


-- | Necessary to make fields optional without using a Maybe type
instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    licenseKey <- o .:? "licenseKey" .!= ""
    transformBackendFlag <- o .:? "transformBackend" .!= FlatCVBackend
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


data TransformBackend
  = ImageMagickBackend
  | HipBackend
  | FlatCVBackend
  deriving (Show, Eq)


instance FromJSON TransformBackend where
  parseJSON = withText "TransformBackend" $ \case
    "ImageMagick" -> return ImageMagickBackend
    "Hip" -> return HipBackend
    "FlatCV" -> return FlatCVBackend
    _ -> return FlatCVBackend


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
  | BlackWhiteSmoothExport


data UiComponent
  = Button
      { text :: Text
      , width :: Int
      , height :: Int
      , bgColor :: Int
      }
  | Select
  deriving (Show)


data View = HomeView | ImageView | BannerView
  deriving (Show)


data ImageData
  = ImageToLoad {filePath :: FilePath}
  | ImageData
      { inputPath :: FilePath
      , outputPath :: FilePath
      , width :: Int
      , height :: Int
      , widthTarget :: Int
      , heightTarget :: Int
      , content :: Picture
      , rotation :: Float
      }
  deriving (Show)


-- | State of app
data AppState = AppState
  { currentView :: View
  , tickCounter :: Int
  , corners :: [Corner]
  -- ^ Reversed to order of addition
  -- ^ (0, 0) is center of coordinate system
  , cornerDragged :: Maybe Corner
  -- ^ Currently dragged corner
  , images :: [ImageData]
  , appWidth :: Int
  , appHeight :: Int
  , scaleFactor :: Float -- TODO: Should be smaller than 1
  , transformBackend :: TransformBackend
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
sidebarInitialWidth = 180


initialState :: AppState
initialState =
  AppState
    { currentView = HomeView
    , tickCounter = 0
    , corners = []
    , cornerDragged = Nothing
    , images = []
    , appWidth = appInitialWidth
    , appHeight = appInitialHeight
    , scaleFactor = 1
    , transformBackend = FlatCVBackend
    , isRegistered = False
    , bannerIsVisible = False
    , sidebarWidth = sidebarInitialWidth
    , sidebarColor = 0
    , uiComponents =
        [ Button
            { text = "Save"
            , width = 160
            , height = 30
            , bgColor = 0
            }
        , Button
            { text = "Save Gray"
            , width = 160
            , height = 30
            , bgColor = 0
            }
        , Button
            { text = "Save BW"
            , width = 160
            , height = 30
            , bgColor = 0
            }
        , Button
            { text = "Save BW Smooth"
            , width = 160
            , height = 30
            , bgColor = 0
            }
        ]
    }
