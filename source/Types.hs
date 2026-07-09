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
  ($),
  (<>),
 )

import Text.Show (show)

import Brillo (Picture, Point)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  withText,
  (.!=),
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


data TransformBackend
  = HipBackend
  | FlatCVBackend
  deriving (Show, Eq)


instance FromJSON TransformBackend where
  parseJSON = withText "TransformBackend" $ \case
    "Hip" -> return HipBackend
    "FlatCV" -> return FlatCVBackend
    _ -> return FlatCVBackend


data RenameMode
  = Sequential
  | Even
  | Odd
  deriving (Show)


data RotationDirection
  = Clockwise
  | CounterClockwise
  deriving (Show, Eq)


data SortOrder
  = Ascending
  | Descending


data ExportMode
  = UnmodifiedExport
  | GrayscaleExport
  | BlackWhiteExport
  | BlackWhiteSmoothExport
  deriving (Show)


data UiComponent
  = Button
      { text :: Text
      , width :: Int
      , height :: Int
      }
  | Select
  deriving (Show)


data View = HomeView | ImageView | BannerView
  deriving (Show, Eq)


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
      , isFlipped :: Bool
      -- ^ Horizontal flip (EXIF orientations 2, 4, 5, 7)
      }
  deriving (Show)


-- | Edge index (0 = top, 1 = right, 2 = bottom, 3 = left)
newtype EdgeIndex = EdgeIndex Int
  deriving (Show, Eq)


data AppState = AppState
  { currentView :: View
  , tickCounter :: Int
  , corners :: [Corner]
  {- ^ Reversed to order of addition
  ^ (0, 0) is center of coordinate system
  -}
  , cornerDragged :: Maybe Corner
  -- ^ Currently dragged corner
  , edgeDragged :: Maybe EdgeIndex
  -- ^ Currently dragged edge (0=top, 1=right, 2=bottom, 3=left)
  , lastMousePos :: Maybe Point
  -- ^ Last mouse position during edge dragging
  , images :: [ImageData]
  , appWidth :: Int
  , appHeight :: Int
  , scaleFactor :: Float
  , transformBackend :: TransformBackend
  , isRegistered :: Bool
  , bannerIsVisible :: Bool
  , bannerUrlError :: Maybe Text
  -- ^ Set when opening the Buy License URL in a browser fails
  , sidebarWidth :: Int
  , uiComponents :: [UiComponent]
  , hoveredButton :: Maybe Int
  -- ^ Index of the button currently being hovered
  }


instance Show AppState where
  show appState =
    "AppState "
      <> ("{ currentView = " <> show appState.currentView)
      <> (", tickCounter = " <> show appState.tickCounter)
      <> (", corners = " <> show appState.corners)
      <> (", cornerDragged = " <> show appState.cornerDragged)
      <> (", edgeDragged = " <> show appState.edgeDragged)
      <> (", lastMousePos = " <> show appState.lastMousePos)
      <> (", images = " <> show appState.images)
      <> (", appWidth = " <> show appState.appWidth)
      <> (", appHeight = " <> show appState.appHeight)
      <> (", scaleFactor = " <> show appState.scaleFactor)
      <> (", transformBackend = " <> show appState.transformBackend)
      <> (", isRegistered = " <> show appState.isRegistered)
      <> (", bannerIsVisible = " <> show appState.bannerIsVisible)
      <> (", bannerUrlError = " <> show appState.bannerUrlError)
      <> (", sidebarWidth = " <> show appState.sidebarWidth)
      <> (", uiComponents = " <> show appState.uiComponents)
      <> (", hoveredButton = " <> show appState.hoveredButton)
      <> "}"


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
    , edgeDragged = Nothing
    , lastMousePos = Nothing
    , images = []
    , appWidth = appInitialWidth
    , appHeight = appInitialHeight
    , scaleFactor = 1
    , transformBackend = FlatCVBackend
    , isRegistered = False
    , bannerIsVisible = False
    , bannerUrlError = Nothing
    , sidebarWidth = sidebarInitialWidth
    , uiComponents =
        [ Button
            { text = "Save"
            , width = 160
            , height = 30
            }
        , Button
            { text = "Save Gray"
            , width = 160
            , height = 30
            }
        , Button
            { text = "Save BW"
            , width = 160
            , height = 30
            }
        , Button
            { text = "Save BW Smooth"
            , width = 160
            , height = 30
            }
        ]
    , hoveredButton = Nothing
    }
