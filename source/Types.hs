module Types where

import Protolude as P (
  Applicative (pure),
  Bool (False),
  Bounded,
  Enum,
  Eq,
  FilePath,
  Float,
  Generic,
  Int,
  Maybe (Nothing),
  Monad (return),
  Show,
  Text,
  length,
  ($),
  (<>),
 )

import Text.Show (show)

import Brillo (Picture, Point)
import Codec.Picture (Image, PixelRGBA8)
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


-- | Color processing applied to the corrected image
data ExportStyle
  = ColorStyle
  | GrayStyle
  | BlackWhiteStyle
  | BlackWhiteSmoothStyle
  deriving (Show, Eq, Enum, Bounded)


exportStyleName :: ExportStyle -> Text
exportStyleName = \case
  ColorStyle -> "Color"
  GrayStyle -> "Gray"
  BlackWhiteStyle -> "BW"
  BlackWhiteSmoothStyle -> "BW Smooth"


data OutputFormat
  = ImageFormat
  | -- | Searchable PDF with the text recognized by Tesseract
    PdfFormat
  deriving (Show, Eq, Enum, Bounded)


outputFormatName :: OutputFormat -> Text
outputFormatName = \case
  ImageFormat -> "Image"
  PdfFormat -> "PDF"


-- | Language of the text Tesseract recognizes for the searchable PDF
data OcrLanguage
  = English
  | German
  | Spanish
  deriving (Show, Eq, Enum, Bounded)


ocrLanguageName :: OcrLanguage -> Text
ocrLanguageName = \case
  English -> "English"
  German -> "German"
  Spanish -> "Spanish"


-- | A word recognized by Tesseract
data OcrWord = OcrWord
  { text :: Text
  , left :: Int
  {- ^ Pixel coordinates of the bounding box,
  ^ measured from the top left corner of the image
  -}
  , top :: Int
  , right :: Int
  , bottom :: Int
  , baseline :: Int
  -- ^ Y coordinate of the baseline at the left edge of the word
  , confidence :: Float
  -- ^ Between 0 and 100
  }
  deriving (Show, Eq)


-- | Page of a searchable PDF
data PdfPage = PdfPage
  { image :: Image PixelRGBA8
  , ocrWords :: [OcrWord]
  -- ^ Recognized on the 'image'
  }


{-| Searchable PDF that is assembled from several images.
It's written once all its pages were added,
which is the case when an image starts a new PDF,
after the last image, or when the window is closed.
-}
data PdfDocument = PdfDocument
  { number :: Int
  -- ^ Counts the PDFs exported in this session, starting at 1
  , outputPath :: FilePath
  , pages :: [PdfPage]
  -- ^ In reading order
  }


instance Show PdfDocument where
  show pdfDocument =
    "PdfDocument "
      <> ("{ number = " <> show pdfDocument.number)
      <> (", outputPath = " <> show pdfDocument.outputPath)
      <> (", pages = " <> show (length pdfDocument.pages) <> " pages")
      <> "}"


-- | Whether an image starts a new PDF or is added to the open one
data PdfAction
  = NewPdf
  | AddToPdf
  deriving (Show, Eq)


data ExportSettings = ExportSettings
  { style :: ExportStyle
  , outputFormat :: OutputFormat
  , ocrLanguage :: OcrLanguage
  -- ^ Only used for the 'PdfFormat'
  }
  deriving (Show, Eq)


data UiComponent
  = -- | Heading of a group of components
    Label
      { text :: Text
      , width :: Int
      }
  | -- | Segmented control to select the 'ExportStyle'
    StyleSelect
      { width :: Int
      , height :: Int
      }
  | -- | Segmented control to select the 'OutputFormat'
    OutputFormatSelect
      { width :: Int
      , height :: Int
      }
  | -- | Drop-down menu to select the 'OcrLanguage'
    OcrLanguageSelect
      { width :: Int
      , height :: Int
      }
  | -- | Horizontal line to group the components
    Separator {width :: Int}
  | {-| Exports the image with the current 'ExportSettings'.
    For the 'PdfFormat', it starts a new PDF with the image.
    -}
    SaveButton
      { width :: Int
      , height :: Int
      }
  | -- | Adds the image to the open PDF. Hidden if there is none.
    AddToPdfButton
      { width :: Int
      , height :: Int
      }
  deriving (Show)


-- | Button of the zoom controls in the bottom right corner of the image view
data ZoomControl
  = ZoomOut
  | -- | Shows the zoom level and resets it when clicked
    ZoomReset
  | ZoomIn
  deriving (Show, Eq, Enum, Bounded)


-- | Clickable part of a sidebar component or of the zoom controls
data UiTarget
  = StyleOption ExportStyle
  | OutputFormatOption OutputFormat
  | OcrLanguageSelectTarget
  | -- | Option of the open language select
    OcrLanguageOption OcrLanguage
  | SaveTarget
  | PdfActionTarget PdfAction
  | ZoomControlTarget ZoomControl
  deriving (Show, Eq)


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
  -- ^ Width of the window in logical units (see 'uiScale')
  , appHeight :: Int
  -- ^ Height of the window in logical units (see 'uiScale')
  , uiScale :: Float
  {- ^ Factor between the logical units the interface is laid out in
  ^ and the window coordinates of the display it's shown on.
  ^ 1.75 on a Windows display set to 175 % scaling, 1 on a normal one.
  -}
  , scaleFactor :: Float
  -- ^ Factor between the pixels of the image and the logical units
  , zoom :: Float
  -- ^ Factor the image is shown larger than it fits the image view
  , panOffset :: Point
  {- ^ Offset of the image's center from the image view's center
  ^ in pixels of the image, with the y axis pointing up
  -}
  , panDragged :: Maybe Point
  -- ^ Last mouse position while the image is dragged
  , transformBackend :: TransformBackend
  , isRegistered :: Bool
  , bannerIsVisible :: Bool
  , bannerUrlError :: Maybe Text
  -- ^ Set when opening the Buy License URL in a browser fails
  , sidebarWidth :: Int
  , uiComponents :: [UiComponent]
  , hoveredButton :: Maybe Int
  -- ^ Index of the button currently being hovered in the home view
  , hoveredUiTarget :: Maybe UiTarget
  , exportSettings :: ExportSettings
  , ocrLanguageSelectIsOpen :: Bool
  , pdfDocument :: Maybe PdfDocument
  -- ^ PDF the pages of the 'PdfFormat' are added to
  , hasExported :: Bool
  -- ^ Whether an image was exported in this session
  , fontPath :: Maybe FilePath
  {- ^ Font to render the UI text with, resolved at startup.
  ^ 'Nothing' if the system has no usable font, in which case
  ^ text is skipped instead of crashing the renderer.
  -}
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
      <> (", uiScale = " <> show appState.uiScale)
      <> (", scaleFactor = " <> show appState.scaleFactor)
      <> (", zoom = " <> show appState.zoom)
      <> (", panOffset = " <> show appState.panOffset)
      <> (", panDragged = " <> show appState.panDragged)
      <> (", transformBackend = " <> show appState.transformBackend)
      <> (", isRegistered = " <> show appState.isRegistered)
      <> (", bannerIsVisible = " <> show appState.bannerIsVisible)
      <> (", bannerUrlError = " <> show appState.bannerUrlError)
      <> (", sidebarWidth = " <> show appState.sidebarWidth)
      <> (", uiComponents = " <> show appState.uiComponents)
      <> (", hoveredButton = " <> show appState.hoveredButton)
      <> (", hoveredUiTarget = " <> show appState.hoveredUiTarget)
      <> (", exportSettings = " <> show appState.exportSettings)
      <> (", ocrLanguageSelectIsOpen = " <> show appState.ocrLanguageSelectIsOpen)
      <> (", pdfDocument = " <> show appState.pdfDocument)
      <> (", hasExported = " <> show appState.hasExported)
      <> (", fontPath = " <> show appState.fontPath)
      <> "}"


appInitialWidth, appInitialHeight, sidebarInitialWidth :: Int
appInitialWidth = 1280
appInitialHeight = 960
sidebarInitialWidth = 260


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
    , uiScale = 1
    , scaleFactor = 1
    , zoom = 1
    , panOffset = (0, 0)
    , panDragged = Nothing
    , transformBackend = FlatCVBackend
    , isRegistered = False
    , bannerIsVisible = False
    , bannerUrlError = Nothing
    , sidebarWidth = sidebarInitialWidth
    , uiComponents =
        [ Label{text = "Output Style:", width = 240}
        , StyleSelect{width = 240, height = 30}
        , Label{text = "Output Format:", width = 240}
        , OutputFormatSelect{width = 240, height = 30}
        , Label{text = "OCR Language:", width = 240}
        , OcrLanguageSelect{width = 240, height = 30}
        , Separator{width = 240}
        , SaveButton{width = 240, height = 30}
        , AddToPdfButton{width = 240, height = 30}
        ]
    , hoveredButton = Nothing
    , hoveredUiTarget = Nothing
    , exportSettings =
        ExportSettings
          { style = ColorStyle
          , outputFormat = ImageFormat
          , ocrLanguage = English
          }
    , ocrLanguageSelectIsOpen = False
    , pdfDocument = Nothing
    , hasExported = False
    , fontPath = Nothing
    }
