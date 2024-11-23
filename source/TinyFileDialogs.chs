module TinyFileDialogs
( -- * The functions
  beep
, notifyPopup
, messageBox
, inputBox
, saveFileDialog
, openFileDialog
, selectFolderDialog
, colorChooser
  -- * Message box options
, IconType(..)
, MessageBox
, OK(..)
, OKCancel(..)
, YesNo(..)
, YesNoCancel(..)
) where

import Protolude (
  Applicative (pure),
  Bool (..),
  (.),
  Eq ((==)),
  die,
  maxBound,
  minBound,
  return,
  identity,
  Functor (fmap),
  IO,
  Int,
  Maybe (Just, Nothing),
  Monad ((>>=)),
  Ord,
  Semigroup ((<>)),
  fromIntegral,
  show,
  ($),
  (<>),
  (>>),
  Show,Read,Enum,Bounded
  )

import Data.List (lookup)
import           Data.Char          (toLower)
import qualified Data.Text          as T
import           Foreign            (Ptr, Word8, nullPtr, peekArray, withArray, withArrayLen, withMany)
import           Foreign.C          (CInt, CString, CUChar)

#ifdef WINDOWS
import qualified Data.ByteString    as B
import qualified Data.Text.Encoding as TE
#else
import           Foreign.C          (peekCString, withCString)
#endif

#include "tinyfiledialogs.h"

{#context prefix = "tinyfd_" #}

withCText :: T.Text -> (CString -> IO a) -> IO a
#ifdef WINDOWS
withCText = B.useAsCString . TE.encodeUtf8
#else
withCText = withCString . T.unpack
#endif

withCShowLower :: (Show a) => a -> (CString -> IO b) -> IO b
withCShowLower = withCText . T.pack . fmap toLower . show

withCMaybeText :: Maybe T.Text -> (CString -> IO a) -> IO a
withCMaybeText mt f = case mt of
  Nothing -> f nullPtr
  Just t  -> withCText t f

peekMaybeText :: CString -> IO (Maybe T.Text)
peekMaybeText cstr = if cstr == nullPtr
  then pure Nothing
#ifdef WINDOWS
  else fmap (Just . TE.decodeUtf8) $ B.packCString cstr
#else
  else fmap (Just . T.pack) $ peekCString cstr
#endif

peekMaybeTextMultiple :: CString -> IO (Maybe [T.Text])
peekMaybeTextMultiple = fmap (fmap $ T.splitOn (T.singleton '|')) . peekMaybeText

withCTexts :: [T.Text] -> ((CInt, Ptr CString) -> IO a) -> IO a
withCTexts ts f = withMany withCText ts $ \ptrs ->
  withArrayLen ptrs $ \len ptr -> f (fromIntegral len, ptr)

class (Enum a, Bounded a) => MessageBox a where
  messageBoxType :: a -> T.Text
  messageBoxValue :: a -> Int

data IconType = Info | Warning | Error | Question
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data OK = OK
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance MessageBox OK where
  messageBoxType _ = T.pack "ok"
  messageBoxValue OK = 1

data OKCancel = OC_OK | OC_Cancel
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance MessageBox OKCancel where
  messageBoxType _ = T.pack "okcancel"
  messageBoxValue OC_Cancel = 0
  messageBoxValue OC_OK     = 1

data YesNo = YN_Yes | YN_No
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance MessageBox YesNo where
  messageBoxType _ = T.pack "yesno"
  messageBoxValue YN_No  = 0
  messageBoxValue YN_Yes = 1

data YesNoCancel = YNC_Yes | YNC_No | YNC_Cancel
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance MessageBox YesNoCancel where
  messageBoxType _ = T.pack "yesnocancel"
  messageBoxValue YNC_Cancel = 0
  messageBoxValue YNC_Yes    = 1
  messageBoxValue YNC_No     = 2

{#fun messageBox as c_messageBox
  { withCText*      `T.Text'     -- ^ title
  , withCText*      `T.Text'     -- ^ message, may contain @\\n@ and @\\t@
  , withCText*      `T.Text'     -- ^ @"ok" "okcancel" "yesno" "yesnocancel"@
  , withCShowLower* `IconType'   -- ^ 'Info', 'Warning', 'Error', 'Question'
  ,                 `Int'        -- ^ default button: 0 for cancel/no, 1 for ok/yes, 2 for no in yesnocancel
  } -> `Int' -- ^ 0 for cancel/no, 1 for ok/yes, 2 for no in yesnocancel
#}

{#fun notifyPopup
  { withCText*      `T.Text'   -- ^ title
  , withCText*      `T.Text'   -- ^ message, may contain @\\n@ and @\\t@
  , withCShowLower* `IconType' -- ^ 'Info', 'Warning', 'Error'
  } -> `()'
#}

{#fun beep {} -> `()' #}

messageBox
  :: (MessageBox a)
  => T.Text -- ^ title
  -> T.Text -- ^ message, may contain @\\n@ and @\\t@
  -> IconType -- ^ 'Info', 'Warning', 'Error', 'Question'
  -> a -- ^ default button
  -> IO a
messageBox ttl msg icon dflt = do
  n <- c_messageBox ttl msg (messageBoxType dflt) icon (messageBoxValue dflt)
  case lookup n [ (messageBoxValue x, x) | x <- [minBound .. maxBound] ] of
    Just x  -> pure x
    Nothing -> die $ "TinyFileDialogs.messageBox: "
      <> "internal error; unrecognized return value " <> show n

{#fun inputBox
  { withCText*      `T.Text'       -- ^ title
  , withCText*      `T.Text'       -- ^ message, may NOT contain @\\n@ and @\\t@ on windows
  , withCMaybeText* `Maybe T.Text' -- ^ default input, if 'Nothing' it's a passwordBox
  } -> `Maybe T.Text' peekMaybeText* -- ^ returns 'Nothing' on cancel
#}

{#fun saveFileDialog
  { withCText*  `T.Text'    -- ^ title
  , withCText*  `T.Text'    -- ^ default path and file
  , withCTexts* `[T.Text]'& -- ^ filter patterns, @["*.jpg","*.png"]@
  , withCText*  `T.Text'    -- ^ single filter description, @"text files"@
  } -> `Maybe T.Text' peekMaybeText* -- ^ returns 'Nothing' on cancel
#}

{#fun openFileDialog
  { withCText*  `T.Text'    -- ^ title
  , withCText*  `T.Text'    -- ^ default path and file
  , withCTexts* `[T.Text]'& -- ^ filter patterns, @["*.jpg","*.png"]@
  , withCText*  `T.Text'    -- ^ single filter description, @"text files"@
  ,             `Bool'      -- ^ allow multiple selects
  } -> `Maybe [T.Text]' peekMaybeTextMultiple* -- ^ returns 'Nothing' on cancel
#}

{#fun selectFolderDialog
  { withCText* `T.Text' -- ^ title
  , withCText* `T.Text' -- ^ default path
  } -> `Maybe T.Text' peekMaybeText* -- ^ returns 'Nothing' on cancel
#}

{#fun colorChooser as c_colorChooser
  { withCText*      `T.Text'
  , withCMaybeText* `Maybe T.Text'
  , identity        `Ptr CUChar'
  , identity        `Ptr CUChar'
  } -> `Maybe T.Text' peekMaybeText* -- ^ returns 'Nothing' on cancel
#}

withColor :: (Word8, Word8, Word8) -> (Ptr CUChar -> IO a) -> IO a
withColor (r, g, b) = withArray $ fmap fromIntegral [r, g, b]

colorChooser
  :: T.Text                           -- ^ title
  -> (Word8, Word8, Word8)            -- ^ default RGB color
  -> IO (Maybe (Word8, Word8, Word8)) -- ^ returns 'Nothing' on cancel
colorChooser title color = withColor color $ \ptr -> do
  res <- c_colorChooser title Nothing ptr ptr
  case res of
    Nothing -> pure Nothing
    Just _  -> fmap
      ((\case
        [r, g, b] -> Just (r, g, b)
        _        -> Nothing
        ) . fmap fromIntegral)
      (peekArray 3 ptr)
