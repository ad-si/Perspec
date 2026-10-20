{-# LANGUAGE CPP #-}

{-| Files macOS opens with the app,
e.g. when they are dropped onto its icon in the Dock.
On other platforms, no such files are delivered.
-}
module OpenFiles (
  installOpenFilesHandler,
  setOpenFilesCallback,
) where


#if defined(darwin_HOST_OS)
import Protolude (
  Bool,
  Eq ((/=)),
  IO,
  fromIntegral,
  mapM,
  (<&>),
  (=<<),
 )

import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (FunPtr, Ptr)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import System.IO (FilePath)


foreign import ccall "perspec_install_open_files_handler"
  c_installOpenFilesHandler :: IO CBool


foreign import ccall "perspec_set_open_files_callback"
  c_setOpenFilesCallback :: FunPtr (CInt -> Ptr CString -> IO ()) -> IO ()


foreign import ccall "wrapper"
  wrapOpenFilesCallback ::
    (CInt -> Ptr CString -> IO ()) -> IO (FunPtr (CInt -> Ptr CString -> IO ()))


{-| Receive the files macOS opens with the app.
Must be called before GLFW is initialized.
Returns 'False' if they can't be received.
-}
installOpenFilesHandler :: IO Bool
installOpenFilesHandler = c_installOpenFilesHandler <&> (/= 0)


{-| Call the function with each batch of opened files,
starting with the ones that were opened before.
-}
setOpenFilesCallback :: ([FilePath] -> IO ()) -> IO ()
setOpenFilesCallback onOpenFiles = do
  fileSystemEncoding <- getFileSystemEncoding
  let
    callback count pathsPtr = do
      cPaths <- peekArray (fromIntegral count) pathsPtr
      onOpenFiles =<< mapM (GHC.peekCString fileSystemEncoding) cPaths

  -- Never freed, as it's called until the app exits
  c_setOpenFilesCallback =<< wrapOpenFilesCallback callback
#else
import Protolude (Bool (True), IO, pure)
import System.IO (FilePath)


installOpenFilesHandler :: IO Bool
installOpenFilesHandler = pure True


setOpenFilesCallback :: ([FilePath] -> IO ()) -> IO ()
setOpenFilesCallback _ = pure ()
#endif
