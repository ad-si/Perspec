module FlatCV where

import Protolude (
  Double,
  Eq,
  fromIntegral,
  identity,
  Int,
  IO,
  Bool,
  Ptr,
  show,
  realToFrac,
  Show,
  return,
  (*),
  ($),
  (<>),
  (>>=),
 )

import Data.Text qualified as T
import Data.Text (Text)
import Foreign.C.Types (CUChar)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))
import Text.Printf (printf)

#include "flatcv.h"

data Corners = Corners
  { tl_x :: Double
  , tl_y :: Double
  , tr_x :: Double
  , tr_y :: Double
  , br_x :: Double
  , br_y :: Double
  , bl_x :: Double
  , bl_y :: Double
  } deriving (Show, Eq)
{#pointer *Corners as CornersPtr foreign -> Corners#}


prettyShowCorners :: Corners -> Text
prettyShowCorners Corners{..} =
  (show (tl_x, tl_y) <> " " <> show (tr_x, tr_y)) <> "\n" <>
  (show (bl_x, bl_y) <> " " <> show (br_x, br_y))


data Matrix3x3 = Matrix3x3
  { m00 :: Double
  , m01 :: Double
  , m02 :: Double
  , m10 :: Double
  , m11 :: Double
  , m12 :: Double
  , m20 :: Double
  , m21 :: Double
  , m22 :: Double
  } deriving (Show)
{#pointer *Matrix3x3 as Matrix3x3Ptr foreign -> Matrix3x3#}


prettyShowMatrix3x3 :: Matrix3x3 -> Text
prettyShowMatrix3x3 Matrix3x3{..} =
  let
    fNum num = printf "% .5f " num
  in
    T.pack $
      fNum m00 <> " " <> fNum m01 <> " " <> fNum m02 <> "\n" <>
      fNum m10 <> " " <> fNum m11 <> " " <> fNum m12 <> "\n" <>
      fNum m20 <> " " <> fNum m21 <> " " <> fNum m22


instance Storable Corners where
  sizeOf _ = 8 * sizeOf (0.0 :: Double)
  alignment _ = alignment (0.0 :: Double)
  peek ptr = do
    tl_x <- peekByteOff ptr 0
    tl_y <- peekByteOff ptr 8
    tr_x <- peekByteOff ptr 16
    tr_y <- peekByteOff ptr 24
    br_x <- peekByteOff ptr 32
    br_y <- peekByteOff ptr 40
    bl_x <- peekByteOff ptr 48
    bl_y <- peekByteOff ptr 56
    return Corners{..}
  poke ptr Corners{..} = do
    pokeByteOff ptr 0 tl_x
    pokeByteOff ptr 8 tl_y
    pokeByteOff ptr 16 tr_x
    pokeByteOff ptr 24 tr_y
    pokeByteOff ptr 32 br_x
    pokeByteOff ptr 40 br_y
    pokeByteOff ptr 48 bl_x
    pokeByteOff ptr 56 bl_y

instance Storable Matrix3x3 where
  sizeOf _ = 9 * sizeOf (0.0 :: Double)
  alignment _ = alignment (0.0 :: Double)
  peek ptr = do
    m00 <- peekByteOff ptr 0
    m01 <- peekByteOff ptr 8
    m02 <- peekByteOff ptr 16
    m10 <- peekByteOff ptr 24
    m11 <- peekByteOff ptr 32
    m12 <- peekByteOff ptr 40
    m20 <- peekByteOff ptr 48
    m21 <- peekByteOff ptr 56
    m22 <- peekByteOff ptr 64
    return Matrix3x3{..}
  poke ptr Matrix3x3{..} = do
    pokeByteOff ptr 0 m00
    pokeByteOff ptr 8 m01
    pokeByteOff ptr 16 m02
    pokeByteOff ptr 24 m10
    pokeByteOff ptr 32 m11
    pokeByteOff ptr 40 m12
    pokeByteOff ptr 48 m20
    pokeByteOff ptr 56 m21
    pokeByteOff ptr 64 m22

{#fun fcv_grayscale
  {               `Int'        -- ^ width
  ,               `Int'        -- ^ height
  , identity      `Ptr CUChar' -- ^ Original image data
  } -> `Ptr CUChar' castPtr    -- ^ Grayscale image data
#}

{#fun fcv_apply_gaussian_blur
  {               `Int'        -- ^ width
  ,               `Int'        -- ^ height
  ,               `Double'     -- ^ radius
  , identity      `Ptr CUChar' -- ^ Original image data
  } -> `Ptr CUChar' castPtr    -- ^ Blurred image data
#}

{#fun fcv_grayscale_stretch as ^
  {               `Int'        -- ^ width
  ,               `Int'        -- ^ height
  , identity      `Ptr CUChar' -- ^ Original image data
  } -> `Ptr CUChar' castPtr    -- ^ Grayscale image data
#}

{#fun fcv_otsu_threshold_rgba
  {               `Int'        -- ^ width
  ,               `Int'        -- ^ height
  ,               `Bool'       -- ^ whether to use double thresholding
  , identity      `Ptr CUChar' -- ^ Original image data
  } -> `Ptr CUChar' castPtr    -- ^ Thresholded image data
#}

{#fun fcv_calculate_perspective_transform as ^
  { castPtr `Ptr Corners'  -- ^ Source points
  , castPtr `Ptr Corners'  -- ^ Destination points
  } -> `Ptr Matrix3x3' castPtr -- ^ Transformation matrix
#}

{#fun fcv_apply_matrix_3x3 as ^
  {          `Int'           -- ^ width of the input image
  ,          `Int'           -- ^ height of the input image
  , identity `Ptr CUChar'    -- ^ Original image data
  ,          `Int'           -- ^ width of the output image
  ,          `Int'           -- ^ height of the output image
  , castPtr  `Ptr Matrix3x3' -- ^ Transformation matrix
  } -> `Ptr CUChar' castPtr -- ^ Transformed image data
#}

{#fun fcv_bw_smart as ^
  {               `Int'        -- ^ width
  ,               `Int'        -- ^ height
  ,               `Bool'       -- ^ whether to use double thresholding
  , identity      `Ptr CUChar' -- ^ Original image data
  } -> `Ptr CUChar' castPtr    -- ^ Anti-aliased black and white image data
#}
