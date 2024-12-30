module SimpleCV where

import Protolude (
  Double,
  fromIntegral,
  identity,
  Int,
  IO,
  Ptr,
  return,
  (*),
  (>>=),
 )

import Foreign.C.Types (CUChar)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

#include "simplecv.h"
#include "perspectivetransform.h"

data Corners = Corners
  { tl_x :: Int
  , tl_y :: Int
  , tr_x :: Int
  , tr_y :: Int
  , br_x :: Int
  , br_y :: Int
  , bl_x :: Int
  , bl_y :: Int
  }
{#pointer *Corners as CornersPtr foreign -> Corners#}

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
  }
{#pointer *Matrix3x3 as Matrix3x3Ptr foreign -> Matrix3x3#}

instance Storable Corners where
  sizeOf _ = 8 * sizeOf (0 :: Int)
  alignment _ = alignment (0 :: Int)
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

{#fun grayscale
  {               `Int'        -- ^ width
  ,               `Int'        -- ^ height
  , identity      `Ptr CUChar' -- ^ Original image data
  } -> `Ptr CUChar' castPtr    -- ^ Grayscale image data
#}

{#fun otsu_threshold_rgba
  {               `Int'        -- ^ width
  ,               `Int'        -- ^ height
  , identity      `Ptr CUChar' -- ^ Original image data
  } -> `Ptr CUChar' castPtr    -- ^ Thresholded image data
#}

{#fun calculate_perspective_transform as ^
  { castPtr `Ptr Corners'  -- ^ Source points
  , castPtr `Ptr Corners'  -- ^ Destination points
  } -> `Ptr Matrix3x3' castPtr -- ^ Transformation matrix
#}
