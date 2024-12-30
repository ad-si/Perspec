module Correct where

import Protolude as P (
  Double,
  Fractional ((/)),
  Num (abs),
  RealFrac (round),
 )

-- hip
import Graphics.Image (Ix2 ((:.)), Sz (Sz), Sz2)

-- linear
import Linear (Additive ((^+^), (^-^)), V2 (..), V4 (..))


determineSize :: V4 (V2 Double) -> Sz2
determineSize (V4 c1 c2 c3 c4) = Sz (round height :. round width)
  where
    diagonalA = c3 ^-^ c1
    diagonalB = c4 ^-^ c2
    V2 width height = (abs diagonalA ^+^ abs diagonalB) / 2
