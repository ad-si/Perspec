{-# language ApplicativeDo #-}
{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
{-# language DataKinds #-}

module Correct where

import Protolude as P

-- hip
import Graphics.Image hiding ((!))

-- hmatrix
import Numeric.LinearAlgebra (linearSolve, linearSolveSVD, (!), (><))

-- lens
import Control.Lens hiding (transform)

-- linear
import Linear


determineSize :: V4 (V2 Double) -> Sz2
determineSize (V4 c1 c2 c3 c4) = Sz ((round height) :. (round width))
  where
    diagonalA = c3 ^-^ c1
    diagonalB = c4 ^-^ c2
    V2 width height = (abs diagonalA ^+^ abs diagonalB) / 2


-- /* Calculates coefficients of perspective transformation
--  * which maps (xi,yi) to (ui,vi), (i=1,2,3,4):
--  *
--  *      c00*xi + c01*yi + c02
--  * ui = ---------------------
--  *      c20*xi + c21*yi + c22
--  *
--  *      c10*xi + c11*yi + c12
--  * vi = ---------------------
--  *      c20*xi + c21*yi + c22
--  *
--  * Coefficients are calculated by solving linear system:
--  * / x0 y0  1  0  0  0 -x0*u0 -y0*u0 \ /c00\ /u0\
--  * | x1 y1  1  0  0  0 -x1*u1 -y1*u1 | |c01| |u1|
--  * | x2 y2  1  0  0  0 -x2*u2 -y2*u2 | |c02| |u2|
--  * | x3 y3  1  0  0  0 -x3*u3 -y3*u3 |.|c10|=|u3|,
--  * |  0  0  0 x0 y0  1 -x0*v0 -y0*v0 | |c11| |v0|
--  * |  0  0  0 x1 y1  1 -x1*v1 -y1*v1 | |c12| |v1|
--  * |  0  0  0 x2 y2  1 -x2*v2 -y2*v2 | |c20| |v2|
--  * \  0  0  0 x3 y3  1 -x3*v3 -y3*v3 / \c21/ \v3/
--  *
--  * where:
--  *   cij - matrix coefficients, c22 = 1
--  */
-- }
calculatePerspectiveTransform :: V4 (V2 Double) -> V4 (V2 Double) -> M33 Double
calculatePerspectiveTransform s d =
  let
    a = (8><8)
        [ s ^. _x . _x  , s ^. _x . _y , 1 ,            0 ,            0 , 0, negate(s ^. _x . _x * d ^. _x . _x), negate(s ^. _x . _y * d ^. _x . _x)
        , s ^. _y . _x  , s ^. _y . _y , 1 ,            0 ,            0 , 0, negate(s ^. _y . _x * d ^. _y . _x), negate(s ^. _y . _y * d ^. _y . _x)
        , s ^. _z . _x  , s ^. _z . _y , 1 ,            0 ,            0 , 0, negate(s ^. _z . _x * d ^. _z . _x), negate(s ^. _z . _y * d ^. _z . _x)
        , s ^. _w . _x  , s ^. _w . _y , 1 ,            0 ,            0 , 0, negate(s ^. _w . _x * d ^. _w . _x), negate(s ^. _w . _y * d ^. _w . _x)
        ,            0  ,            0 , 0 , s ^. _x . _x , s ^. _x . _y , 1, negate(s ^. _x . _x * d ^. _x . _y), negate(s ^. _x . _y * d ^. _x . _y)
        ,            0  ,            0 , 0 , s ^. _y . _x , s ^. _y . _y , 1, negate(s ^. _y . _x * d ^. _y . _y), negate(s ^. _y . _y * d ^. _y . _y)
        ,            0  ,            0 , 0 , s ^. _z . _x , s ^. _z . _y , 1, negate(s ^. _z . _x * d ^. _z . _y), negate(s ^. _z . _y * d ^. _z . _y)
        ,            0  ,            0 , 0 , s ^. _w . _x , s ^. _w . _y , 1, negate(s ^. _w . _x * d ^. _w . _y), negate(s ^. _w . _y * d ^. _w . _y)
        ]
    b = (8><1)
        [ d ^. _x . _x
        , d ^. _y . _x
        , d ^. _z . _x
        , d ^. _w . _x
        , d ^. _x . _y
        , d ^. _y . _y
        , d ^. _z . _y
        , d ^. _w . _y
        ]
    m = fromMaybe (linearSolveSVD a b) (linearSolve a b)
  in V3
      (V3 (m ! 0 ! 0) (m ! 1 ! 0) (m ! 2 ! 0))
      (V3 (m ! 3 ! 0) (m ! 4 ! 0) (m ! 5 ! 0))
      (V3 (m ! 6 ! 0) (m ! 7 ! 0) 1)
