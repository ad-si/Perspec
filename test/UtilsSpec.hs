module UtilsSpec where

import Protolude (Bool (False, True), ($))

import Test.Hspec (Spec, describe, it, shouldBe)

import FlatCV (Corners (..))
import Utils (applyRotationToCorners)


spec :: Spec
spec = do
  describe "Utils" $ do
    let
      -- Image: 200 x 150
      -- Document: 90 x 100
      --
      --                     200
      --  +------------------------------------+
      --  |     tl(10,20)     tr(100,20)       |
      --  |       +-------------+              |
      --  |       |             |              | 150
      --  |       +-------------+              |
      --  |     bl(10,120)    br(100,120)      |
      --  +------------------------------------+
      --
      selectedCorners =
        Corners
          { tlX = 10.0
          , tlY = 20.0
          , trX = 100.0
          , trY = 20.0
          , brX = 100.0
          , brY = 120.0
          , blX = 10.0
          , blY = 120.0
          }

    describe "applyRotationToCorners rotation logic" $ do
      it "returns the same corners when rotation is 0" $ do
        applyRotationToCorners selectedCorners 200 150 0.0 False
          `shouldBe` selectedCorners

      it "rotates corners correctly for 90 degree clockwise rotation" $ do
        -- For 90° CW: srcWidth=200 (displayed width = rawHeight), srcHeight=150 (displayed height = rawWidth)
        -- Raw image is 150×200, displayed is 200×150
        -- Inverse: displayed (x,y) → raw (y, rawHeight-1-x) = (y, 199-x)
        applyRotationToCorners selectedCorners 200 150 90.0 False
          `shouldBe` Corners
            { tlX = 20.0
            , tlY = 99.0
            , trX = 120.0
            , trY = 99.0
            , brX = 120.0
            , brY = 189.0
            , blX = 20.0
            , blY = 189.0
            }

      it "rotates corners correctly for 180 degree clockwise rotation" $ do
        -- For 180°: dimensions unchanged, formula (w-1-x, h-1-y) = (199-x, 149-y)
        applyRotationToCorners selectedCorners 200 150 180.0 False
          `shouldBe` Corners
            { tlX = 99.0
            , tlY = 29.0
            , trX = 189.0
            , trY = 29.0
            , brX = 189.0
            , brY = 129.0
            , blX = 99.0
            , blY = 129.0
            }

      it "rotates corners correctly for 270 degree clockwise rotation" $ do
        -- For 270° (= -90° = 90° CCW): formula (h-1-y, x) = (149-y, x)
        applyRotationToCorners selectedCorners 200 150 270.0 False
          `shouldBe` Corners
            { tlX = 29.0
            , tlY = 10.0
            , trX = 129.0
            , trY = 10.0
            , brX = 129.0
            , brY = 100.0
            , blX = 29.0
            , blY = 100.0
            }

      it "rotates corners correctly for negative rotations" $ do
        -- -90° is the same as 270°
        applyRotationToCorners selectedCorners 200 150 (-90.0) False
          `shouldBe` Corners
            { tlX = 29.0
            , tlY = 10.0
            , trX = 129.0
            , trY = 10.0
            , brX = 129.0
            , brY = 100.0
            , blX = 29.0
            , blY = 100.0
            }

      it "works for bigger numbers" $ do
        let
          selCorners =
            Corners
              { tlX = 197.265625
              , tlY = 109.375
              , trX = 892.7272338867188
              , trY = 81.81817626953125
              , brX = 804.6875
              , brY = 390.625
              , blX = 85.45454406738281
              , blY = 300.9090881347656
              }

        -- For -90° with srcWidth=1000, srcHeight=500
        -- rotatePoint270 (x, y) = (h-1-y, x) = (499-y, x)
        applyRotationToCorners selCorners 1000 500 (-90.0) False
          `shouldBe` Corners
            { tlX = 198.09091186523438
            , tlY = 85.45454406738281
            , trX = 389.625
            , trY = 197.265625
            , brX = 417.18182373046875
            , brY = 892.7272338867188
            , blX = 108.375
            , blY = 804.6875
            }

    -- Test all 8 EXIF orientations to prevent regressions.
    -- EXIF orientations and their transformations:
    --   1: Normal (no transformation)
    --   2: Horizontal flip
    --   3: 180° rotation
    --   4: Vertical flip (180° + horizontal flip)
    --   5: Transpose (90° CW + horizontal flip)
    --   6: 90° CW rotation
    --   7: Transverse (90° CCW + horizontal flip)
    --   8: 90° CCW rotation (270° CW)
    describe "applyRotationToCorners for all EXIF orientations" $ do
      it "EXIF 1: Normal - no transformation" $ do
        applyRotationToCorners selectedCorners 200 150 0.0 False
          `shouldBe` selectedCorners

      it "EXIF 2: Horizontal flip only" $ do
        -- Flip mirrors X: newX = 199 - x
        applyRotationToCorners selectedCorners 200 150 0.0 True
          `shouldBe` Corners
            { tlX = 189.0
            , tlY = 20.0
            , trX = 99.0
            , trY = 20.0
            , brX = 99.0
            , brY = 120.0
            , blX = 189.0
            , blY = 120.0
            }

      it "EXIF 3: 180° rotation" $ do
        -- Formula: (w-1-x, h-1-y) = (199-x, 149-y)
        applyRotationToCorners selectedCorners 200 150 180.0 False
          `shouldBe` Corners
            { tlX = 99.0
            , tlY = 29.0
            , trX = 189.0
            , trY = 29.0
            , brX = 189.0
            , brY = 129.0
            , blX = 99.0
            , blY = 129.0
            }

      it "EXIF 4: Vertical flip (180° + horizontal flip)" $ do
        -- First unflip (mirror X): newX = 199 - x
        -- Then inverse rotate 180°: (w-1-x, h-1-y)
        applyRotationToCorners selectedCorners 200 150 180.0 True
          `shouldBe` Corners
            { tlX = 100.0
            , tlY = 29.0
            , trX = 10.0
            , trY = 29.0
            , brX = 10.0
            , brY = 129.0
            , blX = 100.0
            , blY = 129.0
            }

      it "EXIF 5: Transpose (90° CW + horizontal flip)" $ do
        -- srcWidth=200 (displayed width), srcHeight=150 (displayed height)
        -- First unflip: newX = 199 - x
        -- Then inverse rotate 90° with corner remapping
        applyRotationToCorners selectedCorners 200 150 90.0 True
          `shouldBe` Corners
            { tlX = 20.0
            , tlY = 100.0
            , trX = 120.0
            , trY = 100.0
            , brX = 120.0
            , brY = 10.0
            , blX = 20.0
            , blY = 10.0
            }

      it "EXIF 6: 90° CW rotation" $ do
        -- srcWidth=200 (displayed width = rawHeight), srcHeight=150 (displayed height = rawWidth)
        -- Inverse: displayed (x,y) → raw (y, 199-x)
        applyRotationToCorners selectedCorners 200 150 90.0 False
          `shouldBe` Corners
            { tlX = 20.0
            , tlY = 99.0
            , trX = 120.0
            , trY = 99.0
            , brX = 120.0
            , brY = 189.0
            , blX = 20.0
            , blY = 189.0
            }

      it "EXIF 7: Transverse (90° CCW + horizontal flip)" $ do
        -- srcWidth=200 (displayed width), srcHeight=150 (displayed height)
        -- First unflip: newX = 199 - x
        -- Then inverse rotate -90° with corner remapping
        applyRotationToCorners selectedCorners 200 150 (-90.0) True
          `shouldBe` Corners
            { tlX = 29.0
            , tlY = 189.0
            , trX = 129.0
            , trY = 189.0
            , brX = 129.0
            , brY = 99.0
            , blX = 29.0
            , blY = 99.0
            }

      it "EXIF 8: 90° CCW rotation (270° CW)" $ do
        -- srcWidth=200 (displayed width), srcHeight=150 (displayed height)
        -- Formula: (h-1-y, x) = (149-y, x)
        applyRotationToCorners selectedCorners 200 150 (-90.0) False
          `shouldBe` Corners
            { tlX = 29.0
            , tlY = 10.0
            , trX = 129.0
            , trY = 10.0
            , brX = 129.0
            , brY = 100.0
            , blX = 29.0
            , blY = 100.0
            }
