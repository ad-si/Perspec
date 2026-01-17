module UtilsSpec where

import Protolude (Bool (False), ($))

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
        applyRotationToCorners selectedCorners 200 150 90.0 False
          `shouldBe` Corners
            { tlX = 30.0
            , tlY = 10.0
            , trX = 130.0
            , trY = 10.0
            , brX = 130.0
            , brY = 100.0
            , blX = 30.0
            , blY = 100.0
            }

      it "rotates corners correctly for 180 degree clockwise rotation" $ do
        applyRotationToCorners selectedCorners 200 150 180.0 False
          `shouldBe` Corners
            { tlX = 100.0
            , tlY = 30.0
            , trX = 190.0
            , trY = 30.0
            , brX = 190.0
            , brY = 130.0
            , blX = 100.0
            , blY = 130.0
            }

      it "rotates corners correctly for 270 degree clockwise rotation" $ do
        applyRotationToCorners selectedCorners 200 150 270.0 False
          `shouldBe` Corners
            { tlX = 20.0
            , tlY = 100.0
            , trX = 120.0
            , trY = 100.0
            , brX = 120.0
            , brY = 190.0
            , blX = 20.0
            , blY = 190.0
            }

      it "rotates corners correctly for negative rotations" $ do
        applyRotationToCorners selectedCorners 200 150 (-90.0) False
          `shouldBe` Corners
            { tlX = 20.0
            , tlY = 100.0
            , trX = 120.0
            , trY = 100.0
            , brX = 120.0
            , brY = 190.0
            , blX = 20.0
            , blY = 190.0
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

        applyRotationToCorners selCorners 1000 500 (-90.0) False
          `shouldBe` Corners
            { tlX = 81.81817626953125
            , tlY = 107.27276611328125
            , trX = 390.625
            , trY = 195.3125
            , brX = 300.9090881347656
            , brY = 914.5454559326172
            , blX = 109.375
            , blY = 802.734375
            }
