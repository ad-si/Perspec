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
          { tl_x = 10.0
          , tl_y = 20.0
          , tr_x = 100.0
          , tr_y = 20.0
          , br_x = 100.0
          , br_y = 120.0
          , bl_x = 10.0
          , bl_y = 120.0
          }

    describe "applyRotationToCorners rotation logic" $ do
      it "returns the same corners when rotation is 0" $ do
        applyRotationToCorners selectedCorners 200 150 0.0 False
          `shouldBe` selectedCorners

      it "rotates corners correctly for 90 degree clockwise rotation" $ do
        applyRotationToCorners selectedCorners 200 150 90.0 False
          `shouldBe` Corners
            { tl_x = 30.0
            , tl_y = 10.0
            , tr_x = 130.0
            , tr_y = 10.0
            , br_x = 130.0
            , br_y = 100.0
            , bl_x = 30.0
            , bl_y = 100.0
            }

      it "rotates corners correctly for 180 degree clockwise rotation" $ do
        applyRotationToCorners selectedCorners 200 150 180.0 False
          `shouldBe` Corners
            { tl_x = 100.0
            , tl_y = 30.0
            , tr_x = 190.0
            , tr_y = 30.0
            , br_x = 190.0
            , br_y = 130.0
            , bl_x = 100.0
            , bl_y = 130.0
            }

      it "rotates corners correctly for 270 degree clockwise rotation" $ do
        applyRotationToCorners selectedCorners 200 150 270.0 False
          `shouldBe` Corners
            { tl_x = 20.0
            , tl_y = 100.0
            , tr_x = 120.0
            , tr_y = 100.0
            , br_x = 120.0
            , br_y = 190.0
            , bl_x = 20.0
            , bl_y = 190.0
            }

      it "rotates corners correctly for negative rotations" $ do
        applyRotationToCorners selectedCorners 200 150 (-90.0) False
          `shouldBe` Corners
            { tl_x = 20.0
            , tl_y = 100.0
            , tr_x = 120.0
            , tr_y = 100.0
            , br_x = 120.0
            , br_y = 190.0
            , bl_x = 20.0
            , bl_y = 190.0
            }

      it "works for bigger numbers" $ do
        let
          selCorners =
            Corners
              { tl_x = 197.265625
              , tl_y = 109.375
              , tr_x = 892.7272338867188
              , tr_y = 81.81817626953125
              , br_x = 804.6875
              , br_y = 390.625
              , bl_x = 85.45454406738281
              , bl_y = 300.9090881347656
              }

        applyRotationToCorners selCorners 1000 500 (-90.0) False
          `shouldBe` Corners
            { tl_x = 81.81817626953125
            , tl_y = 107.27276611328125
            , tr_x = 390.625
            , tr_y = 195.3125
            , br_x = 300.9090881347656
            , br_y = 914.5454559326172
            , bl_x = 109.375
            , bl_y = 802.734375
            }
