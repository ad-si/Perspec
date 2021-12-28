import Test.Hspec

import Protolude

import Rename
import Types


main :: IO ()
main = hspec $ do
  describe "Perspec" $ do
    describe "Rename" $ do
      it "renames files according to natural sort and avoids collisions" $ do
        let
          files = ["1.txt", "10.txt", "2.txt"]
          batches =
            [ [ ("1.txt","0.txt")
              , ("2.txt", "_perspec_temp_1.txt")
              , ("10.txt","_perspec_temp_2.txt")
              ]
            , [ ("_perspec_temp_1.txt", "1.txt")
              , ("_perspec_temp_2.txt","2.txt")
              ]
            ]

        getRenamingBatches 0 Sequential files `shouldBe` batches


      it "renames files with even page numbers" $ do
        let
          files = ["a.txt", "c.txt", "e.txt"]
          batches =
            [ [ ("a.txt","0.txt")
              , ("c.txt", "2.txt")
              , ("e.txt","4.txt")
              ]
            ]

        getRenamingBatches 0 Even files `shouldBe` batches


      it "rounds to next even page number" $ do
        let
          files = ["a.txt", "c.txt", "e.txt"]
          batches =
            [ [ ("a.txt","2.txt")
              , ("c.txt", "4.txt")
              , ("e.txt","6.txt")
              ]
            ]

        getRenamingBatches 1 Even files `shouldBe` batches


      it "renames files with odd page numbers" $ do
        let
          files = ["b.txt", "d.txt", "f.txt"]
          batches =
            [ [ ("b.txt","1.txt")
              , ("d.txt", "3.txt")
              , ("f.txt","5.txt")
              ]
            ]

        getRenamingBatches 0 Odd files `shouldBe` batches
