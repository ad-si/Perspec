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

        (getRenamingBatches 0 Sequential Ascending files) `shouldBe` batches

      it "renames files in descending order" $ do
        let
          files = ["1.txt", "10.txt", "2.txt"]
          batches =
            [ [ ("1.txt","_perspec_temp_2.txt")
              , ("2.txt", "_perspec_temp_1.txt")
              , ("10.txt","0.txt")
              ]
            , [ ("_perspec_temp_2.txt","2.txt")
              , ("_perspec_temp_1.txt", "1.txt")
              ]
            ]

        (getRenamingBatches 2 Sequential Descending files) `shouldBe` batches


      describe "renames files with even page numbers" $ do
        it "directly with even page number" $ do
          let
            files = ["a.txt", "c.txt", "e.txt"]
            batches =
              [ [ ("a.txt","0.txt")
                , ("c.txt", "2.txt")
                , ("e.txt","4.txt")
                ]
              ]

          (getRenamingBatches 0 Even Ascending files) `shouldBe` batches


        it "rounds to next even page number" $ do
          let
            files = ["a.txt", "c.txt", "e.txt"]
            batches =
              [ [ ("a.txt","2.txt")
                , ("c.txt", "4.txt")
                , ("e.txt","6.txt")
                ]
              ]

          (getRenamingBatches 1 Even Ascending files) `shouldBe` batches


      it "renames files with odd page numbers" $ do
        let
          files = ["b.txt", "d.txt", "f.txt"]
          batches =
            [ [ ("b.txt","1.txt")
              , ("d.txt", "3.txt")
              , ("f.txt","5.txt")
              ]
            ]

        (getRenamingBatches 0 Odd Ascending files) `shouldBe` batches
        (getRenamingBatches 1 Odd Ascending files) `shouldBe` batches

      it "renames files with odd page numbers in descending order" $ do
        let
          files = ["8.txt", "10.txt", "9.txt"]
          batches =
            [ [ ("8.txt","7.txt")
              , ("9.txt", "5.txt")
              , ("10.txt","3.txt")
              ]
            ]

        (getRenamingBatches 7 Odd Descending files) `shouldBe` batches
        (getRenamingBatches 8 Odd Descending files) `shouldBe` batches

      it "prefixes pages with negative numbers with \"_todo_\"" $ do
        let
          files = ["8.txt", "10.txt", "9.txt"]
          batches =
            [ [ ("8.txt","1.txt")
              , ("9.txt", "_todo_-1.txt")
              , ("10.txt","_todo_-3.txt")
              ]
            ]

        (getRenamingBatches 1 Odd Descending files) `shouldBe` batches
