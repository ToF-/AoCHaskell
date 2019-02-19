-- [-] (0)
-- [1]  0 (1)
-- [2]  0 (2) 1 
-- [3]  0  2  1 (3)
-- [4]  0 (4) 2  1  3 
-- [5]  0  4  2 (5) 1  3 
-- [6]  0  4  2  5  1 (6) 3 
-- [7]  0  4  2  5  1  6  3 (7)
-- [8]  0 (8) 4  2  5  1  6  3  7 
import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import Marbles

list ns = foldl (\c n -> add n c) i ns
i = circle 0
main = hspec $ do
    describe "a circle " $ do
        it "is created with a head" $ do
            show i `shouldBe` "(0)"

        it "can insert a new marble after the head" $ do
            show (add 1 i) `shouldBe` "0 (1)"

        it "can insert several new marbles" $ do
            show (add 2 (add 1 i)) `shouldBe` "0 1 (2)"
            show (list [1..3]) `shouldBe` "0 1 2 (3)"

        it "can shift the head to the right" $ do
            show (right i) `shouldBe` "(0)"
            show (right (add 1 i)) `shouldBe` "(0) 1"
            show (right (list [1..3])) `shouldBe` "(0) 1 2 3"

        it "playing is shift once then adding" $ do
            show (play 1 i) `shouldBe` "0 (1)"
            show (play 2 (play 1 i)) `shouldBe` "0 (2) 1"
            show (play 3 (play 2 (play 1 i))) `shouldBe` "0 2 1 (3)"
            show (foldl (\c n -> play n c) i [1..7]) `shouldBe` "0 4 2 5 1 6 3 (7)"
            show (foldl (\c n -> play n c) i [1..22])`shouldBe`
                "0 16 8 17 4 18 9 19 2 20 10 21 5 (22) 11 1 12 6 13 3 14 7 15"

        it "can remove the head" $ do
            show (remove (add 1 i)) `shouldBe` "(1,(0))"
            show (remove (add 2 (add 1 i))) `shouldBe` "(2,(0) 1)"
            show (remove (play 2 (play 1 i))) `shouldBe` "(2,0 (1))"

        it "can shift the head to the left" $ do
            show (left i) `shouldBe` "(0)"
            show (left (add 1 i)) `shouldBe` "(0) 1"
            show (left (left (add 1 i))) `shouldBe` "0 (1)"
            show (left (list [1..3])) `shouldBe` "0 1 (2) 3"
