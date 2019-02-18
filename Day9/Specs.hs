import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import CircularList as C
import Marbles
import Data.Map as M

main = hspec $ do
    describe "a circular list" $ do
        it "is created with one item" $ do
            showCList (clist 42) `shouldBe` "[]42[]"
        it "can add items" $ do
            showCList (C.add 17 (clist 42)) `shouldBe` "[42]17[]"
        it "can shift right" $ do
            showCList (C.shiftRight (clist 42)) `shouldBe` "[]42[]"
            showCList (C.shiftRight (C.add 17 (clist 42))) `shouldBe` "[17]42[]"


    describe "marble circle" $ do
        let splay n = showCircle $ play n 9 
        describe  "can be show with current between ( )" $ do
            it "for 0" $ do
                splay 0 `shouldBe` "(0)"
            it "for 1" $ do
                splay 1 `shouldBe` "0 (1)"
            it "for 2" $ do
                splay 2 `shouldBe` "0 (2) 1"
            it "for 3" $ do
                splay 3 `shouldBe` "0 2 1 (3)"
            it "for 4" $ do
                splay 4 `shouldBe` "0 (4) 2 1 3"
            it "for 5" $ do
                splay 5 `shouldBe` "0 4 2 (5) 1 3"
            it "for 6" $ do
                splay 6 `shouldBe` "0 4 2 5 1 (6) 3"
            it "for 7" $ do
                splay 7 `shouldBe` "0 4 2 5 1 6 3 (7)"
            it "for 22" $ do
                splay 22 `shouldBe` "0 16 8 17 4 18 9 19 2 20 10 21 5 (22) 11 1 12 6 13 3 14 7 15"

        it "has special rules for marble multiple of 23" $ do
            splay 23 `shouldBe` "0 16 8 17 4 18 (19) 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15"    

        it "has n players with each a score" $ do
            M.toList (scores (circle 4)) `shouldBe` 
                [(0,0),(1,0),(2,0),(3,0)]
            let sc = M.lookup 4 (scores (play 25 9)) 
            sc `shouldBe` Just 32
        it "has a highest score" $ do
            highScore (play 25 9) `shouldBe` 32
          --  highScore (play 1618 10) `shouldBe` 8317
          --  highScore (play 7999 13) `shouldBe` 146373
          --  highScore (play 1104 17) `shouldBe` 2764
           --  highScore (play 6111 21) `shouldBe` 54718
           --  highScore (play 5807 30) `shouldBe` 37305  -- Too Slow
            -- highScore (play 71843 468) `shouldBe` 0
        it "will not contain a multiple of 23" $ property $
            \n-> 
                all (\x -> (x `mod`  23) /= 0 || x == 0) 
                    (marbles (play (abs n) 1)) 
            
            
