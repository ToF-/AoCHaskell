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
import Data.Map as M
import Data.List as L
import Marbles

list ns = L.foldl (\c n -> add n c) i ns
i = newCircle 0
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
            show (L.foldl (\c n -> play n c) i [1..7]) `shouldBe` "0 4 2 5 1 6 3 (7)"
            show (L.foldl (\c n -> play n c) i [1..22])`shouldBe`
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
            show (left (left (list [1..3]))) `shouldBe` "0 (1) 2 3"

        it "winning is shifting left seven times and then removing" $ do
            let c = (L.foldl (\c n -> play n c) i [1..22])
            
            show (win c)  `shouldBe` 
                "(9,0 16 8 17 4 18 (19) 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15)"

        it "has a count" $ do
            count (list [1..21]) `shouldBe` 22
            count (list [1..41]) `shouldBe` 42

    describe "properties" $ do
        describe "a shift is without loss" $ do
            it "for right shift" $ forAll (choose (1,1000)) $ \n ->
                forAll (choose (1,1000)) $ \m ->
                    let origin = play n i
                        shifted = L.foldl (\c _ -> right c) origin ([1..m] :: [Int])
                    in count origin == count shifted

            it "for left shift" $ forAll (choose (1,1000)) $ \n ->
                forAll (choose (1,1000)) $ \m ->
                    let origin = play n i
                        shifted = L.foldl (\c _ -> left c) origin ([1..m] :: [Int])
                    in count origin == count shifted

        describe "multiple of 23 rule" $ do
            it "increases if not multiple of 23" $ forAll (choose (2,100)) $ 
               \n -> let origin = list [1..n-1]
                         played = list [1..n]
                     in count played == count origin + 1
            it "decreases if  multiple of 23" $ forAll (elements (L.map (*23) [1..100])) $ 
                 \n ->let before = play (pred n) i
                          winner = snd (win before)
                       in count winner == (count before) - 1

    describe "a game" $ do
        it "is created with an initial circle, and scores for n players" $ do
            let g = game 5
            show (circle g)  `shouldBe` "(0)"
            M.toList (scores g) `shouldBe` [(0,0),(1,0),(2,0),(3,0),(4,0)]
            marble g `shouldBe` 1
            player g `shouldBe` 0

        it "can be played a next move" $ do
            let g = move (game 5)
            show (circle g)  `shouldBe` "0 (1)"
            player g `shouldBe` 1
            marble g `shouldBe` 2

        it "can be played a series of moves" $ do
            let g = moves 10 (game 5)
            show (circle g)  `shouldBe` "0 8 4 9 2 (10) 5 1 6 3 7"
            player g `shouldBe` 0
            marble g `shouldBe` 11

        it "can be won by a player after a series of moves" $ do
            let g = moves 46 (game 5)
            M.toList (scores g)  `shouldBe` [(0,0),(1,0),(2,32),(3,0),(4,0)]
            show (circle g)  `shouldBe` "0 16 8 17 4 18 (19) 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15"
            player g `shouldBe` 3
            marble g `shouldBe` 24
            highScore g `shouldBe` 32

        it "can make some scores" $ do
            let result np ms = highScore (moves ms (game np))
            result 10  1618 `shouldBe` 8317
            result 13  7999 `shouldBe` 146373
            result 17  1104 `shouldBe` 2764
            result 21  6111 `shouldBe` 54718
            result 30  5807 `shouldBe` 37305


