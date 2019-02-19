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
import Marbles as B
import Oracle as O

rep :: (a -> a) -> Int -> a -> a
rep f n a = L.foldl (\a _ -> f a) a [1..n]

list ns = L.foldl (\c n -> B.add n c) i ns
i = newCircle 0
main = hspec $ do
    describe "a circle " $ do
        it "is created with a head" $ do
            show i `shouldBe` "(0)"

        it "can insert a new marble after the head" $ do
            show (B.add 1 i) `shouldBe` "0 (1)"

        it "can insert several new marbles" $ do
            show (B.add 2 (B.add 1 i)) `shouldBe` "0 1 (2)"
            show (list [1..3]) `shouldBe` "0 1 2 (3)"

        it "can shift the head to the right" $ do
            show (right i) `shouldBe` "(0)"
            show (right (B.add 1 i)) `shouldBe` "(0) 1"
            show (right (list [1..3])) `shouldBe` "(0) 1 2 3"

        it "playing is shift once then adding" $ do
            show (B.play 1 i) `shouldBe` "0 (1)"
            show (B.play 2 (B.play 1 i)) `shouldBe` "0 (2) 1"
            show (B.play 3 (B.play 2 (B.play 1 i))) `shouldBe` "0 2 1 (3)"
            show (L.foldl (\c n -> B.play n c) i [1..7]) `shouldBe` "0 4 2 5 1 6 3 (7)"
            show (L.foldl (\c n -> B.play n c) i [1..22])`shouldBe`
                "0 16 8 17 4 18 9 19 2 20 10 21 5 (22) 11 1 12 6 13 3 14 7 15"

        it "can remove the head" $ do
            show (remove (B.add 1 i)) `shouldBe` "(1,(0))"
            show (remove (B.add 2 (B.add 1 i))) `shouldBe` "(2,(0) 1)"
            show (remove (B.play 2 (B.play 1 i))) `shouldBe` "(2,0 (1))"

        it "can shift the head to the left" $ do
            show (left i) `shouldBe` "(0)"
            show (left (B.add 1 i)) `shouldBe` "(0) 1"
            show (left (left (B.add 1 i))) `shouldBe` "0 (1)"
            show (left (list [1..3])) `shouldBe` "0 1 (2) 3"
            show (left (left (list [1..3]))) `shouldBe` "0 (1) 2 3"

        it "can shift left and right symmetrically" $ do
            let c0 = B.play 4 i
             in B.toList (B.right (B.right (B.left (B.left c0))))  `shouldBe` B.toList c0

        it "winning is shifting left seven times and then removing" $ do
            let c = (L.foldl (\c n -> B.play n c) i [1..22])
            
            show (win c)  `shouldBe` 
                "(9,0 16 8 17 4 18 (19) 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15)"

        it "has a count" $ do
            count (list [1..21]) `shouldBe` 22
            count (list [1..41]) `shouldBe` 42

    describe "properties" $ do
        describe "a shift is without loss" $ do
            it "for right shift" $ forAll (choose (1,1000)) $ \n ->
                forAll (choose (1,1000)) $ \m ->
                    let origin = B.play n i
                        shifted = rep B.right m origin 
                    in count origin == count shifted

            it "for left shift" $ forAll (choose (1,1000)) $ \n ->
                forAll (choose (1,1000)) $ \m ->
                    let origin = B.play n i
                        shifted = L.foldl (\c _ -> left c) origin ([1..m] :: [Int])
                    in count origin == count shifted

            it "right and left are symmetric" $ forAll (choose (1,2)) $ \n ->
                forAll (choose (1,10)) $ \m -> 
                    let circle = B.circle (B.moves n (B.game 5))
                in rep B.left m (rep B.right m circle) == circle

        describe "multiple of 23 rule" $ do
            it "increases if not multiple of 23" $ forAll (choose (2,100)) $ 
               \n -> let origin = list [1..n-1]
                         played = list [1..n]
                     in count played == count origin + 1
            it "decreases if  multiple of 23" $ forAll (elements (L.map (*23) [1..100])) $ 
                 \n ->let before = B.play (pred n) i
                          winner = snd (win before)
                       in count winner == (count before) - 1
        describe "should be the same as oracle" $ do 
            it "for multiples of 23" $ forAll (choose (1,10)) $
                \m -> let n = m * 23
                          g = moves n (B.game 5)
                          h = O.play n 5
                      in show (B.circle g) == (O.showCircle h)
            it "for any move" $ forAll (choose (90,94)) $
                \n -> let g = moves n (B.game 5)
                          h = O.play n 5
                      in show (B.circle g) == (O.showCircle h)


    describe "a game" $ do
        it "is created with an initial circle, and scores for n players" $ do
            let g = game 5
            show (B.circle g)  `shouldBe` "(0)"
            M.toList (B.scores g) `shouldBe` [(0,0),(1,0),(2,0),(3,0),(4,0)]
            B.marble g `shouldBe` 1
            B.player g `shouldBe` 0

        it "can be played a next move" $ do
            let g = move (game 5)
            show (B.circle g)  `shouldBe` "0 (1)"
            B.player g `shouldBe` 1
            B.marble g `shouldBe` 2

        it "can be played a series of moves" $ do
            let g = moves 10 (game 5)
            show (B.circle g)  `shouldBe` "0 8 4 9 2 (10) 5 1 6 3 7"
            B.player g `shouldBe` 0
            B.marble g `shouldBe` 11

        it "can be won by a player after a series of moves" $ do
            let g = moves 46 (game 5)
            M.toList (B.scores g)  `shouldBe` [(0,63),(1,0),(2,32),(3,0),(4,0)]


        it "can make some scores" $ do
            let result np ms = B.highScore (moves ms (game np))
            result 10  1618 `shouldBe` 8317
            result 13  7999 `shouldBe` 146373
            result 17  1104 `shouldBe` 2764
            result 21  6111 `shouldBe` 54718
            result 30  5807 `shouldBe` 37305
            result 468 71843  `shouldBe` 385820
            result 468 7184300  `shouldBe` 0

