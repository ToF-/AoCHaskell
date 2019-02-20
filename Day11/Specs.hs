import Test.Hspec
import Power
import Data.Array 

a :: Array Int (Array Int Int)
a = array (1,6) [(1,array (1,6) (zip [1..6] [31, 2, 4,33, 5,36]))
                ,(2,array (1,6) (zip [1..6] [12,26, 9,10,29,25]))
                ,(3,array (1,6) (zip [1..6] [13,17,21,22,20,18]))
                ,(4,array (1,6) (zip [1..6] [24,23,15,16,14,19]))
                ,(5,array (1,6) (zip [1..6] [30, 8,28,27,11, 7]))
                ,(6,array (1,6) (zip [1..6] [ 1,35,34, 3,32,36]))]

main = hspec $ do
    describe "power level" $ do
        it "depends on x,y and serial" $ do
            powerLevel 8 3 5 `shouldBe` 4
            powerLevel 57 122 79 `shouldBe` -5
            powerLevel 39 217 196 `shouldBe` 0
            powerLevel 71 101 153 `shouldBe` 4

    describe "square 3x3 level" $ do
        it "sum the power levels on a 3 x 3 square" $ do
            square3x3Level 18 33 45 `shouldBe` [[4, 4, 4]
                                            ,[3, 3, 4]
                                            ,[1, 2, 4]]

    describe "power square" $ do
        it "tells the square with the largest power level in the grid" $ do
            powerSquare 18 `shouldBe` (29,(33,45))
            powerSquare 42 `shouldBe` (30,(21,61))
            powerSquare 4151 `shouldBe` (30,(20,46))

    describe "power grid" $ do
        let g = grid 18
        it "creates a 300x300 grid from a given serial" $ do
            g ! 1 ! 1 `shouldBe` powerLevel 18 1 1
            g ! 300 ! 300 `shouldBe` powerLevel 18 300 300

        describe "square level" $ do
            it "tells the power level of any square area" $ do
                let sq = square3x3Level 18 33 45
                squareLevel g 33 45 3 `shouldBe` (sum . map sum) sq

        describe "squares " $ do
            it "tells all the possibles squares within a grid of a given size" $ do
                squares 1 `shouldBe` [(1,1,1)]
                squares 2 `shouldBe` [(1,1,1),(1,1,2),(1,2,1),(2,1,1),(2,2,1)]
--         describe "best square" $ do
--             it "tells the square with the largest power value in a grid given a serial" $ do
--                 -- bestSquare g `shouldBe` (113,(90,269,16)) -- too long
-- -- see https://en.wikipedia.org/wiki/Summed-area_table#/media/File:Integral_image_application_example.svg
        
        describe "partial sums" $ do
            it "tells for a cell x,y the sum of all the cells on the left and above x,y + cell x,y" $ do
                let a = array (1,6) [(1,array (1,6) (zip [1..6] [ 31,  2,  4, 33,  5, 36]))
                                    ,(2,array (1,6) (zip [1..6] [ 12, 26,  9, 10, 29, 25]))
                                    ,(3,array (1,6) (zip [1..6] [ 13, 17, 21, 22, 20, 18]))
                                    ,(4,array (1,6) (zip [1..6] [ 24, 23, 15, 16, 14, 19]))
                                    ,(5,array (1,6) (zip [1..6] [ 30,  8, 28, 27, 11,  7]))
                                    ,(6,array (1,6) (zip [1..6] [  1, 35, 34,  3, 32,  6]))]

                let b = array (1,6) [(1,array (1,6) (zip [1..6] [ 31, 33, 37, 70, 75,111]))
                                    ,(2,array (1,6) (zip [1..6] [ 43, 71, 84,127,161,222]))
                                    ,(3,array (1,6) (zip [1..6] [ 56,101,135,200,254,333]))
                                    ,(4,array (1,6) (zip [1..6] [ 80,148,197,278,346,444]))
                                    ,(5,array (1,6) (zip [1..6] [110,186,263,371,450,555]))
                                    ,(6,array (1,6) (zip [1..6] [111,222,333,444,555,666]))]
                
                partialSums a  `shouldBe` b
