import Test.Hspec
import Power

main = hspec $ do
    describe "power level" $ do
        it "depends on x,y and serial" $ do
            powerLevel 8 3 5 `shouldBe` 4
            powerLevel 57 122 79 `shouldBe` -5
            powerLevel 39 217 196 `shouldBe` 0
            powerLevel 71 101 153 `shouldBe` 4

    describe "square level" $ do
        it "sum the power levels on a 3 x 3 square" $ do
            squareLevel 18 33 45 `shouldBe` [[4, 4, 4]
                                            ,[3, 3, 4]
                                            ,[1, 2, 4]]

    describe "power square" $ do
        it "tells the square with the largest power level in the grid" $ do
            powerSquare 18 `shouldBe` (29,(33,45))
            powerSquare 42 `shouldBe` (30,(21,61))
            powerSquare 4151 `shouldBe` (30,(20,46))
