import Test.Hspec
import Steps
import Data.Map as M
import Data.List as L

main = hspec $ do
    let g = graph small
    describe "graph" $ do
        it "creates a graph of all steps" $ do
            let l = L.sort $ M.toList $ g
            l `shouldBe` 
                [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])]

    describe "target" $ do
        it "tells the final step of the graph" $ do
            target g `shouldBe` E

    describe "previous steps" $ do
        it "tells the previous steps not yet being done" $ do
            previous g [E] E `shouldBe` [B,D,F]
            previous g [E,B,D,F] B `shouldBe` [A]
            previous g [E,B,D,F,A] D `shouldBe` []


    describe "backward" $ do
        it "tells all the previous steps" $ do
            backward g E `shouldBe` []

small = [(C,A)
        ,(C,F)
        ,(A,B)
        ,(A,D)
        ,(B,E)
        ,(D,E)
        ,(F,E)]
