import Test.Hspec
import Steps
import Data.Map as M
import Data.List as L

small = [(C,A)
        ,(C,F)
        ,(A,B)
        ,(A,D)
        ,(B,E)
        ,(D,E)
        ,(F,E)]

main = hspec $ do
    let g = graph small
    describe "graph" $ do
        it "creates a graph of all steps" $ do
            let l = L.sort $ M.toList $ g
            l `shouldBe` [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])]

