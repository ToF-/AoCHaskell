import Test.Hspec
import Marbles

main = hspec $ do
    describe "marble circle" $ do
        describe "has a current marble" $ do
            it "initially it's marble 0" $ do
                current circle `shouldBe` 0
            
