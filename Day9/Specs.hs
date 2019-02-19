import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import Marbles

main = hspec $ do
    describe "a circle " $ do
        it "is created with a head" $ do
            show (circle 0) `shouldBe` "(0)"
