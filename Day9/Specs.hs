import Test.Hspec
import Test.Hspec.Core.QuickCheck
import Test.QuickCheck
import CircularList as C

e = C.clist 0
i = C.insert
list = foldl (flip i) e 
s = C.shift

main = hspec $ do
    describe "a circular list" $ do
        it "is created with a head" $ do
            show e `shouldBe` "(0)"
        it "can be inserted an item" $ do
            show (i 42 e) `shouldBe` "0 (42)"
        it "can be inserted several items" $ do
            show (list [1]) `shouldBe` "0 (1)"
            show (list [1,2]) `shouldBe` "0 1 (2)"

        it "can be shifted to the right" $ do
            show (s (list [])) `shouldBe` "(0)"
            show (s (list [1])) `shouldBe` "(0) 1"
            show ((s.s) (list [1])) `shouldBe` "0 (1)"
            show (s (list [1,2])) `shouldBe` "1 2 (0)"
