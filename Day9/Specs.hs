import Test.Hspec
import Marbles

main = hspec $ do
    describe "marble circle" $ do
        it "can be show with current between ( )" $ do
            let play n = showCircle (last (take (succ n) (iterate add circle)))
            play 0 `shouldBe` "(0)"
            play 1 `shouldBe` "0 (1)"
            play 2 `shouldBe` "0 (2) 1"
            play 3 `shouldBe` "0 2 1 (3)"
            play 4 `shouldBe` "0 (4) 2 1 3"
            play 5 `shouldBe` "0 4 2 (5) 1 3"
            play 6 `shouldBe` "0 4 2 5 1 (6) 3"
            play 7 `shouldBe` "0 4 2 5 1 6 3 (7)"
            play 22 `shouldBe` "0 16 8 17 4 18 9 19 2 20 10 21 5 (22) 11 1 12 6 13 3 14 7 15"
                
            
