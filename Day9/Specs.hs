import Test.Hspec
import Marbles

main = hspec $ do
    describe "marble circle" $ do
        let splay n = showCircle $ play n 9 
        it "can be show with current between ( )" $ do
            splay 0 `shouldBe` "(0)"
            splay 1 `shouldBe` "0 (1)"
            splay 2 `shouldBe` "0 (2) 1"
            splay 3 `shouldBe` "0 2 1 (3)"
            splay 4 `shouldBe` "0 (4) 2 1 3"
            splay 5 `shouldBe` "0 4 2 (5) 1 3"
            splay 6 `shouldBe` "0 4 2 5 1 (6) 3"
            splay 7 `shouldBe` "0 4 2 5 1 6 3 (7)"
            splay 22 `shouldBe` "0 16 8 17 4 18 9 19 2 20 10 21 5 (22) 11 1 12 6 13 3 14 7 15"

        it "has special rules for marble multiple of 23" $ do
            splay 23 `shouldBe` "0 16 8 17 4 18 (19) 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15"    

        it "has n players with each a score" $ do
            scores (circle 4) `shouldBe` [0,0,0,0]
            scores (play 25 9) `shouldBe` []
                
            
