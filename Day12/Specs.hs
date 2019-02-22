import Test.Hspec
import Sustain

notes = [("...##",'#')
        ,("..#..",'#')
        ,(".#...",'#')
        ,(".#.#.",'#')
        ,(".#.##",'#')
        ,(".##..",'#')
        ,(".####",'#')
        ,("#.#.#",'#')
        ,("#.###",'#')
        ,("##.#.",'#')
        ,("##.##",'#')
        ,("###..",'#')
        ,("###.#",'#')
        ,("####.",'#')
        ,(".....",'.')]

main = hspec $ do
    describe "number" $ do
        it "tells the numbers of the pots containing a plant" $ do
            number 0 "#..#.#..##......###...###" `shouldBe` [0,3,5,8,9,16,17,18,22,23,24]

    describe "patterns" $ do
        it "denote sequence of plants that sustain in position 0" $ do
            let ps = patterns notes
            ps!!0 `shouldBe` [1,2]
            ps!!1 `shouldBe` [0]
            ps!!2  `shouldBe` [-1]
            ps!!3  `shouldBe` [-1,1]
            (last ps) `shouldBe` [-2,-1,0,1]

