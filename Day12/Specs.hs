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
    describe "a pattern " $ do
        it "tells if a plant will grow or not" $ do
                     "...##" `find` notes `shouldBe` True
                     "#####" `find` notes `shouldBe` False
                     "....." `find` notes `shouldBe` False
    describe "numberPots" $ do
        it "numbers the pots and add 5 empty pots before and after a line" $ do
            numberPots "##.#.##.#" `shouldBe`
                [('.',-5),('.',-4),('.',-3),('.',-2),('.',-1),('#',0),('#',1),('.',2),('#',3),('.',4),('#',5),('#',6),('.',7),('#',8),('.',9),('.',10),('.',11),('.',12),('.',13)]
                
            
            



--     it "tells if several plants will grow or not" $ do
--         let initial = "...#..#.#..##......###...###..........."
--         let next    = "...#...#....#.....#..#..#..#..........."
--         sustain notes initial  `shouldBe` next 
