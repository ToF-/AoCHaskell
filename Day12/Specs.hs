import Test.Hspec
import Test.QuickCheck
import Sustain

notes = ["...##"
        ,"..#.."
        ,".#..."
        ,".#.#."
        ,".#.##"
        ,".##.."
        ,".####"
        ,"#.#.#"
        ,"#.###"
        ,"##.#."
        ,"##.##"
        ,"###.."
        ,"###.#"
        ,"####."]

initial = "#..#.#..##......###...###"

listOfPlants :: Gen String
listOfPlants = fmap (trim . ('#':)) (listOf1 (elements ['#','.']))
    where
    trim = reverse . dropWhile (=='.') . reverse

main = hspec $ do
    describe "pattern" $ do
        it "tells the pattern of the pots containing a plant" $ do
            pattern 0    initial `shouldBe` [0,0,3,5,8,9,16,17,18,22,23,24]
            pattern (-5) initial `shouldBe` [-5,-5,-2,0,3,4,11,12,13,17,18,19]
    describe "plants" $ do
        it "tells the plants from a pattern" $ do
            plants [0,0,3,5,8,9,16,17,18,22,23,24] `shouldBe` initial

    describe "offset" $ do
        it "describe a list of plants with an number of empty pots ahead" $ do
            offset (-3) [0,0] `shouldBe` "...#"

    describe "plants and pattern" $ do
        it "are symmetrical" $ forAll listOfPlants $ \s -> forAll (choose (-5,5)) $ 
            \n -> plants (pattern n s) == s
            
