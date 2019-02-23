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

--                 1         2         3     
--       0         0         0         0     
-- 0: ...#..#.#..##......###...###...........
-- 1: ...#...#....#.....#..#..#..#...........
-- 2: ...##..##...##....#..#..#..##..........
-- 3: ..#.#...#..#.#....#..#..#...#..........
-- 4: ...#.#..#...#.#...#..#..##..##.........
-- 5: ....#...##...#.#..#..#...#...#.........
-- 6: ....##.#.#....#...#..##..##..##........
-- 7: ...#..###.#...##..#...#...#...#........
-- 8: ...#....##.#.#.#..##..##..##..##.......
-- : ...##..#..#####....#...#...#...#.......
--10: ..#.#..#...#.##....##..##..##..##......
--11: ...#...##...#.#...#.#...#...#...#......
--12: ...##.#.#....#.#...#.#..##..##..##.....
--13: ..#..###.#....#.#...#....#...#...#.....
--14: ..#....##.#....#.#..##...##..##..##....
--15: ..##..#..#.#....#....#..#.#...#...#....
--16: .#.#..#...#.#...##...#...#.#..##..##...
--17: ..#...##...#.#.#.#...##...#....#...#...
--18: ..##.#.#....#####.#.#.#...##...##..##..
--19: .#..###.#..#.#.#######.#.#.#..#.#...#..
--20: .#....##....#####...#######....#.#..##.
--
--         0    0    1    0    2   
--         0    5    0    5    0     
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
        it "describe a list of 5 plants at the given position" $ do
            offset (-3) [0,0]     `shouldBe` "...#."
            offset (-3) [-2,-2,0] `shouldBe` ".#.#."
            offset 0 [3,3]        `shouldBe` "...#."
            offset 3 [3,3]        `shouldBe` "#...."
            let p = pattern 0 initial
            offset 0 p `shouldBe` "#..#."
            offset 1 p `shouldBe` "..#.#"
            offset 2 p `shouldBe` ".#.#."
            offset 5 p `shouldBe` "#..##"

    describe "sustain" $ do
        let p = pattern 0 initial
        it "sustainAt if a list of plant can sustain in a given pot" $ do
            sustainAt [0,0] notes (-5) `shouldBe` Nothing
            sustainAt [0,0] notes (-2) `shouldBe` Just 0
            sustainAt [0,1,3,4] notes 0 `shouldBe` Just 2
            sustainAt p notes 22 `shouldBe` Just 24
        it "sustain tells all the plants that grew from a pattern" $ do
            sustain [0,0] notes  `shouldBe` [0,0,1]
            sustain [0,0,1] notes  `shouldBe` [-1,-1,1]
            sustain p notes `shouldBe` [0,0,4,9,15,18,21,24]
            plants (sustain p notes) `shouldBe` "#...#....#.....#..#..#..#"
        it "sustainN tells all the plants that grew after N generations" $ do
            sustainN 1 p notes `shouldBe` sustain p notes
            sustainN 2 p notes `shouldBe` sustain (sustain p notes) notes
            plants (sustainN 20 p notes) `shouldBe` "#....##....#####...#######....#.#..##"

    describe "sum pots" $ do
        let p = pattern 0 initial
        it "sum the pot numbers of a given pattern" $ do
            sumPots (sustainN 20 p notes) `shouldBe` 325
        let notes = ["#####" 
                    ,"#.##."
                    ,"##.#." 
                    ,"#..#." 
                    ,"##..." 
                    ,"#...#" 
                    ,".#..#" 
                    ,".#..." 
                    ,"##.##" 
                    ,"...##" 
                    ,"..###" 
                    ,".##.#" 
                    ,"##..#"]
        let q = pattern 0 "##.......#.######.##..#...#.#.#..#...#..####..#.##...#....#...##..#..#.##.##.###.##.#.......###....#"
        it "finds the pot numbers for the puzzle" $ do
            sumPots (sustainN 20 q notes) `shouldBe` 2840

    describe "plants and pattern" $ do
        it "are symmetrical" $ forAll listOfPlants $ \s -> forAll (choose (-5,5)) $ 
            \n -> plants (pattern n s) == s
            
