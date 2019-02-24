import Test.Hspec
import Test.QuickCheck
import Sustain
import Data.Bits

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
--     .                            .
--     .                            .
--20: .#....##....#####...#######....#.#..##.
--

initialPattern :: Gen String
initialPattern = fmap ('#':) $ listOf1 (elements "#.")

main = hspec $ do
    describe "initial" $ do
        describe "yields a pattern" $ do
            it "when given #" $ initial "#"  `shouldBe` Pattern 0 1 1
            it "when given #.#" $ initial "#.#"  `shouldBe` Pattern 0 3 5
            it "ignoring . on left" $ initial ".###" `shouldBe` Pattern 0 3 7
            it "ignoring . on right" $ initial "#..#." `shouldBe` Pattern 0 4 9
    describe "a pattern" $ do
        describe "can be extended on the right" $ do
            it "by a certain number" $ do
                extendRight (initial "#..#") 2 `shouldBe` Pattern 0 6 36
        describe "can be extended on the left" $ do
            it "by a certain number" $ do
                extendLeft (initial "#..#") 2 `shouldBe` Pattern (-2) 6 9
        describe "can be represented" $ do
            it "in normal size with smallest value" $ represent (initial "#") `shouldBe` "#"
            it "in normal size with large value" $ represent (initial "#.##") `shouldBe` "#.##"
            it "in extended right" $ do represent (Pattern 0 6 36) `shouldBe` "#..#.."
            it "in extended left" $ do represent (Pattern (-2) 8 36) `shouldBe` "..#..#.."

        describe "can be normalized" $ do
            it "to remove extensions on the right" $ normalize (Pattern 0 6 36) `shouldBe` Pattern 0 4 9
            it "to remove extensions on the left" $ normalize (Pattern (-2) 8 36) `shouldBe` Pattern 0 4 9

            it "always" $ forAll initialPattern $ \s ->
                forAll (choose (1,10)) $ \r -> 
                    forAll (choose (1,10)) $ \l ->
                        let i = initial (take 30 s)
                            p = extendLeft (extendRight i r) l
                        in normalize p == i
    describe "note" $ do
        it "is a pattern of 5 plants" $ note "#..#." `shouldBe` Pattern 0 5 18
        it "can be extended many times to attain a given size" $ do
            map represent (extensions (note "#..#.") 7) `shouldBe`
                ["#..#.","#..#..","#..#..."]
    describe "match" $ do
        it "tells if a note matches a pattern at the last position" $ do
            ((initial "#..#.#..#..#") `match` (note ".#..#")) `shouldBe` True
            ((initial "#..#.#..#..#") `match` (extendRight (note ".#..#") 1)) `shouldBe` False
    describe "matches" $ do
         it "tells if a note matches a pattern in what positions" $ do
            ((initial "#..#.#..#..#") `matches` (note ".#..#")) `shouldBe` [1,6,9]
            ((initial "#..#.#..#..#") `matches` (note "....#")) `shouldBe` [1,6,9]

