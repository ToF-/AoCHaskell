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

initial = (0,"#..#.#..##......###...###") :: Pattern
initialPattern :: Gen (Int,String)
initialPattern = fmap ((\s -> (0,s)).(++"#").('#':)) $ listOf1 (elements "#.")

main = do 
    quickCheck $ forAll initialPattern $ \(n,s) ->  head s == '#' && last s == '#'
    quickCheck $ forAll initialPattern $ \p -> 
        forAll (choose (1,4)) $ \l ->
            forAll (choose (1,4)) $ \r ->
                 let
                    p' = extendRight r (extendLeft l p)
                 in normalize p' == p
    hspec $ do
        describe "sustain" $ do
            it "tells which pots will contain a plant according to notes" $ do
                let p = (0,"#..#.#..##......###...###")
                sustain p notes `shouldBe` (0,"#...#....#.....#..#..#..#")

            it "can be repeated" $ do
                times 20 (flip sustain notes) initial `shouldBe`  
                 (-2,"#....##....#####...#######....#.#..##")
        
