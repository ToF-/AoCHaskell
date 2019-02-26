import Test.Hspec
import Test.QuickCheck
import Sustain
import Data.Array

ns = map pattern ["...##"
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

i = pattern "#..#.#..##......###...###" 
initialPattern :: Gen Pattern
initialPattern = fmap (pattern . ('#':) . (++"#")) $ listOf1 (elements "#.")

main = do 
    quickCheck $ forAll initialPattern $ \p ->
        let (start,end) = bounds p in p!start == '#' && p!end == '#'
    quickCheck $ forAll initialPattern $ \p -> 
        forAll (choose (1,4)) $ \l ->
            forAll (choose (1,4)) $ \r ->
                 let
                    p' = extendRight r (extendLeft l p)
                 in normalize p' == p
    hspec $ do
        describe "sustain" $ do
            let pat a = (fst (bounds a),elems a)
            it "tells which pots will contain a plant according to notes" $ do
                let p = pattern "#..#.#..##......###...###"
                pat (sustain p ns) `shouldBe` (0,"#...#....#.....#..#..#..#")

            it "can be repeated" $ do
                let r = times 20 (flip sustain ns) i 
                pat r `shouldBe` (-2,"#....##....#####...#######....#.#..##")
        
