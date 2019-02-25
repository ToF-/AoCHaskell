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
        
