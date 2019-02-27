import Test.Hspec
import Test.QuickCheck
import Sustain
import Data.Array as A
import Data.Set as S
import Data.List as L
import Data.Finite

ns = notes ["...##"
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

p = pattern "#..#.#..##......###...###" 

main = do 

    hspec $ do
        describe "pattern" $ do
            it "collects the numbers of the pots containing a plant" $ do
                0 `S.member` p `shouldBe` True
                1 `S.member` p `shouldBe` False
                findMin p `shouldBe` 0
                findMax p `shouldBe` 24

        describe "note" $ do
            it "collects the numbers of the pots forming a sustainable context" $ do
                let n = note ".#.##"
                S.elems n `shouldBe` [finite 1,finite 3,finite 4]

        describe "matches" $ do
            it "tells if one of the notes match a set of pots at a given position" $ do
                matches ns p 0 `shouldBe` True
                matches ns p 1 `shouldBe` False
                matches ns p 4 `shouldBe` True

        describe "generation" $ do
            it "contains all the positions at which pots match a note" $ do
                let g = generation ns p
                    rep s = L.map (rep' s) [(findMin s)..(findMax s)]
                    rep' s i | i `member` s = '#'
                             | otherwise      = '.'
                S.elems g `shouldBe` [0,4,9,15,18,21,24]
                rep g  `shouldBe` "#...#....#.....#..#..#..#"

        describe "normalize" $ do
            it "changes the numbers of a set so that they begin with 0" $ do
                let s = fromDistinctAscList [-3,6,17]
                normalize s `shouldBe` fromDistinctAscList [0,9,20]
                let s = fromDistinctAscList [3,6,17]
                normalize s `shouldBe` fromDistinctAscList [0,3,14]
        
        describe "findRepetition" $ do
            it "finds after how many generations and with what stating position a pattern is repeated" $ do
                findRepetition p ns `shouldBe` Nothing  

