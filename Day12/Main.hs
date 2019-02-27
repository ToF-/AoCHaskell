import Sustain
import System.Environment

ns = map pattern  ["#####"
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

i = pattern "##.......#.######.##..#...#.#.#..#...#..####..#.##...#....#...##..#..#.##.##.###.##.#.......###....#"
m = 50000000000
n = 1000000
main = do
    n <- fmap (read.head) getArgs
    print $  findLoop i n ns 
-- print (times n (flip sustain ns) i)

