```haskell
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

type Pots = (Int,String)

patterns s = transpose ["...."++s,"..."++s++".",".."++s++"..","."++s++"...",s++"...."]

match t notes | `elem` notes = '#'
              | otherwise    = '.'

trimRight = reverse . dropWhile (=='.') . reverse
normalize (n,[]) = (n,[])
normalize (n,'#':s) = (n,'#': trimRight s)
normalize (n,'.':s) = (pred n,s)
sustain (n,s) = normalize (n-2,map (\p -> match p notes) patterns s)
```


