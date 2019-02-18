
import Tree

main = do
    input <- readFile "input.txt"
    let large = map read (words input)
    let t = fst (tree large)
    print (sumEntries t)
    print (value t)
