
import Tree

main = do
    input <- readFile "input.txt"
    let large = map read (words input)
    print (sumEntries (fst (tree large)))
