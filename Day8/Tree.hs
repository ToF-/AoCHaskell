module Tree
where

type Entry = Int
type Tree = Node
data Node = Node [Node] [Entry]
    deriving (Eq,Show)

sumEntries :: Tree -> Entry
sumEntries (Node subTrees entries) =
    sum (map sumEntries subTrees) + sum entries


tree :: [Int] -> (Tree,[Int])
tree (n:ne:es) = (Node sts' (take ne rs') ,drop ne rs')
    where
    (sts',rs') = foldl collectTrees ([],es) [1..n]
    collectTrees :: ([Tree],[Entry]) -> Int -> ([Tree],[Entry])
    collectTrees (ts,es) _ = (ts ++ [t],rs)
        where
        (t,rs) = tree es
