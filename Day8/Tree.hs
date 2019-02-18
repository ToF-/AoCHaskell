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

value :: Tree -> Int
value (Node [] es) = sum es
value (Node st es) = sum (map (\cn -> valueChildNode st cn)  es)
    where
    valueChildNode st n | (n > 0 && n <= length st) = value (st!!(n-1))
                        | otherwise = 0
