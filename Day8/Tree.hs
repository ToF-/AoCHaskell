module Tree
where

type Entry = Int
type Tree = Node
data Node = Node [Node] [Entry]
    deriving (Eq,Show)

sumEntries :: Tree -> Entry
sumEntries (Node subTrees entries) = sum (map sumEntries subTrees) + sum entries


tree :: [Int] -> (Tree,[Int])
tree (nbSubTrees : nbEntries : rest) = (Node subTrees (take nbEntries newRest) , drop nbEntries newRest)
    where
    (subTrees,newRest) = foldl collectTrees ([],rest) [1..nbSubTrees]

    collectTrees :: ([Tree],[Entry]) -> Int -> ([Tree],[Entry])
    collectTrees (subTrees,entries) _ = (subTrees ++ [newTree],rest)
        where
        (newTree,rest) = tree entries

value :: Tree -> Int
value (Node [] es) = sum es
value (Node st es) = sum (map (valueChildNode st)  es)
    where
    valueChildNode st n | inRange = value (st!!(n-1))
        where
        inRange = (n > 0 && n <= length st)

    valueChildNode _ _ = 0
