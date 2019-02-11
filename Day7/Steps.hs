module Steps 
where
import Data.Map as M
import Data.List as L
import Data.Ord

type Priority = Int
type Required = [Node]
type Task = (Priority,Required)
type Edge = (Node,Node)
type Graph = Map Node Required
type PriorityList = [(Node,Priority)]

data Node = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    deriving (Eq,Ord,Show,Enum)

graph :: [Edge] -> Graph
graph = L.foldl addEdge M.empty
    where
    addEdge :: Graph -> Edge -> Graph
    addEdge map (from,to) = insertWith add to [from] map
        where
        add :: Required -> Required -> Required
        add old new = L.nub (L.sort (old ++ new))

priority :: Graph -> PriorityList
priority g = uniq (sortBy prior (adjustPriority 0 [] t))
    where
    t = head (L.filter (\n -> not (n `L.elem` rs)) ks)
        where
        rs = concat (M.elems g)
        ks = M.keys g

    adjustPriority :: Priority -> PriorityList -> Node -> PriorityList
    adjustPriority p pl n = case M.lookup n g of
        Just rs -> L.foldl (adjustPriority (succ p)) ((n,p):pl) rs
        Nothing -> (n,p) : pl

    prior :: (Node,Priority) -> (Node,Priority) -> Ordering
    prior (n,p) (m,q) | p == q = compare n m
    prior x y                 = flip (comparing snd) x y

    uniq :: PriorityList -> PriorityList
    uniq [] = []
    uniq ((n,p):ps) = (n,p) : uniq (remove n ps)
            where
            remove :: Node -> PriorityList -> PriorityList 
            remove _ [] = []
            remove n ((m,p):ps) | n == m  = remove n ps
                                | otherwise = (m,p) : remove n ps


steps :: Graph -> String
steps g = L.concatMap (show.fst) (priority g)
