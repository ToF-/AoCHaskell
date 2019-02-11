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
type Priorities = Map Node Priority

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

priority :: Graph -> Priorities
priority g = addPriority g t 0 M.empty
    where
    addPriority :: Graph -> Node -> Priority -> Priorities -> Priorities
    addPriority g n p pl = case M.lookup n g of
        Nothing -> M.insertWith max n p pl
        Just rs -> L.foldl (adjustPriority (succ p)) (M.insertWith max n p pl) rs

    adjustPriority :: Priority -> Priorities -> Node -> Priorities
    adjustPriority p pl n = addPriority g n p pl 

    t = head (L.filter (\n -> not (n `L.elem` rs)) ks)
        where
        rs = concat (M.elems g)
        ks = M.keys g

steps :: Graph -> [Node]
steps g = L.map fst (L.sortBy prior (M.toList (priority g)))
    where
    prior :: (Node,Priority) -> (Node,Priority) -> Ordering
    prior (n,p) (m,q) | p == q = compare n m
    prior x y                 = flip (comparing snd) x y
