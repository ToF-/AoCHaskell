module Steps 
where
import Data.Map as M
import Data.List as L

type Edge = (Node,Node)
type Graph = Map Node [Node]

data Node = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    deriving (Eq,Ord,Show,Enum)

graph :: [Edge] -> Graph
graph = L.foldl addEdge M.empty
    where
    addEdge :: Graph -> Edge -> Graph
    addEdge map (from,to) = insertWith add to [from] map
        where
        add :: [Node] -> [Node] -> [Node]
        add old new = L.nub (L.sort (old ++ new))

target :: Graph -> Node
target g = head (L.filter (\n -> not (n `elem` elems)) keys)
    where
    elems = L.nub (L.sort (L.concat (M.elems g)))
    keys  = M.keys g

previous :: Graph -> [Node] -> Node -> [Node]
previous g al n = case n `M.lookup` g of
    Just ns -> L.filter (\n -> not (n `elem` al)) ns
    Nothing -> []

backward :: Graph -> Node -> [[Node]]
backward g n = backward' g [n] n
    where
    backward' :: Graph -> [Node] -> Node -> [[Node]]
    backward' g al e = [n] : case previous g al n of
        [] -> []
        ns -> ns : case previous g (al ++ ns) (head ns) of
            [] -> case previous g (al ++ ns) (head (tail ns)) of
                [] -> []
                ns -> [head ns] :  []
            ns -> [head ns] :  []
    
