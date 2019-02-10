module Steps 
where
import Data.Map as M
import Data.List as L

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

