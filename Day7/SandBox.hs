import Data.Map as M
import Data.List as L
import Data.Set as S

type Edge = (Node,Node)
type Graph = Map Node [Node]

data Node = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    deriving (Eq,Ord,Show,Enum)

small = [(C,A)
        ,(C,F)
        ,(A,B)
        ,(A,D)
        ,(B,E)
        ,(D,E)
        ,(F,E)]

collect :: [(Edge)] -> Graph
collect = L.foldl insertNode M.empty 
    where
    insertNode :: Graph -> (Edge) -> Graph
    insertNode map (from,to) = M.insertWith addNode to [from] map 
    addNode :: [Node] -> [Node] -> [Node]
    addNode new old = L.nub (L.sort (new ++ old)) 

final :: Graph -> [Node]
final m = L.filter (\n -> not (n `elem` (concat $ M.elems m))) (keys m)

traverse :: Graph -> [[Node]]
traverse g = let
    f = head (final g)
    addNodes s ns = s `S.union` (S.fromList ns)
    next :: Graph -> Set Node -> Node -> [[Node]]
    next g s n | n `S.member` s = []
               | otherwise     = [n] : case M.lookup n g of 
        Just ns -> [n] : concatMap (next g (addNodes s ns)) ns
        Nothing -> []
    in next g S.empty f
