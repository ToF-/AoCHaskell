module Steps 
where
import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Ord

type Edge = (Node,Node)
type NodeList = Map Node [Node]

data Node = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    deriving (Eq,Ord,Show,Enum)

predList :: [Edge] -> NodeList 
predList = L.foldl addPreds M.empty 

succList :: [Edge] -> NodeList 
succList = L.foldl addSuccs M.empty

addPreds :: NodeList -> Edge -> NodeList
addPreds l (a,b) = insertWith add b [a] l

addSuccs :: NodeList -> Edge -> NodeList
addSuccs l (a,b) = insertWith add a [b] l

add :: [Node] -> [Node] -> [Node]
add ns n = nub (sort (n++ns))

startSteps :: NodeList -> [Node]
startSteps m = sort (L.filter (\n -> not (n `elem` succs)) nodes)
    where
    succs = L.concat (M.elems m)
    nodes = M.keys m

steps :: [Edge] -> [Node]
steps es = fst (visit ([],startSteps succs))
    where
    succs = succList es
    preds = predList es
    visit (vs,[]) = (vs,[])
    visit (vs,(n:ns)) = case fmap (L.filter allPredVisited) (M.lookup n succs) of
        Just [] -> visit (vs',ns) 
        Just ss -> visit (vs',sort (ss ++ ns)) 
        Nothing -> visit (vs',ns)
        where 
        vs' = vs ++ [n]
        allPredVisited n = case fmap (all (`elem` vs')) (M.lookup n preds) of
            Nothing -> True
            Just b -> b
