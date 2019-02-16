module Steps 
where
import Data.Map as M
import Data.List as L

data Step = A | B | C | D | E | F | G | H | I | J | K | L | M 
          | N | O | P | Q | R | S | T | U | V | W | X | Y | Z 
    deriving (Eq, Ord, Show, Enum)

type Edge = (Step, Step)
type PredList = Map Step [Step]
type SuccList = Map Step [Step]

predList :: [Edge] -> PredList
predList = L.foldl addEdge M.empty
    where
    addEdge :: PredList -> Edge -> PredList
    addEdge pl (pred,succ) = M.insertWith (\l' l -> (L.nub . L.sort) (l ++ l')) succ [pred] pl

succList :: [Edge] -> SuccList
succList = L.foldl addEdge M.empty
    where
    addEdge :: SuccList -> Edge -> SuccList
    addEdge pl (pred,succ) = M.insertWith (\l' l -> (L.nub . L.sort) (l ++ l')) pred [succ] pl

startSteps :: SuccList -> [Step]
startSteps sl = L.filter (not.(`elem` successors)) steps
    where
    steps = M.keys sl
    successors = concat (M.elems sl)

endStep :: SuccList -> Step
endStep sl = case L.filter (not.(`elem` steps)) successors of
    [] -> error "this SuccList has no ending step"
    l -> head l
    where
    steps = M.keys sl
    successors = concat (M.elems sl)
