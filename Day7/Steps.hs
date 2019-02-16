module Steps 
where
import Data.Map as M
import Data.List as L

data Step = A | B | C | D | E | F | G | H | I | J | K | L | M 
          | N | O | P | Q | R | S | T | U | V | W | X | Y | Z 
    deriving (Eq, Ord, Show, Enum)

type Edge = (Step, Step)
type PredList = Map Step [Step]

predList :: [Edge] -> PredList
predList = L.foldl addEdge M.empty
    where
    addEdge :: PredList -> Edge -> PredList
    addEdge pl (pred,succ) = M.insertWith (\l' l -> (L.nub . L.sort) (l ++ l')) succ [pred] pl
