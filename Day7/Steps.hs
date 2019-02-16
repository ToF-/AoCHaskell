module Steps 
where
import Data.Map as M
import Data.List as L
import Data.Maybe

data Step = A | B | C | D | E | F | G | H | I | J | K | L | M 
          | N | O | P | Q | R | S | T | U | V | W | X | Y | Z 
    deriving (Eq, Ord, Show, Enum)

type Edge = (Step, Step)
type PredList = Map Step [Step]
type SuccList = Map Step [Step]
type State = ([Step],[Step])

succList :: [Edge] -> SuccList
succList = L.foldl addEdge M.empty
    where
    addEdge :: SuccList -> Edge -> SuccList
    addEdge pl (pred,succ) = M.insertWith (\l' l -> (L.nub . L.sort) (l ++ l')) pred [succ] pl

predList :: SuccList -> PredList
--foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
predList = succList . M.foldlWithKey reverseEdge []
    where
    reverseEdge :: [Edge] -> Step -> [Step] -> [Edge]
    reverseEdge acc step steps = acc ++ L.map (\succ -> (succ,step)) steps

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

execute :: SuccList -> [Step]
execute sl = snd $ fmap reverse $ execute' (startSteps sl,[])
    where
    pl :: PredList
    pl = predList sl

    execute' :: State -> State
    execute' st@([],_) = st
    execute' ((step:steps),executed) = case step `M.lookup` sl of
        Nothing -> (steps, done)
        Just succs -> execute' (sort ((L.filter allPredExecuted succs) ++ steps), done)
        where
        done :: [Step]
        done = step : executed

        allPredExecuted :: Step -> Bool
        allPredExecuted s = case s `M.lookup` pl of
            Nothing -> True
            Just ss -> all (`elem` done) ss

type Time = Int
data Job = Job Step Time | Idle Time
    deriving (Eq, Show)
type Worker = [Job]

step :: Job -> Maybe Step
step (Job s _) = Just s
step (Idle _) = Nothing

duration :: Job -> Time
duration (Job _ t) = t
duration (Idle t)  = t

time :: Worker -> Time
time = sum . L.map duration

stepsDone :: Worker -> [Step]
stepsDone = catMaybes . L.map step

stepsDoneAt :: Time -> Worker -> [Step]
stepsDoneAt t [] = []
stepsDoneAt t w | t >= time w = stepsDone w
stepsDoneAt t ((Job s d):js) = stepsDoneAt t js

wait :: Time -> Worker -> Worker
wait t w | t > time w = Idle (t - time w) : w
wait t w = w
