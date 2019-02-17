module Steps 
where
import Data.Map as M
import Data.List as L
import Data.Maybe

data Step = Start | A | B | C | D | E | F | G | H | I | J | K | L | M 
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

type Count = Int
type CountList = Map Step Count

predCount :: SuccList -> CountList
predCount sl = (fromSucc sl) `M.union` (fromStartSteps sl)
    where 
    fromSucc =  M.fromList . L.map (\g -> (head g, length g)) . L.group . L.sort . L.concat . M.elems 
    fromStartSteps = M.fromList . L.map (\step -> (step,0)) . startSteps

availableSteps :: CountList -> [Step]
availableSteps = M.keys . M.filter (==0)

decreasePredCount :: CountList -> Step -> CountList
decreasePredCount cl step = M.adjust pred step cl

type Time = Int
data Worker = Job Time Step | Free
    deriving (Eq,Ord,Show)

finishNextJob :: [Worker] -> (Worker,[Worker])
finishNextJob ws = (head ws', L.sort (Free : tail ws')) 
    where 
    ws' = L.sort ws

data Schedule = Schedule { 
    workers :: [Worker],
    successors :: SuccList,
    end        :: Step,
    predecessors  :: CountList,
    nextSteps  :: [Step],
    doneSteps  :: [Step],
    base       :: Time,
    time       :: Time }
    deriving (Eq,Show)

schedule :: Int -> Time -> [Edge] -> Schedule
schedule n base edges = Schedule ws succs ends preds next  [] base 0 
    where
    ws = replicate n Free
    succs = succList edges
    ends = endStep succs
    preds = predCount succs
    next = []
    
done :: Schedule -> Bool
done sch = end sch `elem` doneSteps sch

next :: Schedule -> Schedule 
next sch = sch { nextSteps = nextSteps', predecessors = preds }
    where
    available = availableSteps (predecessors sch)
    preds = L.foldl (flip M.delete) (predecessors sch) available 
    nextSteps' = L.sort (nextSteps sch ++ available)

assign :: Schedule -> Schedule
assign sch | L.null (nextSteps sch) = sch
assign sch | all (/=Free) (workers sch) = sch
assign sch = assign (sch { workers = workers', nextSteps = nextSteps' })
    where
    (step:nextSteps') = L.sort (nextSteps sch)
    (_:ws) = reverse (L.sort (workers sch))
    workers' = L.sort ((Job duration step ) : ws)
    duration = time sch + base sch + fromEnum step

work :: Schedule -> Schedule
work sch | all (==Free) (workers sch) = sch
work sch = sch { workers = workers', predecessors = predecessors', doneSteps = doneSteps', time = time' }
    where
    (Job t step, workers') = finishNextJob (workers sch)
    predecessors' = M.delete step 
        (case step `M.lookup` (successors sch) of
            Nothing -> predecessors sch
            Just succs -> L.foldl decreasePredCount (predecessors sch) succs )
    doneSteps' = (doneSteps sch) ++ [step]
    time' = t

run :: Schedule -> Schedule
run sch | done sch = sch
run sch = run (work (assign (next sch)))
