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

type Time = Int
data Job = Job Step Time | Idle Time
    deriving (Eq, Show)
type Worker = [Job]
type StepTime = (Step,Time)

step :: Job -> Maybe Step
step (Job s _) = Just s
step (Idle _) = Nothing

duration :: Job -> Time
duration (Job _ t) = t
duration (Idle t)  = t

timeWorked :: Worker -> Time
timeWorked = sum . L.map duration

stepsDone :: Worker -> [StepTime]
stepsDone js = L.map (\step -> (step,timeWorked js)) (steps js)
    where
    steps [] = []
    steps ((Job s _):js) = s:steps js
    steps ((Idle _) :js) = steps js

wait :: Time -> Worker -> Worker
wait t w | t > timeWorked w = Idle (t - timeWorked w) : w
wait t w = w

type TimeList = Map Step Time

criticalPathTimeList :: Time -> SuccList -> TimeList
criticalPathTimeList base sl = cptl 0 M.empty (endStep sl) 
    where
    preds = predList sl
    cptl :: Time -> TimeList -> Step -> TimeList
    cptl t tl step = case step `M.lookup` preds of
        Nothing -> tl'
        Just preds -> L.foldl (cptl t') tl' preds
        where
        t' = t + base + (fromEnum step) 
        tl' = M.insertWith max step t' tl
 
data Schedule = Schedule {
        workers :: [Worker],
        baseDuration :: Time,
        successors :: SuccList,
        predecessors :: PredList,
        criticalPath :: TimeList,
        remaining :: [StepTime] }
    deriving (Eq, Show)

schedule :: Int -> Time -> [Edge] -> Schedule
schedule n base edges = Schedule (replicate n []) base succs preds cptl []
    where
    succs = succList edges
    preds = predList (succList edges)
    cptl  = criticalPathTimeList base succs

stepsInProgress :: Schedule -> [Step]
stepsInProgress sch = []

assignStep :: StepTime -> Schedule -> Schedule
assignStep (step,time) sch = sch { workers = replace ixMin (addJob (step,time)) (workers sch) }
    where
    addJob (step,time) jobs = job step : wait time jobs
    job step = Job step ((baseDuration sch) + fromEnum step)
    ixMin = snd $ minimum $ zip (L.map timeWorked (workers sch)) [0..]

replace :: Int -> ([a] -> [a]) -> [[a]] -> [[a]]
replace _ f [] = []
replace 0 f (a:as) = f a : as 
replace n f (a:as) = a : replace (pred n) f as

allStepsDone :: Schedule -> [StepTime]
allStepsDone = concatMap stepsDone . workers

nextSteps :: Schedule -> [StepTime]
nextSteps sch = atMaxTime $ sortBy descCriticalTime steps
    where
    atMaxTime [] = []
    atMaxTime ((step,time):steps) = case step `L.lookup` steps of
         Nothing -> (step,time):atMaxTime steps
         Just otherTime -> case otherTime > time of
            True -> atMaxTime steps
            False -> (step,time) : atMaxTime (remove (step,otherTime) steps)
                where
                remove a [] = []
                remove a (b:as) | a == b = as
                                | otherwise = b : remove a as

    steps = if initial then starts else (remaining sch ) ++ L.concatMap succsTo (allStepsDone sch)

    starts = L.map (\step -> (step,0)) $ startSteps succs
    succs = successors sch
    preds = predecessors sch
    done :: [Step]
    done = L.map fst (allStepsDone sch)
    initial = L.null (allStepsDone sch)
    
    succsTo :: StepTime -> [StepTime]
    succsTo (step,time) = case step `M.lookup` succs of
        Nothing -> []
        Just steps -> L.map (\step -> (step,time)) $ L.filter allPredsDone $ L.filter (not.(`elem` done)) steps

    allPredsDone step = case step `M.lookup` preds of
        Nothing -> True
        Just steps -> all (`elem` done) steps

    descCriticalTime s t = case flip compare ((fst s) `M.lookup` cl) ((fst t) `M.lookup` cl) of
        EQ -> compare s t
        ord -> ord
        where
        cl = criticalPath sch

assignNext :: Schedule -> Schedule 
assignNext sch = case nextSteps sch of
    [] -> sch
    (step:steps) -> assignStep step $ sch { remaining = steps }

run :: Schedule -> Schedule 
run sch | L.null (nextSteps sch) = sch
        | otherwise              = run $ assignNext sch

maxTime :: [Edge] -> Int -> Time -> Time
maxTime edges n base = maximum $ L.map timeWorked $ workers $ run $ schedule n base edges
