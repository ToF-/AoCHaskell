module Steps 
where
import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Ord
import Data.Maybe

type Edge = (Step,Step)
type StepList = Map Step [Step]
type Time = Int
type Index = Int

data Step = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    deriving (Eq,Ord,Show,Enum)

data Job = Job Step Time | Idle Time
    deriving (Eq,Show)
           
type CriticalTime = Map Step Time
type Todo = [Job]
data Schedule = Schedule { 
    base :: Time,
    todos :: [Todo],
    successors :: StepList,
    predecessors :: StepList,
    criticalPathTime :: CriticalTime,
    nextSteps :: [Step]
} 

predList :: [Edge] -> StepList 
predList = L.foldl addPreds M.empty 

succList :: [Edge] -> StepList 
succList = L.foldl addSuccs M.empty

addPreds :: StepList -> Edge -> StepList
addPreds l (a,b) = insertWith add b [a] l

addSuccs :: StepList -> Edge -> StepList
addSuccs l (a,b) = insertWith add a [b] l

add :: [Step] -> [Step] -> [Step]
add ns n = nub (sort (n++ns))

startSteps :: StepList -> [Step]
startSteps m = sort (L.filter (\n -> not (n `elem` succs)) steps)
    where
    succs = L.concat (M.elems m)
    steps = M.keys m

steps :: [Edge] -> [Step]
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

makeCriticalPaths :: Time -> StepList -> Map Step Time
makeCriticalPaths base preds = addCriticalPath 0 M.empty target
    where
    target = head (L.filter (\n -> not(n `elem` allPreds)) steps)
    allPreds = L.concat (M.elems preds)
    steps = M.keys preds

    addCriticalPath :: Time -> Map Step Time -> Step -> Map Step Time
    addCriticalPath t m n = case M.lookup n preds of
        Nothing -> m'
        Just ps -> L.foldl' (addCriticalPath t') m' ps
        where
        t' = t + base + 1 + fromEnum n
        m' = M.insert n t' m

schedule ::  Int -> Time -> [Edge] -> Schedule
schedule n b es = Schedule 
    b 
    (replicate n []) 
    (succList es)
    preds
    (makeCriticalPaths b preds)
    []
    where
    preds = predList es

assignStep :: Schedule -> Step -> Schedule
assignStep sc s = sc { todos = replace i t' ts }
    where
    ts = todos sc
    i = snd (minimum (zip (L.map workLoad ts) [0..]))
    t' = Job s (stepTime s) : ts !! i
    
    replace i a as = (L.take i as) ++ [a] ++ (L.drop (succ i) as)

    stepTime s = fromEnum s + 1 + base sc

workLoad :: [Job] -> Time
workLoad = sum . L.map time

time :: Job -> Time 
time (Job _ t) = t
time (Idle t)  = t
    
idleUntil :: Step -> Schedule -> Schedule
idleUntil s sc = case findJob s (todos sc) of
   Nothing -> sc
   Just time -> sc { todos = fill time (todos sc) }

findJob :: Step -> [Todo] -> Maybe Time
findJob s [] = Nothing
findJob s (((Job s' t):js):ts) = if s' == s then Just (t + workLoad js) else findJob s ts
findJob s (((Idle _):_):ts) = findJob s ts
findJob s ([]:ts) = findJob s ts

fill :: Time -> [Todo] -> [Todo]
fill tt = L.map (\j -> if workLoad j < tt then (Idle (tt - workLoad j) : j) else j)
         
start :: Schedule -> Schedule
start sc = sc { nextSteps = firstSteps }
    where
    firstSteps :: [Step]
    firstSteps = sortBy (flip (comparing (\s -> s `M.lookup` (criticalPathTime sc)))) (startSteps (successors sc))

jobs = L.map reverse . todos
