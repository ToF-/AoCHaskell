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
           
type Worker = [Job]
type Schedule = [Worker]

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

criticalPaths :: Time -> StepList -> Map Step Time
criticalPaths base preds = addCriticalPath 0 M.empty target
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

indexOfFirstDone :: Schedule -> Index
indexOfFirstDone ws = snd (head (sortBy (comparing (timeWhenDone . fst)) (zip ws [0..])))

timeWhenDone :: Worker -> Time
timeWhenDone = sum . L.map workTime 
    where 
    workTime :: Job -> Time
    workTime (Job _ t) = t
    workTime (Idle t)   = t

assign :: Step -> Time -> Schedule -> Schedule
assign s t sch = assignWorker (indexOfFirstDone sch) s t sch
    where
    
    assignWorker :: Int -> Step -> Time -> Schedule -> Schedule
    assignWorker 0 s t (w:ws) = ((w++[Job s t]):ws)
    assignWorker n s t (w:ws) = w : assignWorker (pred n) s t ws 

stepsDone :: Schedule -> [Step]
stepsDone sch = stepsDoneAt (timeWhenDone (sch!!(indexOfFirstDone sch))) sch

stepsDoneAt :: Time -> Schedule -> [Step]
stepsDoneAt t sch = concatMap (fst . (L.foldl (addStepsBefore t) ([],0))) sch
    where
    addStepsBefore :: Time -> ([Step],Time) -> Job -> ([Step],Time)
    addStepsBefore t (w,tt)  j@(Idle jt) = (w,tt+jt)
    addStepsBefore t (w,tt) j@(Job s jt) | tt + jt <= t = (w ++ [s],tt+jt)
                                         | otherwise = (w,tt)

nextSteps :: Schedule -> StepList -> StepList -> Map Step Time -> [Step]
nextSteps sch succs preds cp = sortBy (flip (comparing (`M.lookup` cp))) next
    where
    next = L.filter (allPrecsDone) (concat (catMaybes (L.map (`M.lookup` succs) done)))
    allPrecsDone s = case M.lookup s preds of
        Nothing -> True
        Just ps -> all (\s -> not (s `elem` doing sch) && (s `elem` done)) ps
    done = stepsDone sch



doing :: Schedule -> [Step]
doing sch = L.filter (not . (`elem` (stepsDone sch))) (catMaybes (L.map step (concat sch)))
    where
    step :: Job -> Maybe Step
    step (Job s _) = Just s
    step (Idle _) = Nothing
