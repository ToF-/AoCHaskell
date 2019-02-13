module Steps 
where
import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Ord

type Edge = (Step,Step)
type StepList = Map Step [Step]
type Time = Int

data Step = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
    deriving (Eq,Ord,Show,Enum)

data Work = Work Step Time | Idle Time
    deriving (Eq,Show)
           
type Worker = [Work]

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

assign :: Step -> Time -> [Worker] -> [Worker]
assign s t ws = assignWorker leastLoaded s t ws
    where
    leastLoaded :: Int
    leastLoaded = snd (head (sortBy (comparing workLoad)
         (zip ws [0..])))

    workLoad :: (Worker,Int) -> Time
    workLoad = sum . L.map workTime . fst
        where 
        workTime :: Work -> Time
        workTime (Work _ t) = t
        workTime (Idle t)   = t
    
    assignWorker :: Int -> Step -> Time -> [Worker] -> [Worker]
    assignWorker 0 s t (w:ws) = ((w++[Work s t]):ws)
    assignWorker n s t (w:ws) = w : assignWorker (pred n) s t ws 

    

