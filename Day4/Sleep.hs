module Sleep
where
import Data.List as L
import Data.Map as M

data Guard = Guard GuardId
           | Sleep
           | Wake
    deriving (Eq,Show,Ord)

data Activity = Awake
              | Asleep
    deriving (Eq,Show,Enum)

type GuardId = Int
type Total = Int
type Minute = Int
type Event = (Int,Int,Int,Int,Minute, Guard)

guard :: [Activity]
guard = replicate 60 Awake

sleepAt :: Int -> [Activity] -> [Activity]
sleepAt m a = L.take m a ++ replicate (60-m)  Asleep

wakeAt :: Int -> [Activity] -> [Activity]
wakeAt m a = L.take m a ++ replicate (60-m) Awake 

totalTimeAsleep :: [[Activity]] -> Int
totalTimeAsleep  = sum . L.map (sum . (L.map fromEnum))

sleepPerMinute :: [[Activity]] -> [Int]
sleepPerMinute = L.map (sum . (L.map fromEnum)) . transpose

sleepPerMinuteIx :: [[Activity]] -> [(Int,Int)]
sleepPerMinuteIx = sortBy criteria . flip zip [0..59] . sleepPerMinute
    where criteria (t,m) (t',m') = case flip compare t t' of
            EQ -> compare m m'
            r -> r
            
maxSleepMinute :: [[Activity]] -> Int
maxSleepMinute = snd . head . sleepPerMinuteIx

collect :: [Event] -> Map Int [[Activity]]
collect evts = snd  (L.foldl addEvent (Nothing ,M.empty) (L.sort evts))
    where
    addEvent :: (Maybe GuardId, Map GuardId [[Activity]]) -> Event ->
                (Maybe GuardId, Map GuardId [[Activity]]) 

    addEvent (_, map) (_,_,_,_,_,Guard g') = 
        (Just g', M.insertWith (\[a] as -> a : as) g' [guard] map)

    addEvent (Just g,map) (_,_,_,_,m,Sleep) =
        (Just g, M.adjust (\(a:as) -> ((sleepAt m a) : as)) g map)

    addEvent (Just g,map) (_,_,_,_,m,Wake) =
        (Just g, M.adjust (\(a:as) -> ((wakeAt m a) : as)) g map)
