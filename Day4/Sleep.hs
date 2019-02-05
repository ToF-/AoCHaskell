module Sleep
where
import Data.List (transpose,sortBy)

data Activity = Awake
              | Asleep
    deriving (Eq,Show,Enum)


guard :: [Activity]
guard = replicate 60 Awake

sleepAt :: Int -> [Activity] -> [Activity]
sleepAt m a = take m a ++ replicate (60-m)  Asleep

wakeAt :: Int -> [Activity] -> [Activity]
wakeAt m a = take m a ++ replicate (60-m) Awake 

totalTimeAsleep :: [[Activity]] -> Int
totalTimeAsleep  = sum . map (sum . (map fromEnum))

sleepPerMinute :: [[Activity]] -> [Int]
sleepPerMinute = map (sum . (map fromEnum)) . transpose

sleepPerMinuteIx :: [[Activity]] -> [(Int,Int)]
sleepPerMinuteIx = sortBy criteria . flip zip [0..59] . sleepPerMinute
    where criteria (t,m) (t',m') = case flip compare t t' of
            EQ -> compare m m'
            r -> r
            
maxSleepMinute :: [[Activity]] -> Int
maxSleepMinute = snd . head . sleepPerMinuteIx
