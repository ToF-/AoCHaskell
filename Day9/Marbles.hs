module Marbles
where
import Data.List

type Marble = Int
data Circle = Circle { 
    marbles :: [Marble],
    posCurrent :: Int,
    lastInserted :: Int,
    scores :: [Int],
    player :: Int
    }
    deriving (Eq)

instance Show Circle where
    show = showCircle 
    
circle :: Int -> Circle
circle n = Circle [0] 1 0 (replicate n 0) 0

current :: Circle -> Marble
current c = (marbles c) !! ((posCurrent c)-1)

next :: Int -> Int -> Int
next size pos | pos >= size = 1 +  pos + 2 - (size+1)
              | otherwise  = pos + 2

prev :: Int -> Int -> Int
prev size pos | pos <= 7   = size + pos - 7
              | otherwise = pos - 7
 
add :: Circle -> Circle
add (Circle marbles pos last sc player) 
    | ((last+1) `mod` 23) == 0 = let
    
    pos'     = prev (length marbles) pos
    taken    = marbles !! (pos'-1)
    marbles' = delete taken marbles
    last'    = succ last
    score    = (sc !! player) + last' + (marbles !! (pos'-1))
    scores' =  replace player score sc
    player'= (succ player) `mod` (length sc)
    in Circle marbles' pos' last' (take 3 scores') player'

add (Circle marbles pos last scores player) = let
    pos' = next (length marbles) pos
    ins = pos' - 1
    marbles' = take ins marbles ++ return last' ++ drop ins marbles
    last' = succ last
    player' = (succ player') `mod` (length scores)
    in Circle marbles' pos' last' scores player'

replace :: Int -> a -> [a] -> [a]
replace n elem elems = (take (n-1) elems) ++ [elem] ++ (drop n elems)

showCircle :: Circle -> String
showCircle (Circle ms pc _ _ _) = 
    concat (intersperse " " (zipWith showMarble ms [1..]))
    where
    showMarble :: Marble -> Int -> String
    showMarble m i | i == pc   = "(" ++ show m ++ ")"
                   | otherwise =       show m

play :: Int -> Int -> Circle
play n p = (last (take (succ n) (iterate add (circle p))))
