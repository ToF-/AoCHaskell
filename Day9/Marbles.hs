module Marbles
where
import Data.List as L 
import Data.Map as M
import Data.Maybe

type Score = Int
type Player = Int
type Marble = Int
type Scores = Map Player Score

data Circle = Circle { 
    marbles :: [Marble],
    current :: Int,
    scores  :: Scores,
    player  :: Player,
    marble  :: Marble
    }
    deriving (Eq)

instance Show Circle where
    show = showCircle 
    
circle :: Int -> Circle
circle n = Circle [0] 1 (newScores n) 0 1

newScores :: Int -> Scores 
newScores n = M.fromList (zip [0..n-1] (repeat 0))

next :: Int -> Int -> Int
next size pos | pos >= size = 1 +  pos + 2 - (size+1)
              | otherwise  = pos + 2

prev :: Int -> Int -> Int
prev size pos | pos <= 7   = size + pos - 7
              | otherwise = pos - 7
 
add :: Circle -> Circle
add (Circle marbles current scores player marble) 
    | (marble `mod` 23) == 0 = let
    
    current' = prev (length marbles) current
    taken    = marbles !! (current'-1)
    marbles' = L.delete taken marbles
    scores' =  M.insertWith (+) player (taken+marble) scores
    player'= (succ player) `mod` (M.size scores)
    in Circle marbles' current' scores' player' (succ marble) 


add (Circle marbles current scores player marble) = let
    current' = next (length marbles) current
    (before, after) = L.splitAt (current'-1) marbles    
    marbles' = L.concat [before, [marble], after]
    player'= (succ player) `mod` (M.size scores)
    in Circle marbles' current' scores player' (succ marble)

showCircle :: Circle -> String
showCircle (Circle ms pc _ _ _) = 
    concat (intersperse " " (zipWith showMarble ms [1..]))
    where
    showMarble :: Marble -> Int -> String
    showMarble m i | i == pc   = "(" ++ show m ++ ")"
                   | otherwise =       show m

play :: Int -> Int -> Circle
play n p = (last (L.take (succ n) (iterate add (circle p))))

highScore :: Circle -> Score
highScore = maximum . M.elems . scores 
