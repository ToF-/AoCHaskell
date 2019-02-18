module Marbles
where
import Data.List

type Marble = Int
data Circle = Circle { 
    marbles :: [Marble],
    posCurrent :: Int,
    lastInserted :: Int }
    deriving (Eq)

instance Show Circle where
    show = showCircle 
    
circle :: Circle
circle = Circle [0] 1 0

current :: Circle -> Marble
current c = (marbles c) !! ((posCurrent c)-1)

next :: Int -> Int -> Int
next size pos | pos >= size = 1 +  pos + 2 - (size+1)
              | otherwise  = pos + 2
 
add :: Circle -> Circle
add (Circle [0] 1 0) = Circle [0,1] 2 1
add (Circle marbles pos last) = let
    pos' = next (length marbles) pos
    ins = pos' - 1
    marbles' = take ins marbles ++ return last' ++ drop ins marbles
    last' = succ last
    in Circle marbles' pos' last'

showCircle :: Circle -> String
showCircle (Circle ms pc _) = 
    concat (intersperse " " (zipWith showMarble ms [1..]))
    where
    showMarble :: Marble -> Int -> String
    showMarble m i | i == pc   = "(" ++ show m ++ ")"
                   | otherwise =       show m
