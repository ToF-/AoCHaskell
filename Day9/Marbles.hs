module Marbles
where
import Data.List as L
import Data.Map as M

type Score = Int
type Player = Int
type Marble = Int
type Scores = Map Player Score

data Game = Game { circle :: Circle
                 , scores :: Scores
                 , player :: Player
                 , marble :: Marble }
    deriving (Eq,Show)

data Circle = Circle [Int] Int [Int]
    deriving Eq

instance Show Circle where
    show (Circle ls h rs) = interSpace [showL (reverse ls), "(" ++ show h ++ ")", showL rs]

showL = interSpace . L.map show

interSpace :: [String] -> String
interSpace = concat . intersperse " " . L.filter (/= "")

newCircle :: Int -> Circle
newCircle n = Circle [] n []

add :: Int -> Circle -> Circle
add n (Circle ls h rs) = Circle (h:ls) n rs

right :: Circle -> Circle
right (Circle [] h []) = Circle [] h []
right (Circle ls h []) = Circle [] (head rs) (tail rs)
    where rs = (reverse ls) ++ [h]
right (Circle ls h rs) = Circle (h:ls) (head rs) (tail rs)

toList :: Circle -> [Int]
toList (Circle ls h rs) = ls ++ [h] ++ rs

play :: Int -> Circle -> Circle
play n c = add n (right c)

remove :: Circle -> (Int,Circle)
remove (Circle ls h []) = (h, Circle [] (head rs) (tail rs))
    where  rs = (reverse ls)
remove (Circle ls h rs) = (h, Circle ls (head rs) (tail rs))

left :: Circle -> Circle
left (Circle [] h []) = Circle [] h []
left (Circle ls h []) = Circle (tail ls) (head ls) [h]
left (Circle [] h rs) = Circle (reverse (h:(init rs))) (last rs) []
left (Circle ls h rs) = Circle (tail ls) (head ls) (h:rs)

win :: Circle -> (Int,Circle)
win = remove . left . left . left . left . left . left . left 

count :: Circle -> Int
count (Circle ls h rs) = 1 + length ls + length rs

game :: Int -> Game
game n = Game (newCircle 0) (M.fromList (zip [0..n-1] (repeat 0))) 0 1

move :: Game -> Game
move (Game circle scores player marble) = Game circle' scores' player' marble'
    where
    (score,(removed,circle')) = case marble `mod` 23 == 0 of
        False -> (0,(0,play marble circle))
        True -> (marble,win circle)
    scores' = M.insertWith (+) player (score + removed) scores
    player' = (succ player)`mod` (M.size scores)
    marble' = succ marble

moves :: Int -> Game -> Game
moves n g = L.foldl (\g _ -> move g) g [1..n]

highScore :: Game -> Score
highScore = L.maximum . M.elems . scores
