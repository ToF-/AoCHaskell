module Marbles
where
import Data.List as L
import Data.Map as M
import Data.Sequence as S
import Data.Maybe

type Score = Int
type Player = Int
type Marble = Int
type Scores = Map Player Score

data Game = Game { circle :: Circle
                 , scores :: Scores
                 , player :: Player
                 , marble :: Marble }
    deriving (Eq,Show)

data Circle = Circle { marbles :: Seq Marble, pos :: Int }
    deriving (Eq,Show)

newCircle :: Marble -> Circle
newCircle n = Circle (S.singleton n) 0

atPos :: Circle -> Marble
atPos (Circle ms p) = fromJust (S.lookup p ms)

add :: Marble -> Circle -> Circle
add n (Circle ms p) = Circle (S.insertAt p' n ms) p'
    where p' = succ p

right :: Circle -> Circle
right (Circle ms p) = Circle ms ((succ p)`mod` S.length ms)

play :: Int -> Circle -> Circle
play n c = add n (right c)

remove :: Circle -> (Int,Circle)
remove (Circle ms p) = (fromJust (S.lookup p ms), Circle ms' p')
    where
    ms' = (S.deleteAt p ms) 
    p' = if p < S.length ms' then p else 0

left :: Circle -> Circle
left (Circle ms 0) = (Circle ms (S.length ms - 1))
left (Circle ms p) = (Circle ms (pred p))

win :: Circle -> (Int,Circle)
win = remove . left . left . left . left . left . left . left 

count :: Circle -> Int
count (Circle ms p) = S.length ms

game :: Int -> Game
game n = Game (newCircle 0) (M.fromList (L.zip [0..n-1] (repeat 0))) 0 1

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
