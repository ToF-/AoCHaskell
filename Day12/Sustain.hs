module Sustain
where
import Data.Maybe

type Pattern = [Int]
type Note = String

pattern :: Int -> String -> Pattern
pattern i s = i : map snd (filter (\(c,n) -> c=='#') (s `zip` [i..]))

plants :: Pattern -> String
plants p = plant' (head p) (tail p)
    where
    plant' _ [] = []
    plant' i (n:ns) | i == n = '#' : plant' (succ i) ns
                    | i < n = '.' : plant' (succ i) (n:ns)
                    | i > n = error "ill formed pattern"
                
offset :: Int -> Pattern -> String
offset o (n:ns) | o < n = take 5 ((replicate (n-o) '.') ++ plants (n:ns) ++ (repeat '.'))
offset o (n:ns)         = take 5 (drop (o - n) (plants (n:ns) ++ (repeat '.')))

sustainAt :: Pattern -> [Note] -> Int -> Maybe  Int
sustainAt p notes o = case any (== (offset o p)) notes of
    False -> Nothing
    True -> Just (o+2)

sustain :: Pattern -> [Note] -> Pattern
sustain p notes = case catMaybes (map (\o -> sustainAt p notes o) [start..end]) of
    [] -> []
    (n:ns) -> (n:n:ns)
    where
    start = head p - 5
    end = last p + 5

sustainN :: Int -> Pattern -> [Note] -> Pattern
sustainN n p notes = foldl (\acc _ -> sustain acc notes) p [1..n]

sumPots :: Pattern -> Int
sumPots (n:ns) = sum ns
