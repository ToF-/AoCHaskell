module Sustain
where
import Data.Maybe

type Pattern = [Int]
type Note = (String,Char)

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
offset o (n:ns) = (replicate (n-o) '.') ++ plants (n:ns)
