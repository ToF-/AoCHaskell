module Sustain
where
import Data.Maybe

type Pattern = [Int]
type Note = (String,Char)

number :: Int -> String -> Pattern
number i s = map snd (filter (\(c,n) -> c=='#') (s `zip` [i..]))

patterns :: [Note] -> [Pattern]
patterns = map (number (-2)) . map fst . filter ((=='#').snd) 
    
    
