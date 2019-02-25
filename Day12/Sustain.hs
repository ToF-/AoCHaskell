module Sustain
where
import Data.List

type Pattern = (Int, String)

extendLeft l (n,s) = (n-l,replicate l '.' ++ s)
extendRight r (n,s) = (n,s ++ replicate r '.')
normalize (n,[]) = (n,[])
normalize (n,'#':s) = (n,'#': trimRight s)
    where trimRight = reverse . dropWhile (=='.') . reverse
normalize (n,'.':s) = normalize (succ n,s)

sustain :: Pattern -> [String] -> Pattern
sustain (n,s) notes = let
    ps = transpose ["...."++s,"..."++s++".",".."++s++"..","."++s++"...",s++"...."]
    match t | t `elem` notes = '#'
            | otherwise = '.'
    in normalize (n-2, map match ps)

times n f a = foldr ($) a (replicate n f)
