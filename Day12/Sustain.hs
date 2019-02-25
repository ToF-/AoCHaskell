module Sustain
where
import Data.List as L
import Data.Array as A

type Pattern = Array Integer Char
type Note = Array Integer Char

notes :: [String] -> Note
notes ss = map (listArray (0,4)) ss
initial s = listArray (0,length s) s

extendLeft l a = listArray (m-l,n) (replicate l '.' ++ elems a)
    where (m,n) = bounds a

extendRight r a = listArray (m,n+r) (elems a ++ replicate r '.')
    where (m,n) = bounds a

normalize a | bounds a == (0,0) = a

normalize a | a!m == '.' = listArray (succ m,n) (tail (elems a))
    where (m,n) = bounds a

normalize a | a!m == '#' = listArray (m,m+l) s
    where
    (m,n) = bounds a 
    s = reverse (dropWhile (=='.') (reverse (elems a)))
    l = length s


sustain :: Pattern -> [Array Int Char] -> Pattern
sustain a notes = let
    a' = extendRight 4 (extendLeft 4 a)
    (m,n) = bounds a
    ixs = range (bounds a')
    jxs = range (0,4)
    match i note = and [a'!i+j == note!j | j <- jxs]
    matches i = or (map (match i) notes)
    s = map (represent . matches) ixs
    represent True = '#'
    represent False = '.'
    in normalize (listArray (m-2,n+2) s)

times n f a = foldr ($) a (replicate n f)
