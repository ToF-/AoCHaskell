module Sustain
where
import Data.List as L
import Data.Array as A

type Pattern = Array Int Char
type Note = Pattern

pattern s = listArray (0,length s-1) s

extendLeft l a = listArray (m-l,n) (replicate l '.' ++ elems a)
    where (m,n) = bounds a

extendRight r a = listArray (m,n+r) (elems a ++ replicate r '.')
    where (m,n) = bounds a

normalize a | a!m == '.' = normalize (listArray (succ m,n) (tail (elems a)))
    where (m,n) = bounds a

normalize a | a!n == '.' = normalize (listArray (m,pred n) (init (elems a)))
    where
    (m,n) = bounds a 

normalize a = a

sustain :: Pattern -> [Note] -> Pattern
sustain initial notes = let
    pots = extendRight 4 (extendLeft 4 initial)
    (start, end) = bounds pots
    resultBounds = (start+2,end-2)
    match i note = and [pots!(i-2+j) == note!j | j <- range (bounds note)]
    matches i = or (map (match i) notes)
    result = map (represent . matches) (range resultBounds)
    represent True = '#'
    represent False = '.'
    in normalize (listArray resultBounds result)

times n f a = foldr ($) a (replicate n f)
