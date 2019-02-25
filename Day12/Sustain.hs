module Sustain
where

type Pattern = (Int, String)

extendLeft l (n,s) = (n-l,replicate l '.' ++ s)
extendRight r (n,s) = (n,s ++ replicate r '.')
normalize (n,[]) = (n,[])
normalize (n,'#':s) = (n,'#': trimRight s)
    where trimRight = reverse . dropWhile (=='.') . reverse
normalize (n,'.':s) = normalize (succ n,s)
