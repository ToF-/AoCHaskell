module Sustain
where
import Data.Maybe

type Pattern = String
type Notes = [(Pattern,Char)]
find :: Pattern -> Notes -> Bool
find p notes = maybe False (=='#') (lookup p notes)


numberPots :: Pattern -> [(Char,Int)]
numberPots p = ("....." ++ p ++ ".....") `zip` [-5..]

