{-# LANGUAGE DataKinds #-}

module Sustain
where
import Data.List as L (map, foldr, filter, elem)
import Data.Array as A
import Data.Map as M (empty, insert, lookup, Map,singleton)
import Data.Finite
import Data.Set as S (fromDistinctAscList, Set, member, elems, findMin, findMax, fromList,map)

type Note = Set (Finite 5)
type Notes = Set Note
type Pots    = Set Int 

type Pattern = Array Int Char
type Note' = Pattern

pattern :: String -> Pots
pattern = fromDistinctAscList . L.map fst . (L.filter ((=='#').snd) . zip [0..])

note :: String -> Note
note = fromDistinctAscList . L.map fst . (L.filter ((=='#').snd) . zip [0..])

notes :: [String] -> Notes
notes = fromList . L.map note

matches :: Notes -> Pots -> Int -> Bool
matches ns p i = ps `S.member` ns
    where
    ps = S.fromDistinctAscList . (flip filter finites)$ \j -> (i - 2 + fromIntegral j) `S.member` p
                 
generation :: Notes -> Pots -> Pots
generation ns p = fromDistinctAscList ((filter (\i -> matches ns p i) [findMin p - 2 .. findMax p + 2]))

normalize :: Pots -> Pots
normalize p = S.map (\n -> n - mn) p
    where mn = findMin p

type Start = Int
type Incr = Int
type Offset = Int

findRepetition :: Pots -> Notes -> Maybe (Start, Incr, Offset)
findRepetition p ns = findRepetition' 1 p (M.singleton p (0,0))
    where
    findRepetition` i p seen = case p'Norm `M.lookup` seen of
        Nothing -> findRepetition` (succ i) p' (M.insert p'Norm (i,mn) seen)
        Just (
