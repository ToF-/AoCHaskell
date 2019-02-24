module Sustain
where
import Data.Bits

type Start = Int
type Count = Int
type Pots = Integer
data Pattern = Pattern Start Count Pots
    deriving (Eq, Show)

initial :: String -> Pattern
initial = addBits . trimRight . trimLeft
    where
    addBits "" = error "ill formed initial pattern"
    addBits s = foldl addPot (Pattern 0 0 0) s
    addPot (Pattern s c pots) '#' = Pattern s (succ c) ((pots `shiftL` 1) `setBit` 0)
    addPot (Pattern s c pots) '.' = Pattern s (succ c) (pots `shiftL` 1)

    trimRight = reverse . trimLeft . reverse

    trimLeft = dropWhile (=='.')

extendRight :: Pattern -> Count -> Pattern
extendRight (Pattern s c pots) n = Pattern s (c + n) (pots `shiftL` n)

extendLeft :: Pattern -> Count -> Pattern
extendLeft (Pattern s c pots) n = Pattern (s-n) (c + n) pots

represent :: Pattern -> String
represent (Pattern s c pots) = replicate (c-length (repr)) '.' ++ repr
    where
    repr = reverse (addChar pots) 
    addChar 0 = "."
    addChar 1 = "#"
    addChar pots | odd pots  = '#' : addChar (pots `shiftR` 1)
                 | otherwise = '.' : addChar (pots `shiftR` 1)

normalize :: Pattern -> Pattern 
normalize = normalizeLeft . normalizeRight 
    where
    normalizeRight p@(Pattern _ 0 _) = p
    normalizeRight p@(Pattern s c pots) | odd pots  = p
                                        | otherwise = normalizeRight (Pattern s (pred c) (pots `shiftR` 1))
    normalizeLeft p@(Pattern _ 0 _) = p
    normalizeLeft p@(Pattern s c pots)  | pots `testBit` (pred c) = p 
                                        | otherwise = normalizeLeft (Pattern (succ s) (pred c) pots)
