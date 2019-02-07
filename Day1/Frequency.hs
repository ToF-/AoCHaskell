module Frequency
where
import Data.IntSet as S
import Data.Either
import Control.Monad

resultingFrequency :: [Int] -> Int
resultingFrequency = sum

frequencyFoundTwice :: [Int] -> Int
frequencyFoundTwice = either id (error "Nope")
    . foldM findTwice S.empty 
        . scanl (+) 0 
            . cycle 
    where 
    findTwice :: IntSet -> Int -> Either Int IntSet
    findTwice set x | S.member x set = Left x
                    | otherwise      = Right (S.insert x set)
        
