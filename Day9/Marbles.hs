module Marbles
where
import Data.List

data Circle = Circle [Int] Int [Int]
    deriving Eq

instance Show Circle where
    show (Circle ls h rs) = interSpace [showL (reverse ls), "(" ++ show h ++ ")", showL rs]

showL = interSpace . map show

interSpace :: [String] -> String
interSpace = concat . intersperse " " . filter (/= "")

circle :: Int -> Circle
circle n = Circle [] n []

add :: Int -> Circle -> Circle
add n (Circle ls h rs) = Circle (h:ls) n rs

right :: Circle -> Circle
right (Circle [] h []) = Circle [] h []
right (Circle ls h []) = Circle [] (head rs) (tail rs)
    where rs = (reverse ls) ++ [h]
right (Circle ls h rs) = Circle (h:ls) (head rs) (tail rs)

play :: Int -> Circle -> Circle
play n c = add n (right c)

