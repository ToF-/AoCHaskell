module Marbles
where

data Circle = Circle Int
    deriving Eq

instance Show Circle where
    show (Circle n) = "(" ++ show n ++ ")"

circle :: Int -> Circle
circle n = Circle n
