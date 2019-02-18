module Marbles
where

type Marble = Int
type Circle = [Marble]
    
circle :: Circle
circle = [0]

current :: Circle -> Marble
current [0] = 0
