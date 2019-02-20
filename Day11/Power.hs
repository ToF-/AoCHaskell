module Power
where

type Serial = Int 
powerLevel :: Serial -> Int -> Int -> Int
powerLevel serial x y =
    let rackID = x + 10
        level  = ((((rackID * y + serial) * rackID) `mod` 1000) `div` 100) - 5
    in level

squareLevel :: Serial -> Int -> Int -> [[Int]]
squareLevel serial x y = map rowLevels [y..y+2]
    where
    rowLevels row = map cellLevel [x..x+2]
        where 
        cellLevel col = powerLevel serial col row

powerSquare :: Serial -> (Int,(Int,Int))
powerSquare serial = maximum squares 
    where
    squares = (concatMap squareRow [1..298])
    squareRow row = map (squareCell row) [1..298]
    squareCell row col = (sum (map sum (squareLevel serial col row)), (col,row))
