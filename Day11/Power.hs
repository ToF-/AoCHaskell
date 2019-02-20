module Power
where
import Data.Array

type Serial = Int 
type Grid = Array Int (Array Int Int)
type Square = (Int,Int,Int)

powerLevel :: Serial -> Int -> Int -> Int
powerLevel serial x y =
    let rackID = x + 10
        level  = ((((rackID * y + serial) * rackID) `mod` 1000) `div` 100) - 5
    in level

square3x3Level :: Serial -> Int -> Int -> [[Int]]
square3x3Level serial x y = map rowLevels [y..y+2]
    where
    rowLevels row = map cellLevel [x..x+2]
        where 
        cellLevel col = powerLevel serial col row

powerSquare :: Serial -> (Int,(Int,Int))
powerSquare serial = maximum squares 
    where
    squares = (concatMap squareRow [1..298])
    squareRow row = map (squareCell row) [1..298]
    squareCell row col = (sum (map sum (square3x3Level serial col row)), (col,row))

grid :: Serial -> Grid
grid serial = array (1,300) (map rowArray [1..300])
    where
    rowArray :: Int -> (Int, Array Int Int)
    rowArray row = (row, array (1,300) (map (cell row) [1..300]))
    cell row col = (col,powerLevel serial col row)

squareLevel :: Grid -> Int -> Int -> Int -> Int
squareLevel grid x y size = foldl (\acc row -> acc + rowLevel row) 0 [y..y+size-1]
    where
    rowLevel row = foldl (\acc col -> acc + grid ! row ! col) 0 [x..x+size-1]

squares :: Int -> [Square]
squares m = [(x,y,s)| x <- [1..m], y<-[1..m], s <-[1..min (m+1-x) (m+1-y)]]

bestSquare :: Grid -> (Int,Square)
bestSquare grid = foldl (\(acc,best) square -> compareSquare (acc,best) square) (minBound,(1,1,1)) (squares 300)
    where
    compareSquare (acc,best) (x,y,s) = case compare sl acc of
        GT -> (sl,(x,y,s))
        _  -> (acc,best)
        where
        sl = squareLevel grid x y s
         

partialSums :: Array Int (Array Int Int) -> Array Int (Array Int Int)
partialSums grid = array (a,z) (zipWith grow [a..z] (tail (scanl rowSums initial [a..z])))
    where
    (a,z) = bounds grid
    initial = (0,array (a,z) (zip [a..z] [0..]))
    grow :: Int -> (Int,Array Int Int) -> (Int,Array Int Int)
    grow ix (acc,ar) = (ix, ar)

    rowSums :: (Int,Array Int Int) -> Int -> (Int,Array Int Int)
    rowSums (acc,ar) ix = (acc',sums)
        where
        (a,z) = bounds ar
        sums = array (a,z) (zip [a..z] (tail (scanl (+) acc (elems (grid!ix)))))   
        acc' = acc + sums!a
