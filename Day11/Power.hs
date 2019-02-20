module Power
where
import Data.Array
import Data.List

type Serial = Int 
data Grid = Grid (Array Int (Array Int Int))
    deriving (Eq,Show)
data PSTable = PSTable (Array Int (Array Int Int))
    deriving (Eq,Show)
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
grid serial = Grid (array (1,300) (map rowArray [1..300]))
    where
    rowArray :: Int -> (Int, Array Int Int)
    rowArray row = (row, array (1,300) (map (cell row) [1..300]))
    cell row col = (col,powerLevel serial col row)

squares :: Int -> [Square]
squares m = [(x,y,s)| x <- [1..m], y<-[1..m], s <-[1..min (m+1-x) (m+1-y)]]

bestSquare :: PSTable -> (Int,Square)
bestSquare (PSTable table) = foldl (\(acc,best) square -> compareSquare (acc,best) square) (minBound,(1,1,1)) (squares 300)
    where
    compareSquare :: (Int,Square) -> Square -> (Int,Square)
    compareSquare (best,square) (x,y,size) = max (best ,square) (level,(x,y,size))
        where
        level = squareSum (PSTable table) x y size
         

partialSums :: Grid -> PSTable
partialSums (Grid grid) = psTable 
    where
    es = map elems (elems grid)
    sums = transpose (map (scanl1 (+)) (transpose (map (scanl1 (+)) es)))
    psTable = PSTable (listArray bs (map (listArray bs) sums))
    bs = bounds grid

squareSum :: PSTable -> Int -> Int -> Int -> Int
squareSum ps x y s = rectangleSum ps x y s s

rectangleSum :: PSTable -> Int -> Int -> Int -> Int -> Int
rectangleSum (PSTable ps) 1 1 w h = ps!h!w 
rectangleSum (PSTable ps) 1 y w h = 0              + ps!(y-1+h)!     w  - ps!(y-1)!     w  - 0
rectangleSum (PSTable ps) x 1 w h = 0              + ps!     h !(x-1+w) - 0                - ps!     h !(x-1)
rectangleSum (PSTable ps) x y w h = (ps!(y-1)!(x-1)) + (ps!(y-1+h)!(x-1+w)) - (ps!(y-1)!(x-1+w)) - (ps!(y-1+h)!(x-1))

