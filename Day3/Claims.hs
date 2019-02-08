module Claims
where
import Data.Map as M
import Data.List as L

type ClaimId = Int
type X = Int
type Y = Int
type W = Int
type H = Int
type Patch = (ClaimId,X,Y,W,H)
type Coord = (X,Y)

disputed :: [Patch] -> Int
disputed = length . L.filter (>=2) 
    . M.elems . mapCoords . L.concatMap coords
    where 
    coords :: Patch -> [Coord]
    coords (_,x,y,w,h) = [x..x+w-1] >>= (\x ->
        L.map (\y -> (x,y)) [y..y+h-1])

mapCoords :: [Coord] -> Map Coord Int
mapCoords = L.foldl (\map cd -> insertWith (+) cd 1 map)  M.empty 

overlap :: Patch -> Patch -> Bool
overlap (_,x0,y0,w0,h0) (_,x1,y1,w1,h1) | x0 >= x1+w1 = False
                                        | x1 >= x0+w0 = False
                                        | y0 >= y1+h1 = False
                                        | y1 >= y0+h0 = False
                                        | otherwise = True
    
