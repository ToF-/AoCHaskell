module Stars
where

type Star = (Coord,Velocity)
type Coord = (Int,Int)
type Velocity = (Int,Int)
type Area = (Coord,Coord)
type Surface = (Int,Int)
area :: [Star] -> Area 
area st = foldl getCoords initial (map fst st) 
    where
    initial = ((maxBound,maxBound),(minBound,minBound))
    getCoords :: Area -> Coord -> Area
    getCoords ((x0,y0),(x1,y1)) (x,y) =
        ((min x0 x, min y0 y),(max x1 x, max y1 y)) 

surface :: Area -> Surface
surface ((x0,y0),(x1,y1)) = ((1+(x1-x0)),(1+(y1-y0)))

height :: Surface -> Int
height = snd

width :: Surface -> Int
width = fst

position :: Star -> Coord
position = fst

velocity :: Star -> Velocity
velocity = snd

move :: Int -> [Star] -> [Star]
move n st = foldl (\st _ -> moveStars st) st [1..n]
    where
    moveStars :: [Star] -> [Star]
    moveStars = map moveStar
    
    moveStar :: Star -> Star
    moveStar ((x,y),(vx,vy)) = ((x+vx,y+vy),(vx,vy)) 

viewable :: Area -> Bool
viewable a = (height s) < 40 && (width s) < 80
    where s = surface a

view :: [Star] -> String
view st = let
    ((x0,y0),(x1,y1)) = area st
    pos = map position st
    rows = map drawLine [y0..y1] 
    drawLine row = map (drawCell row) [x0..x1]
    drawCell row col = case (col,row) `elem` pos of
                True  -> '*'
                False -> '.'
    in unlines rows
