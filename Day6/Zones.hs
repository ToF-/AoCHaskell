module Zones
where
import Data.List
import Data.Maybe

type Zone = [[Distance]]

type Coord = (Int,Int)

data Point = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|X|Y|Z|
             A0|B0|C0|D0|E0|F0|G0|H0|I0|J0|K0|L0|M0|N0|O0|P0|Q0|R0|S0|T0|U0|V0|X0|Y0|Z0

    deriving (Eq, Show, Enum, Ord)

data Distance = Dist Point Int
              | Tie Int
    deriving (Eq, Show)


distances :: (Point,(Int,Int)) -> (Int,Int) -> Zone
distances (p,(x,y)) (w,h) =
    map (\row ->
        map (\col ->
            Dist p (abs (x-col)  + abs (y-row))
            ) [0..w-1]
        ) [0..h-1]

merge :: Zone -> Zone -> Zone
merge = zipWith mergeRow
    where 
    mergeRow :: [Distance] -> [Distance] -> [Distance]
    mergeRow = zipWith mergeCol

    mergeCol :: Distance -> Distance -> Distance
    mergeCol (Dist p d) (Dist q e) | d < e = (Dist p d)
                                   | d > e = (Dist q e)
                                   | d == e = (Tie d)
    mergeCol (Tie d)    (Dist q e) | d < e = (Tie d)
                                   | d > e = (Dist q e)
                                   | d == e = (Tie d)
    mergeCol (Dist p d) (Tie e) = mergeCol (Tie e) (Dist p d)

zone :: [Coord] -> Zone
zone coords = let
    (w,h) = ((maximum (map fst coords))+2, (maximum (map snd coords))+2)
    pts = zipWith (\i coords -> (toEnum i, coords)) [0..] coords
    in foldl1 merge (map (flip distances (w,h)) pts)

point (Dist p _) = Just p
point (Tie _)   = Nothing

infinites :: [Coord] -> [Point]
infinites coords = let
    z = zone coords
    points = catMaybes (concatMap (map point) 
            [head z
            ,last z
            ,head (transpose z)
            ,last (transpose z)])
    in nub (sort points)

largest :: [Coord] -> Int
largest coords = let
    z = zone coords
    inf = infinites coords
    points = sort (filter (\p -> not (p `elem` inf)) (catMaybes (concatMap (map point) z)))
    in head $ sortBy (flip compare) $ map length (group points)


