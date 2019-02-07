module Repeats
where
import Data.List
import Data.Maybe

twice :: String -> Bool
twice = any (==2) . map length . group . sort

thrice :: String -> Bool
thrice = any (==3) . map length . group . sort

twiceAndThrice :: [String] -> (Int,Int)
twiceAndThrice ss = (count twice ss, count thrice ss)
    where
    count p = length . filter p

differ :: String -> String -> Bool
differ _ [] = False 
differ [] _ = False
differ (c:cs) (c':cs') | c /= c' = cs == cs'
                       | c == c' = differ cs cs'


common :: String -> String -> String
common s t = catMaybes (zipWith keep s t)
    where
    keep c c' | c /= c' = Nothing
              | otherwise = Just c

similar :: [String] -> String
similar ss = head (ss >>= \s -> map (common s) (filter (differ s) ss))




