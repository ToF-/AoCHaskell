module Reduction
where
import Data.Char
import Data.List

opposite :: Char -> Char -> Bool
opposite c c' = toUpper c == toUpper c' && c /= c' 


reduce :: String -> String
reduce = foldr reduction ""
    where
    reduction c (d:ds) | opposite c d = ds
    reduction c cs = c : cs 

reduce' :: String -> String
reduce' = foldl (flip reduction) ""
    where
    reduction c (d:ds) | opposite c d = ds
    reduction c cs = c : cs 

remove :: String -> Int
remove s = head (sort 
    (map (length . reduce . flip removeUnit s) ['A'..'Z']))

removeUnit :: Char -> String -> String
removeUnit p = filter (not.(== toUpper p).toUpper)  
     
