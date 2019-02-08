module Reduction
where
import Data.Char

-- reduce :: String -> String 
-- reduce (b:c:c':cs) | not (opposite b c) && opposite c c' = reduce (b:cs)
--                    | (opposite b c) = reduce (c':cs)
--                    | otherwise      = b: reduce (c:c':cs)
-- reduce (c:c':cs) | opposite c c' = reduce cs
--                  | otherwise = c : reduce (c':cs)
-- reduce s = s


opposite :: Char -> Char -> Bool
opposite c c' = toUpper c == toUpper c' && c /= c' 

reduce :: String -> String
reduce s | reduction s == s = s
         | otherwise       = reduce (reduction s)

reduction :: String -> String
reduction (c:c':cs) | opposite c c' = reduction cs
                    | otherwise     = c : reduction (c':cs)
reduction s = s
