module CircularList 
where

type CircularList a = ([a],a,[a])
clist n  = ([],n,[])

showCList :: Show a => CircularList a -> String
showCList (pre,head,post) = show (reverse pre) ++ show head ++ show post

add :: a -> CircularList a -> CircularList a
add a (pre,head,post) = (head:pre,a,post)  

shiftRight :: CircularList a -> CircularList a
shiftRight ([],head,[]) = ([],head,[])
shiftRight ([pre],head,[]) = ([head],pre,[])
