module CircularList 
where
import Data.List

data CircularList a = CList { pre :: [a], post :: [a] }
    deriving (Eq)

instance Show a => Show (CircularList a) where
    show cl@(CList pre post) = interSpace [showPre pre,showHead post,showPost post]

showHead :: Show a => [a] -> String
showHead [] = "()"
showHead (a:_) = "(" ++ show a ++ ")"

showPre :: Show a => [a] -> String
showPre [] = ""
showPre as = showSimpleList (reverse as)

showPost :: Show a => [a] -> String
showPost l | length l < 2 = ""
           | otherwise = showSimpleList (tail l)

showSimpleList :: Show a => [a] -> String
showSimpleList = interSpace . map show 

interSpace :: [String] -> String
interSpace = concat . intersperse " " . filter (/= "")

empty = CList [] []

insert :: a -> CircularList a -> CircularList a 
insert a (CList pre post) = CList pre (a:post)

isEmpty :: CircularList a -> Bool
isEmpty (CList pre post) = null pre && null post

size :: CircularList a -> Int
size (CList pre post) = length pre + length post

shiftRight :: CircularList a -> CircularList a
shiftRight cl@(CList pre post) | isEmpty cl || size cl == 1 = cl
                               | otherwise = normalize (CList ((head post):pre) (tail post))

normalize :: CircularList a -> CircularList a
normalize cl | isEmpty cl = cl
normalize (CList [] post) = CList [] post
normalize (CList pre [])  = CList (init pre) (last pre:[])
normalize cl = cl
