module CircularList 
where
import Data.List

data CircularList a = CList { anti :: [a], noon :: a, clock :: [a] } 
    deriving (Eq)

instance Show a => Show (CircularList a) where
    show cl@(CList anti noon clock) = interSpace 
        [showSimpleList (reverse anti)
        ,shownoon noon
        ,showSimpleList (clock)]

shownoon :: Show a => a -> String
shownoon a = "(" ++ show a ++ ")"

showSimpleList :: Show a => [a] -> String
showSimpleList = interSpace . map show 

interSpace :: [String] -> String
interSpace = concat . intersperse " " . filter (/= "")

clist :: a -> CircularList a
clist a = CList [] a []

insert :: a -> CircularList a -> CircularList a 
insert a (CList anti noon clock) = CList (noon:anti) a clock

shift :: CircularList a -> CircularList a
shift (CList [] noon []) = CList [] noon []
shift (CList [a] b []) = CList [] a [b]
shift (CList [] a [b]) = CList [a] b []
shift (CList anti noon []) = CList (noon:(init anti)) (last anti) []
