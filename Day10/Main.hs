import Stars
import Small
import Large


viewStars :: [Star] -> Int -> Int -> IO ()
viewStars _ 0 _ = return ()
viewStars st n s | not (viewable (area st)) = viewStars (move 1 st) (pred n) (succ s)
viewStars st n s = do
    putStrLn (show s)
    putStrLn (view st)
    getLine
    viewStars (move 1 st) (pred n) (succ s)

main = do
    -- viewStars small 40
    viewStars large 40000 0
