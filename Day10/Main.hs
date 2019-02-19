import Stars
import Small


viewStars :: [Star] -> Int -> IO ()
viewStars _ 0 = return ()
viewStars st n | not (viewable (area st)) = viewStars (move 1 st) (pred n)
viewStars st n = do
    putStrLn ""
    putStrLn (view st)
    getLine
    viewStars (move 1 st) (pred n)

main = do
    viewStars small 40
