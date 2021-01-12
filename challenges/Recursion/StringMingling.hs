
mingle :: String -> String -> IO ()
mingle [] [] = putStrLn ""
mingle (x:xs) (y:ys) = do
    putChar x
    putChar y
    mingle xs ys

main :: IO ()
main = do
    line1 <- getLine
    line2 <- getLine
    mingle line1 line2

