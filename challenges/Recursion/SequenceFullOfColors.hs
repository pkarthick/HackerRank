module Main (main) where

import Control.Monad

check :: String -> Int -> Int -> Bool
check [] rg yb = rg == 0 && yb == 0 
check (ch:t) rg yb =
    case ch of
        'R' -> rg < 1 && check t (rg+1) yb 
        'G' -> rg >= -1 && check t (rg-1) yb 
        'Y' -> yb < 1 && check t rg (yb+1) 
        'B' -> yb >= -1 && check t rg (yb-1) 
        _ -> False

isSequenceFullOfColors :: String -> Bool
isSequenceFullOfColors str =
    check str 0 0

main :: IO ()
main = do
    count <- readLn
    patterns <- replicateM count getLine
    mapM_ (print . isSequenceFullOfColors) patterns
