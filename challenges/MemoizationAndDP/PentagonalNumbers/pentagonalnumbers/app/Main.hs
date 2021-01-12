module Main where

import Control.Monad
import Data.Array

-- main :: IO ()
-- main = do
--     count <- readLn
--     input <- replicateM count (fmap (read::String->Int) getLine)
--     mapM_ (print . dotCountMemo) input
    

main :: IO ()
main = do
    count <- readLn
    input <- replicateM count (fmap (read::String->Int) getLine)
    let m = maximum input
    let arr = listArray (1,m) $ scanl (\a n -> a + (n-1) * 3 + 1) 1 [2..m]
    mapM_ (print . (arr!)) input
