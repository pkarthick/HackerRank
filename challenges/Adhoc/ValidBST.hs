module Main (main) where

import Control.Monad

isValidBST :: [Int] -> Bool
isValidBST [] = True
isValidBST [_] = True
isValidBST [_,_] = True
isValidBST (x:xs) = 
    all (>x) right && isValidBST left && isValidBST right
    where
        (left, right) = span (<x) xs

main :: IO ()
main = do
    n <- readLn
    replicateM_ n $ do
        _ <- getLine
        line <- getLine
        let xs = map (read::String->Int) $ words line
        putStrLn $ if isValidBST xs then "YES" else "NO"
