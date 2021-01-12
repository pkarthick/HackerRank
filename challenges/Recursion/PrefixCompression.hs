module Main (main) where

import Control.Monad

prefix :: String -> String -> (String, String, String)
prefix x y =
    (p, x', y')
    where
        plen :: String -> String -> Int -> Int        
        plen (a:xs) (b:ys) i
            | a /= b = i 
            | otherwise = plen xs ys (i+1)

        plen _ _ i = i

        len = plen x y 0
        p = take len x
        x' = drop len x
        y' = drop len y  

printStr :: String -> IO ()
printStr s = do
    (putStr . show . length) s
    unless (null s) $ putChar ' '
    putStrLn s

printPrefix :: String -> String -> IO ()
printPrefix x y = do
    printStr p
    printStr x'
    printStr y'
    where (p, x', y') = prefix x y


main :: IO ()
main = do
    x <- getLine
    y <- getLine
    printPrefix x y
    