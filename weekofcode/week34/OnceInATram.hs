module Main where

import Data.List
import Data.Maybe

getSum :: Int -> Int
getSum num = (num `mod` 10) + ((num `div` 10) `mod` 10) + (num `div` 100)

isLucky :: Int -> Bool
isLucky num = 
    getSum f == getSum s
    where
        f = num `div` 1000
        s = num `mod` 1000
        
getNextLucky :: Int -> Int
getNextLucky num = 
    fromJust $ find isLucky [num+1 .. ]
    

main :: IO ()
main = do
    line <- getLine
    let num = (read::String->Int) line
    print $ getNextLucky num

