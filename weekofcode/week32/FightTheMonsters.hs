module Main (main) where

import Data.List

blast :: Int -> Int -> [Int] -> Int -> Int
blast t hit mh acc =
    case (t <= 0, mh) of
    (True, _) -> acc 
    (_, []) -> acc
    (_, _) -> 
        let 
            mhi = head mh
            hits = mhi `div` hit 
            used = if mhi `mod` hit == 0 then hits else hits + 1
        in  
            if t < used then acc else blast (t-used) hit (tail mh) (acc+1)

main :: IO ()
main = do
    line <- getLine
    let [_, hit, t] = map (read::String->Int) $ words line
    line2 <- getLine 
    let mh = sort $ map (read::String->Int) $ words line2
    print $ blast t hit mh 0
