module Main (main) where

import Control.Monad

area :: (Num a, Floating a, Ord a) => ((a, a), (a,a), (a,a)) -> a
area  ((x1,y1), (x2,y2), (x3,y3)) = 
    abs $ (x1*(y2-y3)+x2*(y3-y1)+x3*(y1-y2))/2

isOutside :: (Num a, Floating a, Ord a) => [(a, a)] -> (a,a) -> Bool
isOutside [(x1,y1), (x2,y2), (x3,y3)] (x,y) =
    a1 == 0 || a2 == 0 || a3 == 0 || a1 + a2 + a3 /= a
    where
    a1 = area ((x,y), (x2,y2), (x3,y3)) 
    a2 = area ((x1,y1), (x,y), (x3,y3)) 
    a3 = area ((x1,y1), (x2,y2), (x,y)) 
    a = area ((x1,y1), (x2,y2), (x3,y3))

isConvex :: (Num a, Floating a, Ord a) => [(a,a)] -> Bool -> Bool
isConvex _ False = False
isConvex [_,_] True = True
isConvex (f:s:t:pts) True = all (isOutside [f,s,t]) pts && isConvex (f:t:pts) True

main :: IO ()
main = do

    n <- readLn
    allLines <- replicateM n getLine
    let xys = map ( (\ [a,b] -> (a, b) ) . map (read::String->Float) . words) allLines
    putStrLn $ if isConvex xys True then "NO" else "YES"
