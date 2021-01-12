module Main (main) where

import Text.Printf
import System.CPUTime
import Data.List

showLine :: Int -> Int -> String
showLine n k = replicate (k - 1) '.' ++ "X" ++ replicate (n - k) '.'

showSolution :: [Int] -> IO ()
showSolution s = mapM_ putStrLn [showLine len k | let len = length s, k <- s] >> putStrLn ""

queens :: Int -> [[Int]]
queens n = queens' n n

queens' :: Int -> Int -> [[Int]]
queens' _ 0 = [[]]
queens' n k = 
    [x:xs | xs <- queens' n (k - 1), x <- [1..n], 
    isSafeColumn x xs, isSafeDiagonal x xs, checkKnights xs]

isSafeColumn :: Int -> [Int] -> Bool
isSafeColumn = notElem

isSafeDiagonal :: Int -> [Int] -> Bool
isSafeDiagonal x xs = notElem (snd (abs . (x -) . fst)) $ zip xs [1..]
    
--    all ( ((/=) . snd) (abs . (x-) . fst)) $ zip xs [1..]
--isSafeDiagonal x xs = all (\(a, b) -> abs(x - a) /= b) $ zip xs [1..]

isKnight :: Int -> Int -> Bool
isKnight x1 x2 = d == 2 || d == 1 where d = abs(x1-x2)

checkKnights :: [Int] -> Bool
checkKnights [x1,x2] = not $ isKnight x1 x2
checkKnights (x3:x2:x1:_) = not $ isKnight x2 x3 || isKnight x1 x3 -- || isKnight (x1,y1) (x2,y2)
checkKnights _ = True 

-- unique :: [Int] -> [Int] -> Bool
-- unique x y = 
--     [length x + 1 - a | a <- reverse x ] == y 

isUnique :: Int -> [[Int]] -> [Int] -> Bool
isUnique n uniq x = 
    [n + 1 - a | a <- reverse x ] `elem` uniq 

main :: IO ()
main = do
    line <- getLine 
    start <- getCPUTime
    print $ length [comb | let n = read line :: Int, let allComb = queens n, comb <- allComb, isUnique n allComb comb] 
    end   <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.4f sec\n" (diff :: Double)
