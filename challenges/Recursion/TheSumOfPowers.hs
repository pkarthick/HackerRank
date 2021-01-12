module Main (main) where

import Data.List


sumOfPowers :: Int -> Int -> [[Int]]
sumOfPowers num expo = 
    if num == 1 then [[1]]
    else
        let limit = truncate $ fromIntegral num ** (1/fromIntegral expo)
        in sumOfPowers' [] [1..limit] []
    where
        sumOfPowers' :: [(Int,[Int])] -> [Int] -> [[Int]] -> [[Int]]
        sumOfPowers' [] (h:t) _ = sumOfPowers' [(h^expo, [h])] t []
        sumOfPowers' _ [] acc = acc
        sumOfPowers' cur (h:t) acc = 
            sumOfPowers' next t  acc'
            where
                new = [ (tot', h:lst) | (tot, lst) <- cur, let tot' =  tot + h ^ expo, tot' <= num ] ++ [(h^expo, [h])]
                next = new ++ cur
                acc' = acc ++ map snd (filter ((==num) . fst) new)

main :: IO ()
main = do
    num <- fmap (read::String->Int) getLine
    power <- fmap (read::String->Int) getLine
    print $ length $ sumOfPowers num power
