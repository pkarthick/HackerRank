module Main (main) where

import Data.List (delete, sort, intersperse, nub)

-- line1 = "10"
-- line2 = "203 204 205 206 207 208 203 204 205 206"
-- line3 = "13"
-- line4 = "203 204 204 205 206 207 205 208 203 206 205 206 204"

-- mc = 10
-- nc = 13

-- m = map (read::String->Int) $ words line2
-- n = map (read::String->Int) $ words line4

-- let ((m1:mtail), (n1:ntail), miss) = (m, n, [])

missingNumbers :: [Int] -> [Int] -> [Int] -> [Int]

missingNumbers [] n nmiss = sort $ nub $ n ++ nmiss
missingNumbers m [] nmiss = missingNumbers m nmiss []
missingNumbers m@(mh:mtail) (nh:ntail) nmiss 
    | mh == nh = missingNumbers mtail ntail nmiss
    | otherwise = missingNumbers m ntail (nh:nmiss)


main :: IO ()
main = do
    _ <- getLine
    line2 <- getLine
    let m = map (read::String->Int) $ words line2
    _ <- getLine
    line4 <- getLine
    let n = map (read::String->Int) $ words line4
    putStrLn $ unwords $ map show $ missingNumbers m n []
