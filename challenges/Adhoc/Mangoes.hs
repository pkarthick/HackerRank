module Main (main) where

import Data.List

-- fc = 5
-- mc = 200
-- m = [2, 5, 3, 2, 4]
-- h = [30, 40, 10, 20, 30]

-- line1 = "5 200"
-- (fc:mc:_) = map (read::String->Int) $ words line1

-- line2 = "2 5 3 2 4"
-- m = map ( read::String->Int ) $ words line2

-- line3 = "30 40 10 20 30"
-- h = map ( read::String->Int ) $ words line3

-- mhs = zip m h
-- sortedmhs = sortBy (\ (m1,h1) (m2,h2) -> if m1+h1 > m2+h2 then GT else LT ) mhs




friendsToInvite :: Int -> Int -> [Int] -> [Int] -> Int
friendsToInvite fc mc m h = 
    count sortedmhs (0,0) 0
    where
        mhs = zip m h
        sortedmhs = sortBy (\ (m1,h1) (m2,h2) -> if h1 > h2 then GT else if h1 == h2 then EQ else LT ) mhs
        
        count :: [(Int,Int)] -> (Int,Int) -> Int -> Int
        count [] _ ind = ind
        count ((m',h'):mhtail) (tm,th) ind 
            -- | acc == mc = ind+1
            | acc > mc = if ind < fc then ind else fc
            | otherwise = count mhtail (tm+m', th+h') (ind+1)
            where
                tm' = tm + m'
                th' = th + h'
                acc = tm' + (ind * th') 

main :: IO ()
main = do
    line1 <- getLine
    let (fc:mc:_) = map (read::String->Int) $ words line1

    line2 <- getLine
    let m = map ( read::String->Int ) $ words line2

    line3 <- getLine
    let h = map ( read::String->Int ) $ words line3

    print $ sortBy (\ (m1,h1) (m2,h2) -> if h1 > h2 then GT else LT ) $ zip m h

    print $ friendsToInvite fc mc m h
