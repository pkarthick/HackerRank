module Main (main) where

import Control.Monad
import Data.Vector

s = fromList "ccaccbbbaccccca"
n = 15
-- check i k = (s ! i == 'a' && s ! k == 'c') || (s ! k == 'a' && s ! i == 'c')
-- split j =   length [ (i-1,k-1) |  k <- [2 .. j-1],                
-- let jsq = j^2, jsq `mod` k == 0, let i = jsq `div` k, i /= j, j /= k, i /= k, i < n, check (i-1) (k-1), i*k==jsq ]

check i k = (s ! i == 'a' && s ! k == 'c') || (s ! k == 'a' && s ! i == 'c')
split ind = Prelude.length [ (i-1,k-1) |  k <- [2 .. ind], let j = ind+1, let jsq = j^2, let i = jsq `div` k, i < n, check (i-1) (k-1), i*k==jsq ]
--split ind = length [ (i-1,k-1) |  k <- [2 .. ind], let j = ind+1, let jsq = j^2, jsq `mod` k == 0, let i = jsq `div` k, i /= j, j /= k, i /= k, i < n, check (i-1) (k-1), i*k==jsq ]

countComb s n lim ind acc 
    | ind > lim = acc
    | otherwise = countComb s n lim (ind+1) (if s ! ind /= 'b' then acc else acc + split ind)

main :: IO ()
main = --do
    -- n' <- getLine
    -- let n = (read :: String -> Int) n'
    -- s <- getLine
    -- print $ countComb s n 0 0
    print $ countComb s 15 (((+1) . round . sqrt) 15) 0 0