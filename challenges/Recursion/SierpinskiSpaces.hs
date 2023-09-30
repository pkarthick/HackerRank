module Main (main) where

import Control.Monad
import Data.List
import Data.Char (chr)

base :: Char -> Int -> Int -> [String]
base ch r c =
    map row [1..r] 
    where
        row :: Int -> String
        row ri = 
            replicate uc ' ' ++ replicate ones ch ++ replicate uc ' '
            where
                ones = (ri-1) * 2 + 1
                uc = (c - ones) `div` 2

hw :: [String] -> (Int, Int)
hw rows = 
    (h,w) 
    where 
        h = length rows
        w = (length.head) rows

splitInto3 :: [String] -> [String]
splitInto3 rows = 
    zipWith (curry fractal) [0 ..] rows
    where 
        (h,w) = hw rows
        hh = h `div` 2
        
        fractal :: (Int, String) -> String
        fractal (i, row) 
            | i < hh = row
            | otherwise = take i row ++ replicate (w - i * 2) ' ' ++ drop (w-i) row

split :: Int -> [String] -> [String]
split n rows 
    | n == 1 = splitInto3 rows
    | otherwise = 
        let smallRows = split (n-1) segment
            (_, sw) = hw smallRows
            uc = (sw + 1) `div` 2
            top = map (\r -> let us = replicate uc ' ' in us ++ r ++ us) smallRows
            bottom = map (\r -> r ++ " " ++ r) smallRows
        in 
            top ++ bottom


    where    
        (h,w) = hw rows
        hh = h `div` 2
        mw = (w-1) `div` 2
        qw = (w+1) `div` 4
        segment = map (take mw. drop qw) $ take hh $ splitInto3 rows

sier :: Int -> [String]
sier n =
    if n == 0 
        then base '0' 32 63
        else split n $ base (chr (48 + n)) 32 63

main :: IO ()
main = do
    itr <- getLine
    mapM_ putStrLn $ sier $ (read :: String -> Int) itr
