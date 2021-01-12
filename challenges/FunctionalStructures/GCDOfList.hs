module Main where

import Control.Monad
import Data.Map (Map, fromList, keysSet, (!))
import qualified Data.Set as S

pair :: [Int] -> (Int,Int)
pair (f:s:_) = (f,s)

toPairs :: [(Int,Int)] -> [Int] -> Map Int Int
toPairs acc [] = fromList acc
toPairs acc xs = toPairs (pair xs:acc) (drop 2 xs) 

commonFactors :: [Map Int Int] -> [Int]
commonFactors maps =
    S.toList $ foldl1 S.intersection $ map keysSet maps

getExponents :: [Map Int Int] -> Int -> Int
getExponents maps f =
    minimum $ map ( ! f ) maps

main :: IO ()
main = do
    n <- readLn
    xss <- replicateM n $ fmap (toPairs [] . map (read::String->Int) . words) getLine

    let cfs = commonFactors xss

    -- mapM_ print xss
    -- print cfs

    mapM_ (\ f -> do
        putStr $ show f
        putChar ' '
        putStr $ show $ getExponents xss f
        putChar ' ') cfs
    putStrLn ""
