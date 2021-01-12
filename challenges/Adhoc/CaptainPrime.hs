module Main where

import Control.Monad

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n =
    let p = (floor . sqrt . fromIntegral) n
    in null [ x | x <- [2 .. p], n `mod` x == 0]

isLeft :: Int -> Int -> Bool
isLeft n d
    | r < d = False
    | otherwise = isPrime r && (d' > n || isLeft n d')
    where
        d' = d * 10
        r = n `mod` d'

isRight :: Int -> Bool
isRight n 
    | n < 10 = isPrime n 
    | otherwise = isPrime n && isRight (n `div` 10) 
    
isCentral :: Int -> Bool
isCentral n = isLeft n 1 && isRight n

getType :: Int -> String
getType n 
    | isCentral n = "CENTRAL"
    | isLeft n 1 = "LEFT"
    | isRight n = "RIGHT"
    | otherwise = "DEAD"

main :: IO ()
main = do
    count <- fmap (read::String->Int) getLine
    xs <- replicateM count ((read::String->Int) <$> getLine)
    mapM_ (putStrLn . getType) xs

