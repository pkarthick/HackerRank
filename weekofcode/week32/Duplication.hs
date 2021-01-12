module Main (main) where

import Control.Monad

repeat' :: String -> String
repeat' [] = repeat' "0"
repeat' "0" = repeat' "01"
repeat' s 
    | len < 500 = repeat' $ s ++ rev s 
    | len == 500 = s 
    | otherwise = s ++ (rev . take (1001 - len)) s
    where 
        len = length s
        rev :: String -> String
        rev = map (\ch -> if ch == '1' then '0' else '1')


main :: IO ()
main = do
    line <- getLine
    let str = repeat' []
    replicateM_ (read line :: Int) $ do
        index <- getLine
        putChar $ str !! (read index :: Int)
        putStrLn ""
    
