module Main (main) where

import Control.Monad

rotateString :: String -> IO ()
rotateString str 
    | len == 1 = putStrLn str 
    | otherwise = rotateString' 1
    
    where    

        len = length str

        rotateString' :: Int -> IO( )
        rotateString' i 
            | len == i = putStrLn str    
            | otherwise = do
                putStr $ drop i str
                putStr $ take i str 
                putStr " "
                rotateString' $ i+1 
                
main :: IO ()
main = do
    count <- readLn
    strs <- replicateM count getLine
    mapM_ rotateString strs
