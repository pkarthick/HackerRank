module Main (main) where

import Control.Monad
import Data.List

printStr :: String -> IO ()
printStr s = do
    putChar $ head s
    when (len > 1) $ putStr $ show len
    where len = length s

main :: IO ()
main = do
    line <- getLine
    mapM_ printStr $ group line
    putStrLn ""
