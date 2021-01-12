module Main where

    
import Control.Monad
import Lib

main :: IO ()
main = do
    _ <- getLine
    xs <- map (read:: String->Int) . words <$> getLine

    let rootNode = createTree xs 

    qn <- readLn
    replicateM_ qn $ do
        [d, m] <- map (read:: String->Int) .words <$> getLine
        let low = getElem d rootNode
        longestSubArray' rootNode d low (low + m)

