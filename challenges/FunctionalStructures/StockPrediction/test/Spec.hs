import Lib
import Control.Monad
import System.IO

main :: IO ()
main = do
    
    (_:input:_:queries) <- lines <$> readFile "C:\\Fun\\HackerRank\\StockPrediction\\test\\testcase #5.txt"
    
    let xs = map (read:: String -> Int) $ words input
    let rootNode = createTree xs

    let pairs = map (map (read:: String -> Int) . words) queries
    results <- readFile "C:\\Fun\\HackerRank\\StockPrediction\\test\\testcase #5 output.txt"

    let ys = zip pairs (map (read:: String -> Int) $ lines results)

    rs <- mapM (
                \([d,m],r) -> 
                    let l = xs !! d 
                        h = l + m
                    in do
                        putStrLn "Starting next test case"

                        putStr "d="
                        print d
                        putStr "l="
                        print l
                        putStr "h="
                        print h

                        putStrLn "expected="
                        print r 
                        (_, _, val) <- longestSubArray' rootNode d l h
                        putStrLn "actual:"
                        print val
                        putStrLn ""

                        return $ val == r
                    ) ys

    mapM_ print rs
