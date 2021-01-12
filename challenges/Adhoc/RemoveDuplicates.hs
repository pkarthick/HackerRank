module Main (main) where

printUnique :: String -> String -> IO ()

printUnique [] _ = putStrLn ""

printUnique (ch:str) acc = 
    if ch `elem` acc then 
        printUnique str acc
    else
        do
            putChar ch
            printUnique str (ch:acc)


main :: IO ()
main = do
    line <- getLine
    printUnique line []
    
    -- putStrLn $ reverse $ foldl (\acc x -> if x `elem` acc then acc else x : acc) [] line 
    
