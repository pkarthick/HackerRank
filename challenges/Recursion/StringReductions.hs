module Main (main) where

reduce :: String -> String
reduce s = 
    process s [] 
    where
        process :: String -> String -> String
        process [] uniq = 
            reverse uniq
        process (x:xs) uniq
            | x `elem` uniq = process xs uniq
            | otherwise = process xs (x:uniq)


main :: IO ()
main = do
    line <- getLine
    putStrLn $ reduce line
