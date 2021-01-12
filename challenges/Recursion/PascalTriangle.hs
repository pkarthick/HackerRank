import Prelude

loop :: [Int] -> [Int] -> [Int]
loop [] _ = [1]
loop [_] acc = 1 : (acc ++ [1])
loop [f,1] acc = loop [1] ((1+f):acc)
loop (1:f:t) acc =  loop (f:t) ((1+f):acc)  
loop (f:s:t) acc = loop (s:t) ((f+s):acc) 

printArr :: [Int] -> IO ()
printArr [1] = putStrLn "1"
printArr (x:xs) = do
    putStr $ show x
    putStr " "
    printArr xs

pascalTriangle :: Int -> [Int]
pascalTriangle 1 = [1]
pascalTriangle 2 = [1,1]
pascalTriangle n = 
    loop (pascalTriangle (n-1)) []
    
main :: IO ()
main = do
    line <- getLine
    mapM_ (printArr . pascalTriangle) [1..(read line :: Int)]


