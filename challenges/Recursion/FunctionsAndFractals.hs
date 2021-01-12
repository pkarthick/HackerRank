module Main where

import Control.Monad

u :: Int -> IO ()
u c = replicateM_ c $ putChar '_'

o :: IO ()
o = putChar '1'

nl :: IO ()
nl = putStrLn ""

drawEmpty :: Int -> IO ()
drawEmpty c = 
    replicateM_ c $ do
        u 100
        nl
    
drawV :: Int -> Int -> IO ()
drawV hec l = do
    u (hec-1)
    u (hec-l)
    o

    u (l-1)
    u 1
    u (l-1)

    o
    u (hec-l)
    u hec 

drawTop :: Int -> IO ()
drawTop n =
    mapM_ 
    (\l -> 
        do
            u 18 
            replicateM_ (2^(n-1)) $ drawV hec l
            u 18 
            nl
        ) [hec, hec-1..1]
    where
        ec = 2 ^ (6-n)
        hec = ec `div` 2
                
drawBottom :: Int -> IO ()
drawBottom n =
    replicateM_ hec $ do 
        u 18 

        replicateM_ (2^(n-1)) $ do
            u $ ec-1
            o
            u ec 

        u 18 
        nl

    where
        ec = 2 ^ (6-n)
        hec = ec `div` 2

drawSegment :: Int -> IO ()
drawSegment n = do
    drawTop n
    drawBottom n

drawTree :: Int -> IO ()
drawTree n = do
    drawEmpty (2^(6-n)-1)  --31 15 8 3 1
    mapM_ drawSegment [n, n-1 .. 1]

main :: IO ()
main = 
    read <$> getLine >>= drawTree
    
    

