import Control.Monad

bstCount :: Int -> Integer
bstCount = (map ways [0 .. ] !!)
    where
        ways :: Int -> Integer
        ways 0 = 1
        ways 1 = 1
        ways 2 = 2
        ways n = 
            count + if n `mod` 2 == 0 then 0 else bstCount h ^ (2::Integer)
            where
                h = n `div` 2
                count = 2 * sum [bstCount (n-i) * bstCount (i-1) | i <- [1 .. h]]

main :: IO()
main = do
    t <- (read::String->Int) <$> getLine

    replicateM_ t $ do
        n <- (read::String->Int) <$> getLine
        print $ bstCount n `mod` 100000007
