import Control.Applicative
import Control.Monad
import System.IO

count :: Int -> Int
count 0 = 0
count ac = (ac+1) `div` 2 + count (ac-1)

-- altCount :: Int -> Int
-- altCount 1 = 0
-- altCount n = count n + altCount (n-1)

zeroCount :: Int -> Int
zeroCount 0 = 0
zeroCount 1 = 1
zeroCount 2 = 3
zeroCount n = n + zeroCount (n-1) 

sameOccurrence :: [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int
sameOccurrence [] _ _ _ ac zc oc = return $ oc + zeroCount zc + count ac 
sameOccurrence [a] x y _ ac zc oc = return $ oc + count ac + zeroCount (if a /= x && a /= y then zc + 1 else zc)
sameOccurrence [f,s] x y p ac zc oc 
    | (p == 0 || p == y) && f == x && s == y = sameOccurrence [] x y 0 (ac+1) 0 (oc + zeroCount zc)
    | (p == 0 || p == x) && f == y && s == x = sameOccurrence [] x y 0 (ac+1) 0 (oc + zeroCount zc)
    | f /= x && f /= y = sameOccurrence [] x y 0 0 (zc+2) (oc + count ac) 
    | f == x || f == y = sameOccurrence [] x y 0 0 0 (oc + count ac + zeroCount zc) 
    | f /= x || f /= y || s /= x || s /= y = sameOccurrence [] x y 0 0 (zc+1) (oc + count ac) 
    | otherwise = return 0

sameOccurrence (f:s:t) x y p ac zc oc 
    | (p == 0 || p == y) && f == x && s == y = sameOccurrence (s:t) x y f (ac+1) 0 (oc + zeroCount zc)
    | (p == 0 || p == x) && f == y && s == x = sameOccurrence (s:t) x y f (ac+1) 0 (oc + zeroCount zc)
    | f /= x && f /= y = sameOccurrence (s:t) x y f 0 (zc+1) (oc + count ac) 
    | f == x || f == y = sameOccurrence (s:t) x y f 0 0 (oc + count ac + zeroCount zc) 
    | otherwise = return 0

main :: IO ()
main = do
    n_temp <- getLine
    let n_t = words n_temp
    let _ = read $ head n_t :: Int
    let q = read $ n_t!!1 :: Int
    arr_temp <- getLine
    let arr = map read $ words arr_temp :: [Int]
    forM_ [1..q] $ \_  -> do
        x_temp <- getLine
        let x_t = words x_temp
        let x = read $ head x_t :: Int
        let y = read $ x_t!!1 :: Int
        res <- sameOccurrence arr x y 0 0 0 0
        print res

{- 
3 2
1 2 1
1 2
4 5

-}
