import Data.List
import Data.Ord

maxGcdSum :: [Int] -> [Int] -> IO Int
maxGcdSum oas obs =
    gcdMaxSum (0,0) oas obs
    where
    gcdMaxSum :: (Int, Int) -> [Int] -> [Int] -> IO Int
    gcdMaxSum (s,_) [] _ = return s
    gcdMaxSum (s,g) as [] = gcdMaxSum (s,g) (tail as) obs
    gcdMaxSum (s,g) as@(ah:at) (bh:bt) 
        | ah < g && bh < g = return s
        | ah < g || bh < g = gcdMaxSum (s,g) at obs
        | otherwise = gcdMaxSum r' as bt
        where
            g' = gcd ah bh
            s' = ah + bh
            r' = if (g' > g) || (g' == g && s' > s) then (s', g') else (s, g)

main :: IO ()
main = do
    _ <- getLine
    a_temp <- getLine
    let as = map read $ words a_temp :: [Int]
    b_temp <- getLine
    let bs = map read $ words b_temp :: [Int]
    res <- maxGcdSum as bs
    print res
