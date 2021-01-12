module Main (main) where

import Data.List
import Data.Ord

r = [1,0,4,2,3,6]
n = 9
p = 6
s' = 0

dist s' = r !! (s' `mod` p)

cw d = abs (s' + d) `mod` n
acw d = abs (n + s' - d) `mod` n

-- canJump :: Int -> Int -> Int -> Int -> [Int] -> Int
-- canJump n s t p r =
--     canJump' 0 [] s
--     where
--     canJump' jc acc s'
--         | t == s' = jc
--         | dist == 0 = -1
--         | jc > 0 && s == s' = -1 
--         -- | jc > 0 && not (null acc) && s' `elem` tail acc = -1
--         | cw dist >= t = jc + 1
--         | acw dist <= t = jc + 1
--         | otherwise = 
--             fromMaybe (-1) $ find (\ next -> (>=0) $ canJump' (jc+1) (next:acc) next ) $ map (\ (ac,dist) -> if ac then negate dist else dist ) sortedDist


main :: IO ()
main = do

-- dcw = maximumBy (comparing dist) $ map cw [1..dist s']
-- dacw = maximumBy (comparing dist) $ map acw [1..dist s']

    let dis = dist s'

    let dcw = filter ((/=0) .snd) $ map (\x -> (cw x, r !! (cw x `mod` p))) [1..dis]
    let dacw = filter ((/=0) .snd) $ map (\x -> (acw x, r !! (acw x `mod` p))) [1..dis]
    
    let sortedDist = sortBy (comparing (Down . snd)) (dcw ++ dacw)

--print $ dcw + dacw

--next = if r !! dcw > 0 && (r !! (dcw `mod` (p-1)) > (r !! (dcw `mod` (p-1)))) then dcw else dacw

    print $ dist s'
    print dcw
    print dacw
    print sortedDist
    
