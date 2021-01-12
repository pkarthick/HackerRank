import Data.List
import Data.Maybe
import Data.Ord

canJump :: Int -> Int -> Int -> Int -> [Int] -> Int
canJump n s t p r =
    
    if r!!s == 0 then -1 
    else canJump' 0 [] s (r!!s)
    where
    canJump' jc acc s' dist
        | t == s' = jc
        | jc > 0 && s == s' = -1 
        -- | jc > 0 && not (null acc) && s' `elem` tail acc = -1
        | cw dist >= t = jc + 1
        | acw dist <= t = jc + 1
        | otherwise = 

            fromMaybe (-1) $ find (>=0) $ map (\ (next, dis) -> canJump' (jc+1) (next:acc) next dis) sortedDist
            
            -- if dcw > 0 && dcw > dacw then s' + dcw else s' - dacw
            
            -- if next == 0 then -1 else canJump' (jc+1) (next:acc) next

            where

                --dist = r !! (s' `mod` p)
                cw d = abs (s' + d) `mod` n
                acw d = abs (n + s' - d) `mod` n
                dcw = filter ((/=0) .snd) $ map (\x -> (cw x, r !! (cw x `mod` p))) [1..dist]
                dacw = filter ((/=0) .snd) $ map (\x -> (acw x, r !! (acw x `mod` p))) [1..dist]

                sortedDist = sortBy (comparing (Down . snd)) (dcw ++ dacw)

                -- next = if dcw > 0 && dcw > dacw then s' + dcw else s' - dacw

main :: IO ()
main = do
    n_temp <- getLine
    let n_t = words n_temp
    let n = read $ head n_t :: Int
    let s = read $ n_t!!1 :: Int
    let t = read $ n_t!!2 :: Int
    r_0_temp <- getLine
    let r_0_t = words r_0_temp
    let r_0 = read $ head r_0_t :: Int
    let g = read $ r_0_t!!1 :: Int
    let seed = read $ r_0_t!!2 :: Int
    let p = read $ r_0_t!!3 :: Int

    let r = r_0 : takeWhile (/=r_0) [howfar (g, seed, p, r_0) n' | n' <- [1..]]
    -- let r = [1,0,4,2,3,6]
    

    let jc = canJump n s t (length r) r 
    print $ if jc >= 0 then jc else -1    

    where
        howfar :: (Int, Int, Int, Int) -> Int -> Int
        howfar (_, _, _, r_0) 0 = r_0
        howfar (g, seed, p, r_0) n = (howfar (g, seed, p, r_0) (n-1) * g + seed) `mod` p
