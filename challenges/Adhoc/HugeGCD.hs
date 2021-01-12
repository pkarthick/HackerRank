module Main where

import Data.List
import qualified Data.Set as S
import Control.Arrow

remove :: ((Integer,Integer) -> Bool) -> S.Set (Integer,Integer) -> Bool -> Maybe (Integer, S.Set (Integer,Integer))
remove prd s removeAll = do
    let fs = S.filter prd s
    if S.null fs then Nothing
    else do
        let (n, c) = S.elemAt 0 fs
        let s' = S.delete (n,c) s
        Just $ if removeAll || c == 1 then (n, s') else (n, S.insert (n,c-1) s')

update :: Integer -> S.Set (Integer,Integer) -> S.Set (Integer,Integer)
update n s = do
    let fs = S.filter ((==n) . fst) s
    if S.null fs then S.insert (n,1) s
    else do
        let (_, c) = S.elemAt 0 fs
        S.insert (n,c+1) $ S.delete (n,c) s
        
factors :: Integer -> Integer -> [Integer] -> [Integer]

factors on 0 fs = fs
factors on n fs =
    case find (\x -> n `mod` x==0) [2..p] of
        Just f -> 
            if f == n then factors on 0 fs else factors on (n `div` f) (f:fs)
        Nothing -> 
            if n < on then factors on 0 (n:fs) else factors on 0 fs
    where
        p = (round::Double->Integer) $ sqrt $ fromIntegral n

hugeGcd :: S.Set (Integer,Integer) -> S.Set (Integer,Integer) -> S.Set (Integer,Integer) -> IO Integer
hugeGcd xs ys ds 
    | S.null xs || S.null ys = do
        print ds
        return $ if null ds then 1 else S.foldl (\ z (n,c) -> ((n ^ c `mod` 1000000007) * z) `mod` 1000000007) 1 ds
    | otherwise = do
        let (x, _) = S.elemAt 0 xs
        let Just (_,xs') = remove ((==x) . fst) xs False

        case remove ((== 0) . (`mod` x) . fst) ys False of
            Nothing -> do
                let fs = factors x x []
                hugeGcd (if null fs then xs' else foldl (flip update) xs' fs) ys ds
            Just (ymx, ys') -> do
                print ds
                hugeGcd xs' (if ymx == x then ys' else update (ymx `div` x)  ys') (update x ds)

main :: IO ()
main = do
    xc <- getLine
    xs <- fmap (S.fromList . map (head &&& (fromIntegral . length)). group . sort . filter (/=1) . map (read::String->Integer) . words) getLine
    yc <- getLine
    ys <- fmap (S.fromList . map (head &&& (fromIntegral . length)). group . sort . filter (/=1) . map (read::String->Integer) . words) getLine

    -- print xs
    -- print ys
    
    z <- if xc <= yc then hugeGcd xs ys S.empty else hugeGcd ys xs S.empty
    print z

