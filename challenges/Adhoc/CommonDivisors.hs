module Main where

import Control.Monad
import Data.List 
import qualified Data.Map as M
import qualified Data.Set as S

add :: S.Set Int -> [Int] -> S.Set Int
add = foldl (\ ods n -> S.union ods $ S.map (* n) ods) 

getDivisors :: M.Map Int (S.Set Int) -> [Int] -> Int -> Int -> (S.Set Int, M.Map Int (S.Set Int))
getDivisors dmap ds on n =
    case M.lookup n dmap of
        Just ods -> 
            let nds = add ods ds
            in (nds, M.insert on nds dmap)
        Nothing -> 
            case find ((==0) . (n `mod`)) [2 .. p] of
                Just ff -> getDivisors dmap (ff:ds) on (n `div` ff)
                Nothing -> 
                    if null ds then (S.fromList [1,n], dmap) -- prime number
                    else do
                        let nds = S.insert 1 $ S.insert on $ add (S.singleton 1) $ n:ds
                        (nds, M.insert on nds dmap)
    where
        p = (floor . sqrt . fromIntegral) n


getCommonDivisors :: (M.Map Int (S.Set Int),Int) -> [Int] -> (M.Map Int (S.Set Int), Int)

getCommonDivisors (dmap,_) (m:l:_) =
    if m == 1 || l == 1 then (dmap, 1)
    else do
        let (mds,dmap') = getDivisors dmap [] m m

        if m == l then
            (dmap', length mds)
        else do
            let (lds, dmap'') = getDivisors dmap' [] l l
            (dmap'', S.size $ S.intersection mds lds)

main :: IO ()
main = do
    n <- fmap (read::String->Int) getLine
    mls <- replicateM n $ fmap (map (read::String->Int) . words) getLine
    let dmap = M.singleton 1 $ S.singleton 1
    mapM_ (print . snd) $ tail $ scanl getCommonDivisors (dmap,0) mls
