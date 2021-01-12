import Control.Monad
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M

main :: IO ()
main = do

    pc <- (read :: String -> Int) <$> getLine
    tc <- (read::String->Int) <$> getLine
    ls <- replicateM tc getLine

    let pairs = concatMap ((\ [a,b] -> [[a,b], [b,a]] ) . map (read :: String -> Int) . words) ls

    let gs =  M.fromList $ map (\xs -> (head (head xs), S.fromList $ insert (head (head xs)) $ sort $ map (!!1) xs)) $ groupBy (\a b -> head a == head b) $ sortOn head pairs

    let groups = processGroups gs (M.keysSet gs) []

    let (gcount, gcost) =
            foldl(\(s, c) size ->  do
                    let cost = ceiling (sqrt $ fromIntegral size)
                    (s + size, c + cost)
                ) (0, 0) groups

    print (gcost + pc - gcount)

    where

        processGroups  :: M.Map Int (S.Set Int)  -> S.Set Int -> [Int] -> [Int]
        processGroups gs ks acc =
            if null ks then acc
            else
                let newSet = processGroup gs [S.elemAt 0 ks] S.empty
                in processGroups gs (ks S.\\ newSet) ((if S.null newSet then 2 else S.size newSet) : acc)

        processGroup :: M.Map Int (S.Set Int) -> [Int] -> S.Set Int -> S.Set Int
        processGroup _ [] acc = acc
        processGroup gs keys acc =
            let keysSet = foldl (\s k -> S.union (M.findWithDefault S.empty k gs) s) acc keys
                newKeys = keysSet S.\\ acc
                acc' = S.union acc newKeys
            in processGroup gs (S.toList newKeys) acc'
