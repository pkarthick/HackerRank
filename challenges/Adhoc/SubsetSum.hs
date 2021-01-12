module Main where
    
import Control.Monad
import Data.List
import Data.Ord
import qualified Data.Map as M

getCounts :: [Integer] -> [(Integer,Integer)] -> [(Integer,Integer)] -> M.Map Integer Integer
getCounts (total:rest) sss acc =
    let sss' = dropWhile ((<total) . fst) sss
    in if null sss' then 
        M.fromList $ map (\t -> (t, -1)) (total:rest) ++ acc
    else
        getCounts rest sss' ((total, (snd . head)sss'):acc)

main :: IO ()
main = do
    _ <- getLine
    xs <- fmap (sortBy (comparing Down) . map (read::String->Integer) . words) getLine
    c <- fmap (read::String->Int) getLine
    ys <- replicateM c $ fmap (read::String->Integer) getLine
    
    let sss = flip zip [1..] $ scanl1 (+) xs

    let counts = getCounts (sort ys) sss []
    mapM_ (print. (counts M.!)) ys