import Control.Monad
import qualified Data.Map as Map
import Data.List

isFunction :: [String] -> Bool
isFunction inputs 
    | length groups == length inputs = True  
    | otherwise = 
        let filtered = Map.filter ((>1). length. nub) groups
        in null filtered 
    where
        pairs = map toPair inputs
        groups = Map.fromListWith (++) [(k, [v]) | (k, v) <- pairs]

        toPair :: String -> (Int,Int)
        toPair s = 
            let x:y:_ = map (read::String->Int) $ words s 
            in (x,y)

main :: IO ()
main = do
    t <- fmap (read::String->Int) getLine    
    mapM_ processPairs [1..t] 
    where 
        processPairs _ = do 
            n <- fmap (read::String->Int) getLine
            pairs <- replicateM n getLine
            putStrLn $ if isFunction pairs then "YES" else "NO"
