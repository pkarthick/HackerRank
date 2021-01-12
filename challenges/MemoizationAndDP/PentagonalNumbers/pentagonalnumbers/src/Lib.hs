module Lib
    ( dotCountMemo
    ) where
        
import Data.Function (fix)
import Control.Monad

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

dotCount :: (Int->Int) -> Int -> Int
dotCount _ 1 = 1
dotCount f n = 
    f (n-1) + count n
    where 
        count y = (y-1) * 3 + 1

dotCountMemo :: Int -> Int
dotCountMemo = fix (memoize . dotCount)

main :: IO ()
main = do
    count <- readLn
    input <- replicateM count (fmap (read::String->Int) getLine)
    let _ = dotCountMemo $ maximum input
    mapM_ (print . dotCountMemo) input
