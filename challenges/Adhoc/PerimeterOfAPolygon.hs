import Control.Monad

main :: IO ()
main = do
    n <- readLn
    lines <- replicateM n getLine
    -- print n
    let points = map ( (\ (a:b:_) -> (a,b) ) . map (read::String->Int) . words) lines
    let pairs = zip points (tail points ++ [head points])
    print $ sum $ map (\ ((x1,y1),(x2,y2)) -> sqrt $ fromIntegral $ (x2-x1)^2 + (y2-y1)^2 ) pairs
    