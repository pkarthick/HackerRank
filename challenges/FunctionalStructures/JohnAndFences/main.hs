import Control.Monad
import Control.Applicative
import Data.List
import Prelude hiding (max)

main :: IO ()
main = do
    _ <- getLine
    xs <- map (read :: String->Int) . words <$> getLine
    calcArea xs [] 0

    where

        calcArea :: [Int] -> [(Int, Int)] -> Int -> IO ()

        calcArea [] pairs maxArea = 
            let max' = if null pairs then 0 else maximum $ map (uncurry (*)) pairs
            in 
                print $ if maxArea > max' then maxArea else max'


        calcArea (xh:xt) pairs maxArea = 
            case partition ((>= xh). fst) pairs of
                ([], nmps) -> do
                    let max' = if null nmps then maxArea else maximum $ map (uncurry (*)) nmps
                    let pairs' = (xh, 1): map (\(f, s) -> if f <= xh then (f, s+1) else (f,s)) nmps
                    let maxArea' = if maxArea > max' then maxArea else max'
                    calcArea xt pairs' maxArea'

                (mps, []) -> do
                
                    let pairs' = [(xh, 1)]
                    let max' = if null mps then maxArea else maximum $ map (uncurry (*)) mps
                    let maxArea' = if maxArea > max' then maxArea else max'
                    calcArea xt pairs' maxArea'
    
                (mps, nmps) -> do
                
                    let pairs' = (xh, 1 + maximum (map snd mps)): map (\(f, s) -> if f <= xh then (f, s+1) else (f,s)) nmps
                    let max' = maximum $ map (uncurry (*)) mps
                    let maxArea' = if maxArea > max' then maxArea else max'
                    calcArea xt pairs' maxArea'



                    
