import Data.List (groupBy, sortBy, minimumBy)
import Prelude hiding (abs)

abs :: Int -> Int -> Int
abs (-1) _ = 0
abs x y = if x > y then x - y else y - x

calcMovement :: [Int] -> [(Int, Int, Int)]
calcMovement ks =
  if length ks <= 2
    then [(0,0,0)]
    else process (tail ks) [(head ks, -1, 0)]
  where
    process :: [Int] -> [(Int, Int, Int)] -> [(Int, Int, Int)]

    process [] xs = xs
    process (k : kt) xs = do
      process kt $ sortBy (\(_, _, m1) (_, _, m2) -> compare m1 m2) $ map (minimumBy (\(_, _, m1) (_, _, m2) -> compare m1 m2)) $ groupBy (\(l1, r1, m1) (l2, r2, m2) -> (l1, r1) == (l2, r2)) $ sortBy (\(l1, r1, m1) (l2, r2, m2) -> compare (l1, r1) (l2, r2)) $ concatMap (\(l, r, m) -> [(k, r, m + abs l k), (l, k, m + abs r k)]) xs

main :: IO ()
main = do
  n <- fmap (read :: String -> Int) getLine
  keys <- fmap (map ((\x -> if x == 0 then 10 else x) . (read :: String -> Int)) . words) getLine
  print $ (\(_,_,m) -> n + m) $ head $ calcMovement keys
