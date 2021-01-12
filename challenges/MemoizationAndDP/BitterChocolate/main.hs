import Control.Monad
import qualified Data.Map.Strict as M

bite :: (Int, Int, Int) -> Int -> Int -> (Int, Int, Int) 
bite (r1,r2,r3) 1 b
    | r2 < b = (b - 1, r2, r3)
    | r3 < b = (b - 1, b - 1, r3)
    | otherwise = (b - 1, b - 1, b - 1)

bite (r1,r2,r3) 2 0 = (r1,0,0)

bite (r1,r2,r3) 2 b
    | r3 < b = (r1, b - 1, r3)
    | otherwise = (r1, b - 1, b - 1)

bite (r1,r2,r3) 3 0 = (r1, r2, 0)

bite (r1,r2,r3) 3 b = (r1, r2, b-1)

possibleSizesAfterBite :: (Int, Int, Int) -> [(Int, Int, Int)]
possibleSizesAfterBite (r1,r2,r3) =
    [bite (r1,r2,r3) 3 b | b <- [1 .. r3]]
    ++ [bite (r1,r2,r3) 2 b | b <- [1 .. r2]]
    ++ [bite (r1,r2,r3) 1 b | b <- [2 .. r1]]

isBitter :: M.Map (Int, Int, Int) Bool -> (Int, Int, Int) -> M.Map (Int, Int, Int) Bool

isBitter m (r1',r2', r3') =
    let bs = possibleSizesAfterBite (r1',r2', r3')
    in case M.lookup (r1',r2', r3') m of
            Just _ -> m
            Nothing ->
                let m' = foldl isBitter m bs
                    res = elem False $ map (m' M.!) bs
                in M.insert (r1',r2',r3') res m'


toTriple :: [Int] -> (Int, Int, Int)
toTriple (r1:r2:r3:_) = (r1,r2,r3)
toTriple _ = error "toTriple: Invalid List Parameter!"

readIntList :: IO [Int]
readIntList = map (read::String->Int) . words <$> getLine

readTriple :: IO (Int, Int, Int)
readTriple = toTriple <$> readIntList

printResult :: Bool -> IO ()
printResult True = putStrLn "WIN"
printResult False = putStrLn "LOSE"

main :: IO ()
main = do
    let m = M.fromList [((0::Int,0::Int,0::Int), False), ((1,0,0), False), ((1,1,0), True), ((1,1,1), True)]
    let m'= foldl isBitter m [(i,j,k) | i <- [0..25], j <- [0..25], k <- [0..25], i >= j, j >= k]
    let processTriple = readTriple >>= printResult . (m' M.!)
    readLn >>= flip replicateM_ processTriple
        