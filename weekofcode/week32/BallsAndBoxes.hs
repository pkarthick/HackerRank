import Control.Applicative
import Control.Monad
import System.IO
import Data.List
import Data.Ord

getMultipleLines :: Int -> IO [String]
getMultipleLines n = replicateM n getLine 

main :: IO ()
main = do
    -- n_temp <- getLine
    -- let n_t = words n_temp
    -- let nc = read $ head n_t :: Int
    -- let nb = read $ n_t!!1 :: Int
    -- a_temp <- getLine
    -- let balls = map read $ words a_temp :: [Int]
    -- c_temp <- getLine
    -- let boxcap = map read $ words c_temp :: [Int]
    -- b_temp <- getMultipleLines nc
    -- let candies = map ( map ( read :: String -> Int ) . words ) b_temp
    
    let nc = 2
    let nb = 2
    let balls = [1,1]
    let boxcap = [0,2]
    let candies = [[1,7], [3,1]]
    
    -- print nc
    -- print mb
    -- print balls
    -- print boxcap
    putStr "Box sorted by candies earned: "
    print $ sortBoxes candies

    print $ candyCount nc nb balls boxcap candies

        
sortBoxes :: [[Int]] -> [[(Int, Int)]]
sortBoxes = map (sortBy (comparing (Down . fst)) . (`zip` [0..]))

candyByColor :: [Int] -> [Int] -> [(Int, Int)] -> Int
candyByColor balls boxcap candyBoxPairs = 
    sum $ zipWith handleBall [0 ..] balls
    where
    handleBall color count = 
        if boxcap !! color >= count then count * fst (candyBoxPairs !! color)
        else count - boxcap !! color

candyCount :: Int -> Int -> [Int] -> [Int] -> [[Int]] -> Int
candyCount nc nb balls boxcap candies = 
    sum $ map (candyByColor balls boxcap) $ sortBoxes candies
    

