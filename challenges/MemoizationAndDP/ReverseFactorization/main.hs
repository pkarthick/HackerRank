import Data.Ord
import Data.List
import qualified Data.Map.Strict as M

lexCompare :: [Int] -> [Int] -> Ordering
lexCompare xs ys 
    | length xs == length ys =
        let zs = dropWhile (uncurry (==)) $ zip xs ys
        in  
            if null zs then EQ 
            else 
                let (x, y) = head zs
                in if x == y then EQ else if x > y then GT else LT
    | length xs > length ys = GT
    | otherwise = LT


getFactors :: M.Map Int [Int] -> [Int] -> [Int] -> Int -> M.Map Int [Int]
getFactors m _ _ 1 = m
getFactors m fs xs n = 
    case M.lookup n m of
        Just _ -> m
        _ ->
            let ns = sort $ map (n `div`) $ filter (\f -> n `mod` f == 0) fs
            in 
                if null ns 
                    then M.insert n [] m
                    else 
                        if ns == [1] 
                            then M.insert n [n] m
                            else
                                let m' = foldl (\m'' x -> getFactors m'' fs [] x) m ns 
                                    rfs = minimumBy lexCompare $ map (m' M.!) ns 
                                in M.insert n (n:rfs) m'

main :: IO ()
main = do
    [n, _] <- map (read::String->Int) . words <$> getLine
    fs <- sortOn Down . map (read :: String -> Int) . words <$> getLine
    let m = getFactors (M.singleton 1 [1]) fs [] n
    let ds = reverse $ m M.! n
    if null ds || notElem (head ds) fs then putStrLn "-1" else putStrLn $ unwords $ map show (1:ds)


