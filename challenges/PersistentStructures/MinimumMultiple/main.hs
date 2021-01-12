import Data.List
import Control.Monad
import Data.Ord

data NodeData = NodeData {lo::Int, hi::Int, val::Integer } deriving (Show)
data Tree a = Node a (Tree a) (Tree a) | Leaf a | Empty deriving (Show)

getMinimumMultiple :: Tree NodeData -> Int -> Int -> IO [Integer] 

getMinimumMultiple Empty _ _ = return []
getMinimumMultiple (Leaf NodeData{val=v}) _ _ = return [v]

getMinimumMultiple (Node NodeData{val=v} (Leaf NodeData{lo=ll, val=lv}) (Leaf NodeData{lo=rl, val=rv})) s e
    | s == ll && e == rl = 
        return [v]
    | s == ll = 
        return [lv]
    | otherwise = 
        return [rv]

getMinimumMultiple (Node _ (Leaf NodeData{val=lv}) Empty) _ _ = return [lv]

getMinimumMultiple (Node _ l@Node{} Empty) s e = getMinimumMultiple l s e

getMinimumMultiple (Node NodeData{val=v} l@(Node NodeData{lo=ll, hi=lh, val=lv} _ _) r@(Node NodeData{lo=rl, hi=rh, val=rv} _ _)) s e 
    | s == ll && e == rh = return [v]
    | s == ll && e == lh = return [lv]
    | s == rl && e == rh = return [rv]
    -- | s == ll && e > lh = 
    --     (lv:) <$> getMinimumMultiple r (lh+1) e
    | s >= ll && e <= lh = 
        getMinimumMultiple l s e
    | s >= rl && e <= rh = 
        getMinimumMultiple r s e
    | otherwise = do
        ls <- getMinimumMultiple l s lh 
        rs <- getMinimumMultiple r (lh+1) e
        return $ ls ++ rs

getMinimumMultiple _ s e = error $ "Unexpected arguments" ++ show s ++ " " ++ show e


query :: Tree NodeData -> Int -> Int -> IO (Tree NodeData)
query root a b = do
    ns <- getMinimumMultiple root a b
    print $ foldl1 lcm ns `mod` 1000000007
    return root

update :: Tree NodeData -> Int -> Integer -> IO (Tree NodeData)

update (Leaf NodeData{lo=ll, val=lv}) i n =
    return $ if ll == i then Leaf NodeData{lo=ll, hi=ll, val=lv * n} else Leaf NodeData{lo=ll, hi=ll, val=lv}

update (Node _ l@Leaf{} r@Leaf{}) i n = do
    l' <- update l i n
    r' <- update r i n
    return $ createParent [l', r'] []

update (Node _ l@Node{} Empty) i n = do
    l' <- update l i n
    return $ createParent [l'] []

update (Node _ l@Leaf{} Empty) i n = do
    l' <- update l i n
    return $ createParent [l'] []

update (Node _ l@(Node NodeData{lo=ll, hi=lh} _ _) r@Node{}) i n 
    | i >= ll && i <= lh = do
        l' <- update l i n
        let rt = createParent [l', r] []
        return rt
    | otherwise = do
        r' <- update r i n
        let rt = createParent [l, r'] []
        return rt
    
update _ _ _ = error "Unexpected error in update!"

createParent :: [Tree NodeData] -> [Tree NodeData] -> Tree NodeData

createParent [] acc = createParent (reverse acc) []

createParent [l@(Leaf nd@NodeData{})] acc = createParent (reverse $ Node nd l Empty : acc) []
createParent [root@Node{}] [] = root

createParent [n@(Node nd _ _)] acc = createParent (reverse $ Node nd n Empty : acc) []

createParent (l@(Leaf NodeData{lo=l1, hi=_, val=v1}):r@(Leaf NodeData{lo=_, hi=h2, val=v2}):xs) acc = 
    createParent xs (Node NodeData{lo=l1, hi=h2, val=lcm v1 v2} l r:acc)

createParent (l@(Node NodeData{lo=l1, hi=h1, val=v1} _ _):Empty:xs) acc = 
    createParent xs (Node NodeData{lo=l1, hi=h1, val=v1} l Empty:acc)

createParent (l@(Node NodeData{lo=l1, hi=_, val=v1} _ _):r@(Node NodeData{lo=_, hi=h2, val=v2} _ _):xs) acc = 
    createParent xs (Node NodeData{lo=l1, hi=h2, val=lcm v1 v2} l r:acc)

createParent _ _ = error "Unexpected parameters in createParent!"

createTree :: [Integer] -> Tree NodeData
createTree xs = 
    let ls = leaves xs
    in createParent ls []


leaves :: [Integer] -> [Tree NodeData]
leaves xs = 
    map (\(i, v) -> Leaf $ NodeData {lo=i, hi=i, val=v} ) $ zip [0 .. ] xs
    

main :: IO ()
main = do
    _ <- (read::String->Int) <$> getLine

    root <- createTree . map (read::String->Integer) . words <$> getLine

    k <- (read::String->Int) <$> getLine

    foldM_ (
        \ root' _ -> do
            l <- getLine
            let (q:[a,b]) = words l

            if q == "Q" 
                then query root' (read a ::Int) (read b ::Int)
                else update root' (read a ::Int) (read b ::Integer)


        ) root [1..k]
