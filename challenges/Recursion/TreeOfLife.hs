import Data.Bits
import Data.Map (fromList, Map, (!), lookup, singleton, insert, empty)
import Control.Monad (forM_, foldM_)

data Tree = 
    Leaf Bool | Node Tree Bool Tree deriving (Show, Eq, Ord)

data Token =
    Open | Close | Value Bool | Parent Token Token Token deriving (Show)

-- instance Show Tree where
--     show (Leaf b) = if b then "X" else "."
--     show (Node t1 b t2) = "(" ++ show t1 ++ " " ++ if b then "X" else "." ++ " " ++ show t2 ++ ")"

tokenize :: String -> [Token] -> [Token]
tokenize [] ts = reverse ts
tokenize (' ': xs) ts = tokenize xs ts
tokenize ('(': xs) ts = tokenize xs (Open:ts)
tokenize (')': xs) ts = tokenize xs (Close:ts)
tokenize ('X': xs) ts = tokenize xs (Value True:ts)
tokenize ('.': xs) ts = tokenize xs (Value False:ts)

parse :: [Token] -> [Token] -> Token

parse [p@(Parent {})] acc = p

parse [] acc = parse acc []

parse (Open:Value l:Value p: Value r:Close:ts) acc =
    parse (Parent (Value l) (Value p) (Value r): ts) acc

parse (Open: Value l: Value p: r@Parent{}: Close : ts) acc =
    parse (Parent (Value l) (Value p) r: ts) acc

parse (Open: l@Parent{}: Value p: Value r: Close : ts) acc =
    parse (Parent l (Value p) (Value r): ts) acc

parse (Open: l@Parent{}: Value p: r@Parent{}: Close : ts) acc =
    parse (Parent l (Value p) r: ts) acc

parse (t:ts) acc =
    parse ts (acc++[t])

createTree::Token->Tree

createTree (Parent (Value l) (Value p) (Value r)) =
    Node (Leaf l) p (Leaf r)

createTree (Parent l@Parent{} (Value p) r@Parent{}) =
    Node (createTree l) p (createTree r)

createTree (Parent l@Parent{} (Value p) (Value r)) =
    Node (createTree l) p (Leaf r)

createTree (Parent (Value l) (Value p) r@Parent{}) =
    Node (Leaf l) p (createTree r)

createTree (Parent t1 t2 t3) =
    Node (Leaf True) False (Leaf True)

createTree t =
    error $ "Unexpected token: " ++ show t

getValue :: Tree -> Bool
getValue (Leaf x) = x
getValue (Node _ x _) = x

nextState ::  Map [Bool] Bool -> (Tree -> Tree)
nextState rulesMap tree = nextState' False tree rulesMap

nextState' ::  Bool -> Tree -> Map [Bool] Bool -> Tree

nextState' False child@(Node l c r) rulesMap = 
    let c' = rulesMap ! getRuleKey False child
    in Node (nextState' c l rulesMap) c' (nextState' c r rulesMap)

nextState' p (Leaf c) rulesMap =
    Leaf $ rulesMap ! [p, False, c, False]

nextState' p child@(Node l c r) rulesMap = 
    let c' = rulesMap ! getRuleKey p child
    in Node (nextState' c l rulesMap) c' (nextState' c r rulesMap)

ruleBook :: Int -> [([Bool], Bool)]
ruleBook rule = 
    map (\x -> (ruleKey x, (rule .&. bit x) /= 0)) [0..15]
    where
        ruleKey :: Int -> [Bool]
        ruleKey key = reverse $ map (\x -> (key .&. bit x) /= 0) [0..3]

getRuleKey :: Bool -> Tree -> [Bool]
getRuleKey parent (Leaf l) = [parent, False, l, False]
getRuleKey parent (Node l c r) = [parent, getValue l, c, getValue r]

getSubTree :: Tree -> Char -> Tree
getSubTree (Node l _ _) '<' = l
getSubTree (Node _ _ r) '>' = r

traverseTree :: Tree -> String -> Bool
traverseTree (Node _ p _) ['[',']'] = p
traverseTree root ('[':ds) = getValue $ foldl getSubTree root $ init ds


getState :: Map [Bool] Bool -> Map Int Tree -> Map (Int, String) Bool -> Int -> (Map Int Tree, Map (Int, String) Bool)

getState rulesMap stateMap traverseMap n =
    case Data.Map.lookup n stateMap of
        Just _ -> (stateMap, traverseMap)
        Nothing -> 
            if n == 0 
                then (stateMap, traverseMap)
                else 
                    let 
                        (stateMap', traverseMap') = getState rulesMap stateMap traverseMap (n-1)
                        state = nextState rulesMap (stateMap' ! (n-1))
                        traverseMap'' = updateMap n state "" traverseMap'
                    in 
                        (insert n state stateMap', traverseMap'')


updateMap :: Int -> Tree -> String -> Map (Int, String) Bool ->  Map (Int, String) Bool

updateMap tree cur@(Node l@Node{} c r@Node{}) acc vmap = 
    let vmap1 = insert (tree, acc) c vmap
        vmap2 = updateMap tree l (acc ++ "<") vmap1
        vmap3 = updateMap tree r (acc ++ ">") vmap2
    in vmap3

updateMap tree cur@(Node (Leaf l) c r@Node{}) acc vmap = 
    let vmap1 = insert (tree, acc) c vmap
        vmap2 = insert (tree, acc ++ "<") l vmap1
        vmap3 = updateMap tree r (acc ++ ">") vmap2
    in vmap3

updateMap tree cur@(Node l@Node{} c (Leaf r)) acc vmap = 
    let vmap1 = insert (tree, acc) c vmap
        vmap2 = insert (tree, acc ++ ">") r vmap1
        vmap3 = updateMap tree l (acc ++ "<") vmap2
    in vmap3

updateMap tree cur@(Node (Leaf l) c (Leaf r)) acc vmap = 
    let vmap1 = insert (tree, acc) c vmap
        vmap2 = insert (tree, acc ++ ">") r vmap1
        vmap3 = insert (tree, acc ++ "<") l vmap2
    in vmap3

updateMap tree cur@(Leaf l) acc vmap = insert (tree, acc) l vmap

main :: IO()
main = do
    num <- fmap (read::String->Int) getLine
    let rulesMap = fromList $ ruleBook num

    exp <- getLine
    qc <- fmap (read::String->Int) getLine

    let tree = createTree $ parse (tokenize exp []) []

    let stateMap = take 1001 $ iterate (nextState rulesMap) tree

    -- let traverseMap = foldl (\tmap n -> updateMap n (stateMap!!n) "" tmap) Data.Map.empty [0..1000]

    -- mapM_ print allStates

    let traverseMap = updateMap 0 tree "" Data.Map.empty
    -- let stateMap = singleton (0::Int) root

    foldM_ (\(cur, traverseMap) _ -> do
                (ns : dirs : _) <- fmap words getLine
                let n = (read::String->Int) ns
                let cur' = cur + n
                -- let (stateMap', traverseMap') = getState rulesMap stateMap traverseMap cur'
                let tree = stateMap !! cur'
                let traverseMap' = updateMap cur' tree "" traverseMap
                let ds = init $ tail dirs
                putChar $ if traverseMap'! (cur', ds) then 'X' else '.'
                putStrLn ""
                return (cur', traverseMap')
                ) (0, traverseMap) [1..qc]
