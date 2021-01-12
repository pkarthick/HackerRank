import Control.Monad
import Data.Maybe

data NodeData = NodeData {startInd::Int, endInd::Int, minVal::Int} deriving Show
data Node = Node NodeData Node Node | Empty deriving Show

createLeaves :: [Int] -> Int -> [Node] -> [Node]
createLeaves [] _ nodes = reverse nodes
createLeaves (v:xs) i nodes = createLeaves xs (i+1) (Node NodeData{startInd = i, endInd=i, minVal = v} Empty Empty : nodes)

createTree :: [Int] -> Node
createTree xs = 
    head $ addParentNodes (createLeaves xs 0 []) []

addParentNodes :: [Node] -> [Node] -> [Node]
    
addParentNodes 
    (l@(Node NodeData{startInd = lsi, minVal = sv } Empty Empty) 
    : r@(Node NodeData{startInd = rsi, minVal = ev} Empty Empty) 
    : ns) pns =
        let minV = if sv <= ev then sv else ev
        in addParentNodes ns (Node (NodeData{ startInd = lsi, endInd=rsi, minVal = minV}) l r : pns)

addParentNodes [l@(Node nd@NodeData{} Empty Empty)] pns 
        | null pns = [l]
        | otherwise = addParentNodes (reverse (Node nd l Empty : pns)) []

addParentNodes [l@(Node nd@NodeData{} _ _)] pns 
        | null pns = [l]
        | otherwise = addParentNodes (reverse (Node nd l Empty : pns)) []

addParentNodes [] pns = addParentNodes (reverse pns) []

addParentNodes 
    (l@(Node NodeData{startInd = lsi, endInd=lei, minVal = lv} _ _) 
    : r@(Node NodeData{startInd = rsi, endInd=rei, minVal = rv} _ _) 
    : ns) pns =
    let minV = if lv <= rv then lv else rv
        ancNode = Node (NodeData{ startInd = lsi, endInd=rei, minVal = minV}) l r
    in 
        if null ns 
            then if null pns then [ancNode] else addParentNodes (reverse (ancNode : pns)) []
            else addParentNodes ns (ancNode : pns) 

addParentNodes _ _ = []

isInRange :: Int -> NodeData -> Bool
isInRange ind NodeData{startInd = si, endInd=ei} =
    ind >= si && ind <= ei

findMinVal :: Node -> Int -> Int -> Int -> IO Int
findMinVal Empty _ _ mv = return mv

findMinVal (Node NodeData{startInd = si, minVal = v} Empty Empty) l h mv
    | si >= l && si <= h = 
        return $ if mv >= v then v else mv
    | otherwise = return mv

findMinVal (Node NodeData{} left@(Node NodeData{} _ _) Empty) l h mv = 
    findMinVal left l h mv

findMinVal (Node NodeData{minVal=pv} 
    left@(Node lnd@NodeData{startInd= lsi, endInd=lei, minVal=lv} _ _) 
    right@(Node rnd@NodeData{startInd= rsi, endInd=rei, minVal=rv} _ _)) l h mv
    | l == lsi && h == rei = 
        return $ if mv >= pv then pv else mv
    | l == lsi && h == lei = 
        return $ if mv >= lv then lv else mv
    | l == rsi && h == rei = 
        return $ if mv >= rv then rv else mv
    | l == lsi && h > lei = do
        let mv' = if mv >= lv then lv else mv
        rmv <- findMinVal right rsi h mv
        return $ if mv' > rmv then rmv else mv'
    | isInRange l lnd && isInRange h rnd = do
        lmv <- findMinVal left l lei mv
        rmv <- findMinVal right rsi h mv
        return $ if lmv <= rmv then lmv else rmv
    | isInRange l lnd && isInRange h lnd =
        findMinVal left l h mv
    | isInRange l rnd && isInRange h rnd =
        findMinVal right l h mv
    | otherwise = 
        if isInRange l lnd then findMinVal left l h mv
        else findMinVal right l h mv

findMinVal node _ _ _ = do
    print "Unexpected node kind: "
    print node
    return 0

main :: IO ()
main = do
    [_, qc] <- map (read::String -> Int) . words <$> getLine
    xs <- map (read::String -> Int) . words <$> getLine

    let rootNode = createTree xs

    replicateM_ qc $ do
        [l, h] <- map (read::String -> Int) . words <$> getLine
        mv <- findMinVal rootNode l h 100000
        print mv
