module Lib where

data NodeData = NodeData { startInd :: Int, endInd :: Int, lowVal :: Int, hiVal :: Int, firstVal :: Int, size::Int } deriving (Show)
data Node = Node NodeData Node Node | Empty deriving (Show)

getElem :: Int -> Node -> Int

getElem ind (Node NodeData{} l@(Node NodeData{} _ _) Empty) =
    getElem ind l

getElem ind (Node NodeData{} (Node NodeData{startInd=li, lowVal=lv} Empty Empty) (Node NodeData{lowVal=rv} Empty Empty)) = 
    if ind == li then lv else rv

getElem ind (Node NodeData{} l@(Node NodeData{startInd = lsi, endInd=lei} _ _) r@(Node NodeData{} _ _)) =
    getElem ind (if ind >= lsi && ind <= lei then l else r)


createLeaves :: [Int] -> Int -> [Node] -> [Node]
createLeaves [] _ nodes = reverse nodes
createLeaves (v:xs) i nodes = createLeaves xs (i+1) (Node NodeData{startInd = i, endInd=i, lowVal = v, hiVal=v, firstVal=v, size=1} Empty Empty : nodes)

addParentNodes :: [Node] -> [Node] -> [Node]
    
addParentNodes 
    (l@(Node NodeData{startInd = lsi, lowVal = llv } Empty Empty) 
    : r@(Node NodeData{startInd = rsi, lowVal = rlv} Empty Empty) 
    : ns) pns =
        let (lv', hv') = if llv <= rlv then (llv, rlv) else (rlv,llv)
        in addParentNodes ns (Node (NodeData{ startInd = lsi, endInd=rsi, lowVal = lv', hiVal = hv', firstVal=llv, size=2}) l r : pns)

addParentNodes [l@(Node nd@NodeData{} Empty Empty)] pns = 
    reverse (Node nd l Empty : pns)

addParentNodes [] pns = reverse pns

addAncestorNodes :: [Node] -> [Node] -> Node

addAncestorNodes [l@(Node NodeData{} _ _)] [] = l

addAncestorNodes [l@(Node nd@NodeData{} _ _)] pns = addAncestorNodes (reverse $ Node nd l Empty : pns) []

addAncestorNodes 
    (l@(Node NodeData{startInd = lsi, endInd=lei, lowVal = llv, hiVal=lhv, size=ls, firstVal=flv} _ _) 
    : r@(Node NodeData{startInd = rsi, endInd=rei, lowVal = rlv, hiVal=rhv, size=rs} _ _) 
    : ns) pns =
    let lv' = if llv <= rlv then llv else rlv
        hv' = if lhv <= rhv then rhv else lhv
        si' = if lsi <= rsi then lsi else rsi
        ei' = if lei <= rei then rei else lei
        ancNode = Node (NodeData{ startInd = si', endInd=ei', lowVal = lv', hiVal = hv', firstVal=flv, size=ls+rs}) l r
    in 
        if null ns 
            then if null pns then ancNode else addAncestorNodes (reverse (ancNode : pns)) []
            else addAncestorNodes ns (ancNode : pns) 

createTree :: [Int] -> Node
createTree xs =
    let leaves = createLeaves xs 0 []
    in addAncestorNodes (addParentNodes leaves []) []

isBetween :: Int -> Int -> Int -> Bool
isBetween a x y = a >= x && a <= y

hasOverlap :: NodeData -> NodeData -> Bool
hasOverlap NodeData{lowVal=llv, hiVal=lhv} NodeData{lowVal=rlv, hiVal=rhv} =
    isBetween lhv rlv rhv || isBetween rhv llv lhv || isBetween llv rlv rhv || isBetween rlv llv lhv

hasMatch :: NodeData -> Int -> Int -> Int -> Bool
hasMatch NodeData {startInd=si, endInd=ei, lowVal = lv, hiVal = hv} ind low high =
    lv >= low --0&& hv >= low -- && lv <= high && hv <= high

isNodeInRange :: NodeData -> Int -> Bool
isNodeInRange NodeData {startInd=si, endInd=ei, lowVal = lv, hiVal = hv} ind =
    isInRange ind si ei

isInRange :: Int -> Int -> Int -> Bool
isInRange v low high =
    v >= low && v <= high

findLength :: [(Bool, Int)] -> Int -> Int -> Int
findLength [] cur len = if cur >= len then cur else len
findLength ((True,l):xt) cur len = findLength xt (cur+l) len
findLength ((False,0):xt) cur len = findLength xt 0 (maximum[len, cur])
findLength ((False,l):xt) cur len = findLength xt 0 (maximum[len, cur, l])

longestSubArray :: Node -> Int -> Int -> Int -> IO Int
longestSubArray node ind low high = do
    (_, _, val) <- longestSubArray' node ind low high
    return val

longestSubArray' :: Node -> Int -> Int -> Int -> IO (Bool, Bool, Int)

longestSubArray' (Node NodeData{} (Node NodeData{startInd = si, lowVal=lv} Empty Empty) Empty) ind low high
    | lv >= low && lv <= high = return (True, True, 1)
    | otherwise = return (False, False, 0)

longestSubArray' (Node NodeData{} (Node NodeData{startInd=lsi, lowVal=lv} Empty Empty) (Node NodeData{startInd=rsi, lowVal=rv} Empty Empty)) ind low high =
    return $ case (lv >= low && lv <= high, rv >= low && rv <= high) of
        (True, True) -> (True, True, 2)
        (False, False) -> (False, False, 0)
        (True, False) -> if rsi < ind then (False, True, 0) else (True, False, 1)
        (False, True) -> if lsi > ind then (True, False, 0) else (False, True, 1)
    
longestSubArray' (
    Node nd@NodeData {lowVal = lv, hiVal = hv, startInd=si, endInd=ei, firstVal=fv, size=s} 
    l@(Node lnd@NodeData{size=ls} _ _) r@(Node rnd@NodeData{size=rs} _ _)
    ) ind low high 
    | lv >= low && hv <= high =
        return (True, True, s)
    | isIndexRight = do
        (rlcont, rrcont, rval) <- longestSubArray' r ind low high 
        if rlcont 
            then do 
                (llcont, lrcont, lval) <- longestSubArray' l ind low high
                return (llcont, rrcont, rval + lval) 
        else return (False, rrcont, rval)
    | isIndexLeft = do
        (llcont, lrcont, lval) <- longestSubArray' l ind low high
        if lval == ls && lrcont 
            then do 
                (rlcont, rrcont, rval) <- longestSubArray' r ind low high 
                return (llcont, rrcont, rval + lval) 
            else return (llcont, False, lval)
    | otherwise =                
        if isNodeInRange lnd ind then do
            (llcont, lrcont, lval) <- longestSubArray' l ind low high 
            
            if lrcont then do
                (rlcont, rrcont, rval) <- longestSubArray' r ind low high
                return (llcont, rrcont, rval + lval)
            else 
                return (llcont, False, lval)

        else do
            (rlcont, rrcont, rval) <- longestSubArray' r ind low high
            
            if rlcont then do
                (llcont, lrcont, lval) <- longestSubArray' l ind low high 
                return (llcont, rrcont, rval + lval)
            else 
                return (False, rrcont, rval)

    where
        isIndexRight = ind > ei
        isIndexLeft = ind < si
        isIndexInRange = isNodeInRange nd ind

        lm = hasMatch lnd ind low high
        rm = hasMatch rnd ind low high
    
longestSubArray' (
    Node NodeData {} l@(Node NodeData{} _ _) Empty
    ) ind low high = 
    longestSubArray' l ind low high

