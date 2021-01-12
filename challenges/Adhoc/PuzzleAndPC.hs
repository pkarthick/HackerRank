module Main where

import Data.List (zip, sort, sortBy, nub)
import Data.Map (fromList, (!))
import Data.Ord (compare)

data Tromino = Tromino Quadrant (Int,Int) -- deriving (Show)

instance Show Tromino where
    show (Tromino q (r,c)) =
        unwords $ map show $
            case q of
                First -> [r, c+1, r+1, c, r+1, c+1]
                Second -> [r, c-1, r+1, c-1, r+1, c]
                Third -> [r-1, c, r-1, c+1, r, c+1]
                Fourth -> [r-1, c-1, r-1, c, r, c-1]
                
data Quadrant = First | Second | Third | Fourth  deriving (Show)

data BlockType = Horizontal | Vertical deriving (Show)

data Block = Single Tromino | Multiple [Tromino] | Block { loc::(Int,Int), bltype::BlockType, width::Int, height::Int } 

instance Show Block where
    show (Single tr) = show tr
    show (Multiple trs) = unlines $ map show trs
    show Block{} = ""

data Board = Board {side::Int, blocks::[[Block]]}

locations :: Tromino -> [(Int, Int)]
locations (Tromino q (r,c)) =
    case q of
        First -> [(r, c+1), (r+1, c), (r+1, c+1)]
        Second -> [(r, c-1), (r+1, c-1), (r+1, c)]
        Third -> [(r-1, c), (r-1, c+1), (r, c+1)]
        Fourth -> [(r-1, c-1), (r-1, c), (r, c-1)]


findQuadrant :: (Int, Int) -> (Int, Int) -> Int -> Quadrant

findQuadrant (r,c) (r',c') 2 =
    case (rx `mod` 2, cx `mod` 2) of
        (1,1) -> Fourth
        (0,1) -> Second
        (1,0) -> Third
        (0,0) -> First
        where
            rx = r - r'
            cx = c - c'

findQuadrant (r,c) (r',c') size =
    case (r < r' + hs, c < c' + hs) of
        (True, True) -> First
        (True, False) -> Second
        (False, True) -> Third
        (False, False) -> Fourth 
    where
        hs = size `div` 2
        

quarter :: Int -> Int        
quarter size = size `div` 4

origin :: Quadrant -> Int -> (Int, Int)
origin quadrant size =
    case quadrant of
        First -> (1,1) 
        Second -> (1, hs+1)
        Third -> (hs+1, 1)
        Fourth -> (hs+1, hs+1)
        where
            hs = size `div` 2

inCorner :: (Int, Int) -> (Int, Int) -> Int -> Bool
inCorner (r,c) (r',c') size =
    (r < r'+qs && c < c'+qs) || (r >= r'+qs3 && c < c'+qs) || (r < r'+qs && c >= c'+qs3) || (r >= r'+qs3 && c >= c'+qs3) 
    where
        qs = quarter size 
        qs3 = qs * 3

inEdge :: (Int, Int) -> (Int, Int) -> Int -> Bool
inEdge (r,c) (r',c') size =
    r < r'+qs || c < c'+qs || r >= r'+qs3 || c >= c'+qs3
    where
        qs = quarter size 
        qs3 = qs * 3


create :: (Int, Int) -> Int -> Int -> BlockType -> [Tromino]

create (r,c) w h Horizontal =
    concat [ [Tromino Fourth (r'+1,c'+1), Tromino First (r', c'+1)] | c' <- [c, c+3 .. c+w-1], r' <- [r, r+2 .. r+h-1] ]

create (r,c) w h Vertical =
    concat [ [Tromino Fourth (r'+1,c'+1), Tromino First (r'+1, c')] | c' <- [c, c+2 .. c+w-1], r' <- [r, r+3 .. r+h-1] ]

split :: Int -> Int -> (Int, Int)
split _ 8 = (3,3)
split n size =
    if r == n then (l-3, r+3) else (l,r)
    where
        qs = quarter size
        qs3 = qs * 3
        quo3 = n `div` 3
        l = if n `mod` 3 == 0 then (quo3-1)*3 else quo3*3
        r = qs3 - l

mirrorC :: Int -> Tromino -> Tromino
mirrorC size (Tromino q (r,c)) =
    case q of
        First -> Tromino Second (r,c') 
        Second -> Tromino First (r,c') 
        Third -> Tromino Fourth (r,c') 
        Fourth -> Tromino Third (r,c') 
        where
            c' = size + 1 - c

mirrorR :: Int -> Tromino -> Tromino
mirrorR size (Tromino q (r,c)) =
    case q of
        First -> Tromino Third (r',c) 
        Second -> Tromino Fourth (r',c) 
        Third -> Tromino First (r',c) 
        Fourth -> Tromino Second (r',c) 
        where
            r' = size + 1 - r

rotateCW :: Int -> Tromino -> Tromino
rotateCW size (Tromino q (r,c)) =
    case q of
        First -> Tromino Second (r',c') 
        Second -> Tromino Fourth (r',c') 
        Third -> Tromino First (r',c') 
        Fourth -> Tromino Third (r',c') 
        where
            c' = size + 1 - r
            r' = c

rotateACW :: Int -> Tromino -> Tromino
rotateACW size (Tromino q (r,c)) =
    case q of
        First -> Tromino Third (r',c') 
        Second -> Tromino First (r',c') 
        Third -> Tromino Fourth (r',c') 
        Fourth -> Tromino Second (r',c') 
        where
            r' = size + 1 - c
            c' =  r
            
add :: (Int, Int) -> Tromino -> Tromino
add (r',c') (Tromino q (r,c)) = Tromino q (r+r'-1, c+c'-1)

fillBlocks :: Int -> (Int,Int) -> (Int,Int) -> [Tromino]
fillBlocks 2 (r,c) (r',c') = 
    [t]
    where
        q' = findQuadrant (r,c) (r',c') 2
        t = Tromino q' (r,c)

fillBlocks 4 (r,c) (r',c') = 
    case q of
        First -> [t, Tromino Third (r'+1, c'+2), Tromino First (r'+1, c'+1), Tromino Second (r'+2, c'+1), Tromino First (r'+2, c'+2)]
        Second -> [Tromino Fourth (r'+1, c'+1), t, Tromino Second (r'+1, c'+2), Tromino Second (r'+2, c'+1), Tromino First (r'+2, c'+2)]
        Third -> [Tromino Fourth (r'+1, c'+1), Tromino Third (r'+1, c'+2), Tromino Third (r'+2, c'+1), t, Tromino First (r'+2, c'+2)]
        Fourth -> [Tromino Fourth (r'+1, c'+1), Tromino Third (r'+1, c'+2), Tromino Fourth (r'+2, c'+2), Tromino Second (r'+2, c'+1), t]
    where
        q = findQuadrant (r,c) (r',c') 4
        q' = findQuadrant (r,c) (r',c') 2
        t = Tromino q' (r,c)
            
fillBlocks 8 (r,c) (r',c') 
    | inEdge (r,c) (r',c') 8 && rx == 2 && (cx==0 || cx==1) = t : map (add (r',c')) ts
    | inEdge (r,c) (r',c') 8 && rx == 2 && (cx==6 || cx==7) = t : map (add (r',c') . mirrorC 8) ts
    | inEdge (r,c) (r',c') 8 && rx == 5 && (cx==0 || cx==1) = t : map (add (r',c') . mirrorR 8) ts
    | inEdge (r,c) (r',c') 8 && rx == 5 && (cx==6 || cx==7) = t : map (add (r',c') . mirrorC 8 . mirrorR 8) ts
    | inEdge (r,c) (r',c') 8 && cx == 2 && (rx==0 || rx==1) = t : map (add (r',c') . mirrorC 8 . rotateCW 8) ts
    | inEdge (r,c) (r',c') 8 && cx == 5 && (rx==6 || rx==7) = t : map (add (r',c') . mirrorC 8 . rotateACW 8) ts
    | inEdge (r,c) (r',c') 8 && cx == 5 && (rx==0 || rx==1) = t : map (add (r',c') . rotateCW 8) ts
    | inEdge (r,c) (r',c') 8 && cx == 2 && (rx==6 || rx==7) = t : map (add (r',c') . rotateACW 8) ts
    where
        rx = r - r'
        cx = c - c'
        q' = findQuadrant (r,c) (r',c') 2
        t = Tromino q' (r,c)
        ts = create (1,1) 6 2 Horizontal ++ create (1,7) 2 6 Vertical ++  create (7,3) 6 2 Horizontal ++ [Tromino Fourth (4, 4), Tromino Third (4, 5), Tromino Third (5, 4), Tromino Third (6, 3), Tromino First (5,5), Tromino Fourth (6, 2), Tromino Fourth (7, 3), Tromino Second (7, 2)]
        -- ts1 = create (r',c') 6 2 Horizontal ++ create (r',c'+6) 2 6 Vertical ++  create (r'+6,c'+2) 6 2 Horizontal ++ [Tromino Fourth (r'+3, c'+3), Tromino Third (r'+3, c'+4), Tromino Third (r'+4, c'+3), Tromino Third (r'+5, c'+2), Tromino First (r'+4, c'+4), Tromino Fourth (r'+5, c'+1), Tromino Fourth (r'+6, c'+2), Tromino Second (r'+6, c'+1)]

fillBlocks size (r,c) (r',c')
    | inCorner (r,c) (r',c') size =
        case q of
            First -> fillBlocks qs (r,c) (r',c') ++ create (r',c'+qs) qs3 qs Horizontal ++ create (r'+qs, c') qs qs3 Vertical ++ create (r'+qs, c'+qs) qs3 qs3 Horizontal
            Second -> create (r',c') qs3 qs Horizontal ++ fillBlocks qs (r,c) (r',c'+qs3) ++ create (r'+qs, c') qs3 qs3 Horizontal ++ create (r'+qs, c'+qs3) qs qs3 Vertical
            Third -> create (r',c') qs qs3 Vertical ++ create (r',c'+qs) qs3 qs3 Horizontal ++  fillBlocks qs (r,c) (r'+qs3,c') ++ create (r'+qs3,c'+qs) qs3 qs Horizontal
            Fourth -> create (r',c') qs3 qs3 Horizontal ++ create (r',c'+qs3) qs qs3 Vertical ++ create (r'+qs3, c') qs3 qs Horizontal ++ fillBlocks qs (r,c) (r'+qs3,c'+qs3)
    | inEdge (r,c) (r',c') size =
        if r < r'+qs then
            create (r',c') lc qs Horizontal ++ fillBlocks qs (r,c) (r', c'+lc) ++ create (r',c'+lc+qs) rc qs Horizontal ++ create (r'+qs, c') size qs3 Vertical
        else if c < c'+qs then
            create (r',c') qs tr Vertical ++ fillBlocks qs (r,c) (r'+tr, c') ++ create (r'+tr+qs,c') qs br Vertical ++ create (r', c'+qs) qs3 size Horizontal
        else if r >= r'+qs3 then
            create (r'+qs3,c') lc qs Horizontal ++ fillBlocks qs (r,c) (r'+qs3, c'+lc) ++ create (r'+qs3,c'+lc+qs) rc qs Horizontal ++ create (r', c') size qs3 Vertical
        else -- if c >= c' + qs3
            create (r',c'+qs3) qs tr Vertical ++ fillBlocks qs (r,c) (r'+tr, c'+qs3) ++ create (r'+tr+qs,c'+qs3) qs br Vertical ++ create (r', c') qs3 size Horizontal
    | otherwise =
        create (r',c') qs3 qs Horizontal ++ create (r',c'+qs3) qs qs3 Vertical ++ create (r'+qs,c') qs qs3 Vertical ++ create (r'+qs3,c'+qs) qs3 qs Horizontal
        ++ fillBlocks hs (r,c) (cr,cc)
    where
        q = findQuadrant (r,c) (r',c') size
        hs = size `div` 2
        qs = quarter size
        qs3 = qs * 3
        (lc,rc) = split (c-c') size
        (tr,br) = split (r-r') size
        (cr, cc) = (r'+qs, c'+qs) 


-- TestCases
    -- 6
    -- 30 22
    -- --------
    -- 7
    -- 123 62
    -- --------
    -- 4
    -- 6 15
    -- ---------
        
main :: IO ()
main = do
    m <- fmap (read::String->Int) getLine
    let size = 2 ^ m
    [r,c] <- fmap (map (read::String->Int) . words) getLine

    let x = sort $ nub $ map fst $ concatMap (\(f, xs) -> map (\x -> (x, f)) xs ) $ zip [1..] $ map locations $ fillBlocks size (r,c) (1,1)
    -- sortBy (\ (f1,_) (f2,_) -> compare f1 f2 ) $ 
    -- -- let arr = [  x ! (r',c') | c' <- [1..size], r' <- [1..size] ]

    mapM_ print x
    print $ length x

    -- mapM_ print x

    -- mapM_ print $ fillBlocks size (r,c) (1,1)
