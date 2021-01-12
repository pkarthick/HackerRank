module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Function

data Block = Horizontal Int (Int,Int) | Vertical Int (Int,Int) deriving (Show, Eq, Ord)

getBlockWords :: [Block] -> [String] -> [(Block,String)]-> [(Block,String)]
getBlockWords [] _ pairs = pairs
getBlockWords (block:unusedBlocks) unusedWords' pairs =
    getBlockWords unusedBlocks unusedWords'' pairs'
    where
        len = getLength block
        word = head $ filter ((==len) . length) unusedWords'
        pair = (block,word)
        unusedWords'' = filter (/=word) unusedWords'
        pairs' = pair : pairs



getLength :: Block -> Int
getLength block =
    case block of
        Horizontal blen _ -> blen 
        Vertical blen _ -> blen

count :: Int
count = 10

overlaps :: [String] -> [(Int, Int)]
overlaps horizontal' = 
    concat [[ (r,c) | c <- [0..9], h r c, doesOverlap r c ] | r <- [0..9] ]
    where

    inBounds :: Int -> Bool
    inBounds x = x >= 0 && x < count

    h :: Int -> Int -> Bool
    h r c = horizontal' !! r !! c == '-'

    isOverlapV :: Int -> Int-> Bool 
    isOverlapV r c = (inBounds (r-1) && h (r-1) c) || (inBounds (r+1) && h (r+1) c)

    isOverlapH :: Int -> Int-> Bool 
    isOverlapH r c = (inBounds (c-1) && h r (c-1)) || (inBounds (c+1) && h r (c+1))

    doesOverlap :: Int -> Int-> Bool 
    doesOverlap r c = isOverlapH r c && isOverlapV r c

blocks :: String -> [(Int, Int)]
blocks s = blocks' s 0 [] 
    where
    blocks' :: String -> Int -> [(Int, Int)] -> [(Int, Int)]
    blocks' [] _ acc = reverse acc
    blocks' s' oldlen acc = 
        blocks' dropped' oldlen' acc'
        where
            (dropped, source) = span ('+' ==) s'
            (taken, dropped') = span ('-' ==) source
            tl = length taken
            ts = length dropped + oldlen
            acc' = if tl <= 1 then acc else (tl, ts):acc
            oldlen' = length s' - length dropped'

hs :: [String] -> [Block]
hs horizontal' = concat [ map (\ (len,c) -> Horizontal len (r,c)) segs | r <- [0..count-1], let l = horizontal' !! r, let segs = blocks l, (not . null) segs ] 

vs :: [String] -> [Block]
vs vertical' = concat [ map (\ (len,r) -> Vertical len (r,c)) segs | c <- [0..count-1], let l = vertical' !! c, let segs = blocks l, (not . null) segs ] 

findWordsByLen :: [String] -> Int -> [String] 
findWordsByLen allWords len = filter ((==len) . length) allWords

findOverlapsByBlock :: [String] -> Block -> (Block,[(Int,Int)])
findOverlapsByBlock horizontal block =
    case block of
        Horizontal blen (br,bc) -> (block,filter (`elem` overlaps horizontal) $ map (\ c -> (br,c)) [bc .. bc + blen])
        Vertical blen (br,bc) -> (block, filter (`elem` overlaps horizontal) $ map (\ r -> (r,bc)) [br .. br + blen])

findOverlappingChars :: [String] -> (Block,[(Int,Int)]) -> [((Int, Int),[(Char, (Block,String))])]
findOverlappingChars allWords (block, overlapsByBlock) =
    case block of
        Horizontal blen (_,bc) -> map (\ (r,c) -> ((r,c), map ( \ word -> (word!!(c-bc), (block,word))) $ findWordsByLen allWords blen)) overlapsByBlock
        Vertical blen (br,_) -> map (\ (r,c) -> ((r,c), map ( \ word -> (word!!(r-br), (block,word))) $ findWordsByLen allWords blen)) overlapsByBlock

-- listToPair :: [((Int, Int),[(Char, (Block,String))])] -> [((Int, Int),((Char, (Block,String)),(Char, (Block,String))))]
-- listToPair [] = []
-- listToPair [a] = [a]
-- listToPair (a:b) = (a,b)
-- listToPair (a:b:_) = (a,b)


isPairValid :: [(Block,[(Int,Int)])] -> (Block, String) -> (Block, String) -> Bool
isPairValid overlapsByBlock (hblock,hword) (vblock,vword) =
    length hvoverlaps == length hvchars
    where
        (Horizontal _ (_,hc), hoverlaps) =  head $ filter ((==hblock) .fst) overlapsByBlock
        (Vertical _ (vr,_), voverlaps) = head $ filter ((==vblock) .fst) overlapsByBlock
        hvoverlaps = hoverlaps `intersect` voverlaps
        hchars = map (\ (ovr, ovc) -> (ovr, ovc, hword !! (ovc - hc)) ) hvoverlaps
        vchars = map (\ (ovr, ovc) -> (ovr, ovc, vword !! (ovr - vr)) ) hvoverlaps
        hvchars = hchars `intersect` vchars


-- findBlocks :: [String] -> [String] -> [Block] -> (Int,Int) -> [((Block, String),(Block, String))]
-- findBlocks horizontal allWords allBlocks ov =
--     concat $ filter (not . null) [ concat [ [((hblock,hword), (vblock,vword))] | (hchar, (hblock,hword)) <- h, hchar == vchar] | (vchar, (vblock,vword)) <- v ]
--     where
--         overlapsByBlock = map (findOverlapsByBlock horizontal) allBlocks 
--         (h:v:_) = map snd $ filter ((==ov) . 

findBlocks :: [String] -> [String] -> [Block] -> (Int,Int) -> [(Block, String)]
findBlocks horizontal allWords allBlocks ov =
    concat $ filter (not . null) [ concat [ [(hblock,hword), (vblock,vword)] | (hchar, (hblock,hword)) <- h, hchar == vchar] | (vchar, (vblock,vword)) <- v ]
    where
        overlapsByBlock = map (findOverlapsByBlock horizontal) allBlocks 
        (h:v:_) = map snd $ filter ((==ov) . fst) $ concatMap (findOverlappingChars allWords) overlapsByBlock 


getPairsByBlock :: (Block, String) -> [((Int,Int),Char)] 
getPairsByBlock (block, word) =
    case block of
        Horizontal blen (br,bc) -> [((br,bc+i), word!!i) | i <- [0..blen-1]]
        Vertical blen (br,bc) -> [((br+i,bc), word!!i) | i <- [0..blen-1]]

split :: String -> Char -> [String]
split [] _ = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
    rest = split cs delim

fillCrossWords :: [String] -> [String] -> [String]
fillCrossWords input allWords =
    
    [ [  ch | j <- [0..count-1], let ch = fromMaybe '+' $ lookup (i,j) allPairs ] | i <- [0..count-1] ]
    where

        horizontal = input
        vertical = transpose input

        allBlocks = hs horizontal ++ vs vertical

        overlappingBlocks = concatMap (findBlocks horizontal allWords allBlocks) (overlaps horizontal)
        groupedBlocks = groupBy (\ (a,_) (b,_) -> a == b) $ map (\ lst -> (fst $ head lst, (length lst, snd $ head lst))) $ group $ sort overlappingBlocks

        filteredPairs = map (\lst -> (fst $ head lst, snd $ maximumBy (compare `on` fst) $  map snd lst)) groupedBlocks

        usedBlocks = map fst filteredPairs
        usedWords = map snd filteredPairs

        unusedWords = filter (`notElem` usedWords) allWords
        orphanBlocks = filter (`notElem` usedBlocks) allBlocks

        orphanPairs = getBlockWords orphanBlocks unusedWords []

        allPairs = nub $ concatMap getPairsByBlock (filteredPairs ++ orphanPairs)

main :: IO ()
main = do
    input <- replicateM 10 getLine
    wordsLine <- getLine


    -- let input = [ "+-++++++++", "+-++++++++", "+-------++", "+-++++++++", "+-----++++", "+-+++-++++", "+++-----++", "+++++-++++", "+++++-++++", "+++++-++++" ]
    -- let allWords = ["SYDNEY", "TURKEY", "DETROIT", "EGYPT", "PARIS"]

    let allWords = split wordsLine  ';'
    let output = fillCrossWords input allWords
    -- let output = []
    mapM_ putStrLn output 


