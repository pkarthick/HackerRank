import Control.Monad
import Data.List

main :: IO ()
main = do
    count <- getLine
    allLines <- replicateM ((read count)* 2) $ getLine
    let twinList = takeN 2 allLines

    mapM_ (\[s, sub] -> 
        putStrLn $ if isSubstring s sub then "YES" else "NO") twinList

    where
        takeN :: Int -> [a] -> [[a]]
        takeN n [] = [];
        takeN n xs = [take n xs] ++ (takeN n $ drop n xs)

checkMatches :: String -> [(Char, Int)] -> Bool
checkMatches _ [] = True
checkMatches [] _ = False
checkMatches str ((ch, len):subgs) =
    all (== ch) (take len str) && checkMatches (drop len str) subgs

isSubstring :: String -> String -> Bool
isSubstring str sub = 
    isSubstring' str
    where
        (f, flen):subgs = map (\g -> (head g, length g)) $ group sub
        isSubstring' :: String -> Bool
        isSubstring' str' = 
            not (null matchingStart) 
            && (
                fgmatch && (null subgs || checkMatches (fromSecond) subgs 
                || isSubstring' fromSecond 
                ))
            where
                matchingStart = dropWhile (/= f) str'
                fgmatch = all (== f) $ take flen matchingStart
                fromSecond = dropWhile (== f) matchingStart


