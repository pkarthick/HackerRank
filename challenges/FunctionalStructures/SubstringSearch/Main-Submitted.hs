import Control.Monad
import Data.List
import Control.Arrow

main :: IO ()
main = do
    count <- getLine
    replicateM_ (read count) $ do 
        s <- getLine 
        sub <- getLine
        putStrLn $ if isSubstring s sub then "YES" else "NO"

checkMatches :: String -> [(Char, Int)] -> Bool
checkMatches _ [] = True
checkMatches [] _ = False
checkMatches str ((ch, len):subgs) =
    all (== ch) (take len str) && checkMatches (drop len str) subgs

isSubstring :: String -> String -> Bool
isSubstring str sub = 
    isSubstring' str
    where
        (f, flen):subgs = map (head &&& length) $ group sub
        isSubstring' :: String -> Bool
        isSubstring' str' 
            | null matchingStart = False
            | fgmatch = null subgs || checkMatches fromSecond subgs || isSubstring' fromSecond
            where
                matchingStart = dropWhile (/= f) str'
                fgmatch = all (== f) $ take flen matchingStart
                fromSecond = dropWhile (== f) matchingStart


