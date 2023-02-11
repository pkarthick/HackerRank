import Data.List (sort)
import Control.Monad (mapM_)

main :: IO ()
main = do
  (_ : k : _) <- map (read :: String -> Int) . words <$> getLine
  nums <- dropWhile (<0) . map (read :: String -> Int) . words <$> getLine
  let xs = topK k nums 0 (head nums) (0, Nothing) []
  mapM_ print $ take k $ reverse $ sort xs

topK :: Int -> [Int] -> Int -> Int -> (Int, Maybe [Int]) -> [Int] -> [Int]

topK k [] tot mintot (maxtot, maxrem) ksums = do
  case maxrem of
    Nothing -> 
      if tot > maxtot then tot:ksums else (if maxtot > 0 then maxtot:ksums else ksums) 
    Just xs -> do
      if tot > maxtot then 
        tot:ksums
      else do
        let ns1 = dropWhile (< 0) xs
        topK k ns1 0 (head ns1) (0, Nothing) (if maxtot > 0 then maxtot:ksums else ksums) 

topK k (n:ns) tot mintot (maxtot, maxrem) ksums = do
  
  if tot >= 0 then
      if (n < 0) && (tot > maxtot) then
        topK k ns (tot + n) mintot (tot, Just ns) ksums
      else
        topK k ns (tot + n) mintot (maxtot, maxrem) ksums
  else do
    if tot <= mintot then 
      case maxrem of
        Nothing -> 
          topK k (n:ns) tot mintot (maxtot, Nothing) (if maxtot > 0 then maxtot:ksums else ksums)
        Just xs -> 
          let ns1 = dropWhile (< 0) xs
          in topK k ns1 0 (head ns1) (0, Nothing) (if maxtot > 0 then maxtot:ksums else ksums)
    else
        topK k ns (tot+n) mintot (maxtot, maxrem) ksums
      
