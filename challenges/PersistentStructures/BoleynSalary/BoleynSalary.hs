import Control.Monad
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Tree = Tree (Int, Int) (S.Set (Int, Int)) [Tree] deriving (Show)

createTree :: M.Map Int [Int] -> M.Map Int Int -> M.Map Int Tree
createTree relMap salMap =
  create M.empty 1
  where
    create :: M.Map Int Tree -> Int -> M.Map Int Tree
    create treeMap sup =
      case M.lookup sup relMap of
        Just subs ->
          let treeMap1 = foldl create treeMap subs
              children = map (treeMap1 M.!) subs
              sals = S.unions $ map (\(Tree (i, sal) s _) -> S.insert (sal, i) s) children
              tree = Tree (sup, salMap M.! sup) sals children
           in M.insert sup tree treeMap1
        Nothing ->
          let tree = Tree (sup, salMap M.! sup) S.empty []
           in M.insert sup tree treeMap

main :: IO ()
main = do
  (n : q : _) <- map (read :: String -> Int) . words <$> getLine

  relMap <- M.fromList . map (\xs -> (fst $ head xs, map snd xs)) . groupBy (\(f, _) (s, _) -> f == s) . sort . map ((\(c : p : _) -> (p, c)) . map (read :: String -> Int) . words) <$> replicateM (n -1) getLine

  salMap <- M.fromList . zip [1 ..] . map (read :: String -> Int) . words <$> getLine
  let treeMap = createTree relMap salMap

  foldM_
    ( \ind _ -> do
        (off, pos) <- (\(off : pos : _) -> (off, pos)) . map (read :: String -> Int) . words <$> getLine
        let Tree _ s _ = treeMap M.! (ind + off)
        let (_, i) = S.elemAt (pos -1) s
        print i
        return i
    )
    0
    [1 .. q]
