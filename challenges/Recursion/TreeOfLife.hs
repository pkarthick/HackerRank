import Control.Monad (foldM, forM_)
import Data.Bits
import Data.Map (Map, empty, fromList, insert, lookup, singleton, (!))

data Tree
  = Leaf Bool
  | Node Tree Bool Tree
  deriving (Show, Eq, Ord)

data Token
  = Open
  | Close
  | Value Bool
  | TreeSeg Tree
  deriving (Show)

-- instance Show Tree where
--     show (Leaf b) = if b then "X" else "."
--     show (Node t1 b t2) = "(" ++ show t1 ++ " " ++ if b then "X" else "." ++ " " ++ show t2 ++ ")"

parseStr :: String -> [Token] -> Tree
parseStr [] [TreeSeg tree] = tree
parseStr (' ' : xs) ts = parseStr xs ts
parseStr ('(' : xs) ts = parseStr xs (Open : ts)
parseStr ('X' : xs) ts = parseStr xs (Value True : ts)
parseStr ('.' : xs) ts = parseStr xs (Value False : ts)
parseStr (')' : xs) ts =
  case ts of
    Value r : Value p : Value l : Open : tt -> parseStr xs (TreeSeg (Node (Leaf l) p (Leaf r)) : tt)
    TreeSeg r : Value p : Value l : Open : tt -> parseStr xs (TreeSeg (Node (Leaf l) p r) : tt)
    Value r : Value p : TreeSeg l : Open : tt -> parseStr xs (TreeSeg (Node l p (Leaf r)) : tt)
    TreeSeg r : Value p : TreeSeg l : Open : tt -> parseStr xs (TreeSeg (Node l p r) : tt)
    [TreeSeg tree, Open] -> parseStr xs [TreeSeg tree]
    _ -> error "Unexpected tokens!"
parseStr _ _ = error "Unexpected expression!"

getValue :: Tree -> Bool
getValue (Leaf x) = x
getValue (Node _ x _) = x

nextState :: Map [Bool] Bool -> (Tree -> Tree)
nextState rulesMap tree = nextState' False tree rulesMap

nextState' :: Bool -> Tree -> Map [Bool] Bool -> Tree
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
  map (\key -> ([(key .&. bit 3) /= 0, (key .&. bit 2) /= 0, (key .&. bit 1) /= 0, (key .&. bit 0) /= 0], (rule .&. bit key) /= 0)) [0 .. 15]

getRuleKey :: Bool -> Tree -> [Bool]
getRuleKey parent (Leaf l) = [parent, False, l, False]
getRuleKey parent (Node l c r) = [parent, getValue l, c, getValue r]

getSubTree :: Tree -> Char -> Tree
getSubTree (Node l _ _) '<' = l
getSubTree (Node _ _ r) '>' = r
getSubTree _ _ = error "Unexpected Node Kind!"

traverseTree :: Tree -> String -> Bool
traverseTree (Node _ p _) ['[', ']'] = p
traverseTree root ('[' : ds) = getValue $ foldl getSubTree root $ init ds
traverseTree root dirs = error $ "Unexpected: " ++ show root ++ " " ++ show dirs

updateMap :: Int -> Tree -> String -> Map (Int, String) Bool -> Map (Int, String) Bool
updateMap tree cur@(Node l@Node {} c r@Node {}) acc vmap =
  let vmap1 = insert (tree, acc) c vmap
      vmap2 = updateMap tree l (acc ++ "<") vmap1
      vmap3 = updateMap tree r (acc ++ ">") vmap2
   in vmap3
updateMap tree cur@(Node (Leaf l) c r@Node {}) acc vmap =
  let vmap1 = insert (tree, acc) c vmap
      vmap2 = insert (tree, acc ++ "<") l vmap1
      vmap3 = updateMap tree r (acc ++ ">") vmap2
   in vmap3
updateMap tree cur@(Node l@Node {} c (Leaf r)) acc vmap =
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

main :: IO ()
main = do
  num <- fmap (read :: String -> Int) getLine
  let rulesMap = fromList $ ruleBook num

  exp <- getLine
  qc <- fmap (read :: String -> Int) getLine

  (max_step, pairs) <-
    foldM
      ( \(max_step, ps) _ -> do
          let (cur, _) = head ps
          ws <- fmap words getLine
          let (n, dirs) = case ws of
                [dirs] -> (0, dirs)
                (ns : dirs : _) -> ((read :: String -> Int) ns, dirs)
                [] -> error "Unexpected number of arguments in query"
          return $ (if cur + n > max_step then cur + n else max_step, (cur + n, dirs) : ps)
      )
      (0, [(0, [])])
      [1 .. qc]

  let tree = case exp of
        "." -> Leaf False
        "X" -> Leaf True
        _ -> parseStr exp []

  let stateMap = take (max_step + 1) $ iterate (nextState rulesMap) tree

  forM_
    (reverse $ init pairs)
    ( \(cur, dirs) -> do
        let tree = stateMap !! cur
        putChar $ if traverseTree tree dirs then 'X' else '.'
        putStrLn ""
    )
