import Control.Monad (replicateM)
import Data.List (maximumBy, nub, permutations, repeat)
import qualified Data.Map.Strict as Map (Map, insert, (!), (!?), member, lookup, singleton)

data Direction = DirRight | DirDown deriving (Show, Ord, Eq)

data Dice = Dice {total :: Int, top :: Int, left :: Int, front :: Int} deriving (Show, Ord, Eq)

rotateDice :: Direction -> Dice -> Dice
rotateDice DirRight Dice {total = v, top = t, left = l, front = f} =
  Dice {total = v + l, top = l, left = 7 - t, front = f}
rotateDice DirDown Dice {total = v, top = t, left = l, front = f} =
  Dice {total = v + 7 - f, top = 7 - f, left = l, front = t}

getDice :: (Int, Int) -> Map.Map (Int, Int) Dice -> Map.Map (Int, Int) Dice

getDice (1, 1) diceMap = diceMap

getDice (r, c) diceMap =
  if Map.member (r,c) diceMap then diceMap
  else
      let
        rn | r > 3 = 3
           | r > 2 = 2
           | otherwise = 1
        cn | c > 3 = 3
           | c > 2 = 2
           | otherwise = 1
        dirsList = nub $ permutations $ replicate rn DirDown ++ replicate cn DirRight
        dice = maximumBy (\d1 d2 -> compare (total d1) (total d2)) $ map (foldl (flip rotateDice) (diceMap Map.! (r-rn, c-cn))) dirsList
      in
        Map.insert (r, c) dice diceMap

createMap :: Int -> Map.Map (Int, Int) Dice
createMap n = do
  let dice = Dice {total = 1, top = 1, left = 3, front = 2}
  let diceMap = Map.singleton (1, 1) dice
  let firstCol = foldl (\diceMap x -> Map.insert (x, 1) (rotateDice DirDown $ diceMap Map.! (x-1, 1)) diceMap) diceMap [2 .. n]
  let firstRowAndCol = foldl (\diceMap x -> Map.insert (1, x) (rotateDice DirRight $ diceMap Map.! (1, x-1)) diceMap) firstCol [2 .. n]
  foldl
    ( \diceMap x -> do
        let diceMapRow = foldl (\diceMap r -> getDice (r, x) diceMap) diceMap [x .. n]
        foldl (\diceMap c -> getDice (x, c) diceMap) diceMapRow [x .. n]
    ) firstRowAndCol  [2 .. n]

main :: IO ()
main = do
  n <- fmap (read :: String -> Int) getLine
  lines <- replicateM n getLine
  let rcs = map (map (read :: String -> Int) . words) lines
  let max = maximum $ map maximum rcs
  let diceMap = createMap max
  mapM_ (print . total . (diceMap Map.!) . (\(a : b : _) -> (a, b))) rcs