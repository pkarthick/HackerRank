import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Text.Printf

data Point = Point Int Int deriving (Show, Eq, Ord)

data Turn = TurnRight | TurnDown deriving (Show, Eq, Ord)

data Dice = Dice {top :: Face, front :: Face, left :: Face, right :: Face, bottom :: Face, back :: Face} deriving (Show)

rotateRight :: Dice -> Dice
rotateRight Dice {top = t, front = f, right = r, left = l, bottom = bo, back = ba} =
  Dice {top = l, front = f, right = t, left = bo, bottom = r, back = ba}

rotateDown :: Dice -> Dice
rotateDown Dice {top = t, front = f, right = r, left = l, bottom = bo, back = ba} =
  Dice {top = ba, front = t, right = r, left = l, bottom = f, back = bo}

turnDice :: Turn -> Dice -> Dice
turnDice turn
  | turn == TurnRight = rotateRight
  | otherwise = rotateDown

data Face
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Show, Eq, Ord, Bounded, Enum)

toEnum' x = toEnum (x - 1)

fromEnum' x = fromEnum x + 1

rotateDice :: (Point, [(Turn, Dice)]) -> Turn -> (Point, [(Turn, Dice)])
rotateDice (Point cr cc, dices@((_, dice) : _)) turn =
  let dice'@Dice {top = t, front = f, right = r, left = l, bottom = bo, back = ba} = turnDice turn dice
      c' = if turn == TurnRight then Point cr (cc + 1) else Point (cr + 1) cc
   in (c', (turn, dice') : dices)

sumOfPath :: M.Map Point [Int] -> Point -> (Point, [(Turn, Dice)]) -> (M.Map Point [Int], Point, [(Turn, Dice)])
sumOfPath m t@(Point tr tc) (c@(Point cr cc), dices) =
  let turns =
        case (cr < tr, cc < tc) of
          (True, True) -> [TurnDown, TurnRight]
          (True, False) -> [TurnDown]
          (False, True) -> [TurnRight]
          (False, False) -> []
   in let xs = map (rotateDice (c, dices)) turns
          m' = foldl (\m (c, ds) -> M.insert c (map (fromEnum' . top . snd) ds) m) m xs
       in if null turns
            then (m, c, dices)
            else
              maximumBy
                (\(_, dc, dds) (_, rc, rds) -> compare (sum $ map (fromEnum' . top . snd) dds) (sum $ map (fromEnum' . top . snd) rds))
                $ map (sumOfPath m' t) xs

rotate :: Dice -> String -> Int -> (Int, Dice)
rotate dice [] total = (total + 1, dice)
rotate dice ('D' : xs) total =
  let dice'@Dice {top = t, front = f, right = r, left = l, bottom = bo, back = ba} = rotateDown dice
   in rotate dice' xs (total + fromEnum' t)
rotate dice ('R' : xs) total =
  let dice'@Dice {top = t, front = f, right = r, left = l, bottom = bo, back = ba} = rotateRight dice
   in rotate dice' xs (total + fromEnum' t)


-- variants :: (Int, Int) -> [((Int,Int), [[Turn]])] -> IO [((Int,Int), [[Turn]])] 
-- variants (3,3) xs = return xs
-- variants (r,3) xs = (xs ++) <$> variants (r+1,3) [((r+1,3), map (TurnRight :) (concatMap snd xs))]
-- variants (3,c) xs = (xs ++) <$> variants (3,c+1) [((3,c+1), map (TurnDown :) (concatMap snd xs))]
-- variants (r,c) xs = do
--   ys <- (xs ++) <$> variants (r+1,c) [((r+1,c), map (TurnRight :) $ concatMap snd xs)]  
--   (ys ++) <$> variants (r,c+1) [((r,c+1), map (TurnDown :) $ concatMap snd xs)]


variants :: (Int, Int) -> [[Turn]]

variants (1,1) = []

variants (1,2) = [[TurnRight]]

variants (2,1) = [[TurnDown]]

variants (1, n) = [replicate (n-1) TurnRight]

variants (n, 1) = [replicate (n-1) TurnDown]

variants (r,c) = 
  map (TurnDown: ) (variants (r-1,c)) ++ map (TurnRight: ) (variants (r,c-1)) 




-- variants :: (Int, Int) -> M.Map (Int,Int) [[Turn]] -> IO (M.Map (Int,Int) [[Turn]])

-- variants (3,c) m = return m

-- variants (r, 3) m = return m

-- variants (r,c) m = do

--   if M.notMember (r,c) m then do
--     m' <- variants (r, c+1) (M.insert (r,c+1) [[TurnRight]] m)
--     variants (r+1, c) (M.insert (r+1,c) [[TurnDown]] m')
--   else do
--     let ts = m M.! (r,c)
--     m' <- variants (r, c+1) (M.insert (r,c+1) (map (TurnRight:) ts) m)
--     variants (r+1, c) (M.insert (r+1,c) (map (TurnDown:) ts) m')
    

  -- ys <- (xs ++) <$> variants (r+1,c) [((r+1,c), map (TurnRight :) $ concatMap snd xs)]  
  -- (ys ++) <$> variants (r,c+1) [((r,c+1), map (TurnDown :) $ concatMap snd xs)]


-- variants :: (Int, Int) -> Turn -> M.Map (Int,Int) [[Turn]] -> M.Map (Int,Int) [[Turn]]

-- variants (1,1) TurnDown m = 
--     M.insert (2, 1) [[TurnDown]] m

-- variants (1,1) TurnRight m = 
--     M.insert (1, 2) [[TurnRight]] m

-- variants (1, c) TurnRight m = 
--   foldl (\mx x -> M.insert (1, x) [TurnRight : replicate x TurnDown] mx) m [1 .. c]

-- variants (r, 1) TurnDown m = 
--   foldl (\mx x -> M.insert (x, 1) [TurnDown : replicate x TurnRight] mx) m [1 .. r]
  

-- variants (r,c) t m = 
--   if M.member (r,c) m then 
--     let ts = m M.! (r,c)
--         (r',c') = if t == TurnRight then (r, c+1) else (r+1, c)
--     in M.insert (r', c') (map (t :) ts) m
--   else
--     let m' = variants (r-1, c) t m
--         -- m'' = variants (r, c-1) t m'
--         ts = m M.! (r,c)
--         -- ts2 = m M.! (r,c-1)
--         -- ts = ts1 ++ ts2
--     in M.insert (r, c) (map (t :) ts) m

  -- xs ++ variants (r+1,c) [((r+1,c), map (TurnRight :) $ map snd xs)]  ++ variants (r,c+1) [((r,c+1), map (TurnDown :) $ map snd xs)]


calcTotal:: Dice -> (Int, Int) -> (Int, Int) -> M.Map (Int,Int) Int -> Int -> M.Map (Int,Int) Int

calcTotal dice (tr, tc) (r, c) m t 
  | tr == r && tc == c = m'

  | tc == c =
      calcTotal ddice (tr, tc) (r+1, tc) m' dt

  | tr == r =

      calcTotal rdice (tr, tc) (tr, c+1) m' rt

  | otherwise  =

      let rm = calcTotal rdice (tr, tc) (r, c+1) m' rt
      in calcTotal ddice (tr, tc) (r+1, c) rm dt

      where
        ddice = rotateDown dice
        rdice = rotateRight dice
        dt = t + fromEnum' (top ddice)
        rt = t + fromEnum' (top rdice)
        m' = if M.notMember (r,c) m || t > m M.! (r,c) then M.insert (r,c) t m else m


getTotal :: (Int, Int) -> M.Map (Int,Int) Int -> M.Map (Int,Int) Int 
getTotal (r,c) m =
  if M.member (r,c) m 
    then m
    else 
      let xs = [(rx, cx) | rx <- [1 .. 3], cx <- [1 .. 3]]
          vs = map variants xs
      in foldl (\mx v -> getTotal v mx) m vs


main :: IO()
main = do

    let one = Dice {top = One, front = Two, right = Four, left = Three, bottom = Six, back = Five}

    let m = calcTotal one (4, 4) (1,1) M.empty 1

    print m

    t <- (read::String->Int) <$> getLine

    replicateM_ t $ do
        [r, c] <- map (read::String->Int) . words <$> getLine
        print $ m M.! (r,c)
        
