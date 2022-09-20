import Control.Monad (foldM, foldM_)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Num = Add Int | Fetch Int deriving (Show)

findPosition :: Seq.Seq Int -> Int -> Int -> Int -> Int
findPosition xs t s e =
  let m = div (s + e) 2
      sx = Seq.index xs s
      mx = Seq.index xs m
      ex = Seq.index xs e
   in if t <= sx
        then s
        else
          if t >= ex
            then e + 1
            else
              if mx == t
                then m
                else
                  if mx < t
                    then findPosition xs t (m + 1) e
                    else findPosition xs t s (m -1)

main :: IO ()
main = do
  line <- getLine
  let n = read line :: Int
  let cur = []

  (inputs, indices) <-
    foldM
      ( \(xs, s) n -> do
          l <- getLine
          let i = read l :: Int
          return $ if i > 0 then ((n, Add i) : xs, s) else ((n, Fetch (n + i)) : xs, Set.insert (n + i) s)
      )
      ([], Set.empty)
      [1 .. n]

  foldM_
    ( \(map, cur) (index, input) ->
        case input of
          Add i ->
            let len = Seq.length cur
                cur'
                  | len == 0 || i < Seq.index cur 0 = i Seq.<| cur
                  | i > Seq.index cur (len - 1) = Seq.insertAt len i cur
                  | otherwise =
                    let ipos = findPosition cur i 0 (len -1)
                     in Seq.insertAt ipos i cur
                pos = div (Seq.length cur' + 1) 2 - 1
                med = cur' `Seq.index` pos
             in do
                  print med
                  return $ if Set.member index indices then (Map.insert index (med, cur') map, cur') else (map, cur')
          Fetch i -> do
            let (med, cur') = map Map.! i
            let map' = if Set.member index indices then Map.insert index (med, cur') map else map
            print med
            return (map', cur')
    )
    (Map.empty, Seq.empty)
    $ reverse inputs
