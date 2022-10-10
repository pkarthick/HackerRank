import Control.Monad (foldM_)
import Data.ByteString.Char8 qualified as BS
import Data.IntMap.Strict qualified as M
import Data.Maybe (fromJust)

data Army = EmptyArmy | Army Int Army deriving (Show)

maxAbility :: Army -> (Int, Army)
maxAbility (Army ability army) = (ability, army)
maxAbility EmptyArmy = (0, EmptyArmy)

recruit :: Int -> Army -> Army
recruit soldier army@(Army max army11) = if soldier >= max then Army soldier army else Army max $ recruit soldier army11
recruit soldier EmptyArmy = Army soldier EmptyArmy

kill :: Army -> Army
kill (Army max army) = army
kill EmptyArmy = EmptyArmy

merge :: Army -> Army -> Army
merge army EmptyArmy = army
merge EmptyArmy army = army
merge army1@(Army max1 a1) army2@(Army max2 a2) =
  if max1 >= max2
    then Army max1 (merge a1 army2)
    else Army max2 (merge a2 army1)

process :: M.IntMap Army -> [Int] -> IO (M.IntMap Army)
process armies (3 : army_id : soldier : _) =
  if M.member army_id armies
    then
      let army = recruit soldier $ armies M.! army_id
       in return (M.insert army_id army armies)
    else return (M.insert army_id (Army soldier EmptyArmy) armies)
process armies (4 : army_id : army_id2 : _) = do
  let army = merge (armies M.! army_id) (armies M.! army_id2)
  return (M.delete army_id2 $ M.insert army_id army armies)
process armies (2 : army_id : args) = do
  let army = kill (armies M.! army_id)
  return (M.insert army_id army armies)
process armies (1 : army_id : _) = do
  print $ fst $ maxAbility $ armies M.! army_id
  return armies

main :: IO ()
main = do
  xs <-
    map
      ( map (fst . fromJust . BS.readInt)
          . BS.split ' '
      )
      . tail
      . BS.split '\n'
      <$> BS.getContents

  foldM_ process M.empty xs
