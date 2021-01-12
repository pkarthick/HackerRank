import Control.Monad
import Data.List
import Data.Ord
import Control.Arrow
import GHC.Float
import Text.Printf

data Direction = Clockwise | AntiClockwise | Collinear deriving (Show, Eq, Ord)

data Point = Point Int Int deriving (Show, Ord, Eq)

crossProduct :: Point -> Point -> Direction
crossProduct (Point x1 y1) (Point x2 y2)
    | dir == 0 = Collinear
    | dir > 0 = Clockwise
    | otherwise = AntiClockwise
    where
        dir = x1 * y2 - x2 * y1

vector :: Point -> Point -> Point
vector (Point x1 y1) (Point x2 y2) =
    Point (x1-x2) (y1-y2)

direction :: Point -> Point -> Point -> Direction
direction p0 p1 p2 =
    crossProduct (vector p2 p0) (vector p1 p0)

comparePoints :: Point -> Point -> Point -> Ordering
comparePoints p0 p1 p2 =
    case direction p0 p1 p2 of
        AntiClockwise -> LT
        Clockwise -> GT
        Collinear ->
            if distanceSquared p1 p0 < distanceSquared p2 p0 then
                LT
            else
                GT


findBottomLeftMost :: [Point] -> Point -> [Point] -> (Point, [Point])

findBottomLeftMost [] p0 acc = (p0, acc)

findBottomLeftMost (p@(Point x y) : xt) (p0@(Point x0 y0)) acc
    | y < y0 || y == y0 && x < x0 = findBottomLeftMost xt p (p0 : acc)
    | otherwise = findBottomLeftMost xt p0 (p: acc)

filterCollinear :: Point -> [Point] -> [Point]
filterCollinear p0 ps =
    fil ps []
    where
        fil :: [Point] -> [Point] -> [Point]
        fil [] acc = reverse acc
        fil ps acc =
            case ps of
                p1 : p2 : pt | direction p0 p1 p2 == Collinear ->
                    fil (p2:pt) acc
                p : pt ->
                    fil pt (p:acc)

convexHull :: Point -> [Point] -> [Point]
convexHull p0 ps =

    traverse rest hull

    where

    filtered = filterCollinear p0 $ sortBy (comparePoints p0) ps

    hull = (reverse $ take 2 filtered) ++ [p0]
    rest = drop 2 filtered

    traverse :: [Point] -> [Point] -> [Point]
    traverse [] acc = reverse acc
    traverse (p:pt) acc =
        case acc of
            p1 : p2 : _ | direction p2 p1 p == AntiClockwise ->
                traverse pt (p: acc)
            _ ->
                traverse (p:pt) (tail acc)


distanceSquared :: Point -> Point -> Int
distanceSquared (Point x1 y1) (Point x2 y2) =
    (x1-x2)^2 + (y1-y2)^2

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
    sqrt $ fromIntegral $ (x1-x2)^2 + (y1-y2)^2

perimeter :: [Point] -> Double -> Double
perimeter [] pm = pm
perimeter (p1 : p2 : []) pm = pm + distance p1 p2
perimeter (p1 : p2 : pt) pm =
    perimeter (p2 : pt) (pm + distance p1 p2)


main :: IO ()
main = do
    n <- readLn :: IO Int
    ls <- replicateM n getLine
    let p:pt = map (((\ [x, y] -> Point x y) . map (read :: String -> Int)) . words) ls
    let (p0, ps) = findBottomLeftMost pt p []
    let hull = convexHull p0 ps

    let ans = perimeter (hull ++ [head hull]) 0.0
    printf "%.1f\n" ans
