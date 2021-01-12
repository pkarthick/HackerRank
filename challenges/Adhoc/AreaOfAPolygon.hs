import Control.Monad

triangleArea :: Fractional a => (a,a) -> (a,a) -> (a,a) -> a
triangleArea (ax, ay) (bx, by) (cx, cy) =
    abs $ (ax * (by-cy) + bx * (cy-ay) + cx * (ay-by)) * 0.5

polygonArea :: Fractional t => [(t, t)] -> t
polygonArea [] = 0.0
polygonArea [_,_] = 0.0
polygonArea ((ax,ay):t) = polygonArea' (ax,ay) t 0.0

polygonArea' :: Fractional t => (t, t) -> [(t, t)] -> t -> t
polygonArea' (_, _) [] acc = acc
polygonArea' (_, _) [(_,_)] acc = acc

polygonArea' (ax, ay) [(bx,by),(cx,cy)] acc =
    acc + triangleArea (ax, ay) (bx, by) (cx, cy)

polygonArea' (ax, ay) ((bx,by):(cx,cy):t) acc =
    polygonArea' (ax, ay) ((cx,cy):t) (acc + triangleArea (ax, ay) (bx, by) (cx, cy))


main :: IO ()
main = do
    n <- readLn
    pointsLines <- replicateM n getLine
    let points = map ( (\ (a:b:_) -> (a,b) ) . map (read::String->Float) . words) pointsLines
    print $ polygonArea points
    