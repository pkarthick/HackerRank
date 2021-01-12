import Control.Monad

meetingHeight :: [Int] -> Int
meetingHeight [] = error "This is not a valid input"
meetingHeight [h] = h
meetingHeight [h,t] = lcm h t
meetingHeight (f:s:t) = meetingHeight $ lcm f s : t

main :: IO ()
main = do
    _ <- getLine
    s <- getLine
    print $ meetingHeight $ map (read::String->Int) $ words s
