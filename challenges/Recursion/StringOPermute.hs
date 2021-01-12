import Control.Monad

swap :: String -> String -> String
swap [] acc = reverse acc
swap (f:s:t) acc = swap t $ f:s:acc

main :: IO ()
main = do
    n <- fmap (read::String->Int) getLine
    replicateM_ n $ do
        line <- getLine
        putStrLn $ swap line []
