import Control.Monad

data Matrix = Empty | Matrix { rows::Int, columns::Int, cells::[Int], child::Matrix} deriving Show

main :: IO ()
main = do

    [r, _, n] <- map (read::String -> Int) . words <$> getLine

    matrix <- replicateM r (map (read :: String -> Int) . words <$> getLine)

    -- putStrLn ""
    -- print $ flattenMatrix matrix
    -- putStrLn ""
    -- print $ rotateMatrix n $ flattenMatrix matrix
    -- putStrLn ""
    -- print $ getData $ rotateMatrix n $ flattenMatrix matrix
    -- putStrLn ""

    mapM_ (putStrLn . unwords . map show) $ getData $ rotateMatrix n $ flattenMatrix matrix


    where
        flattenMatrix :: [[Int]] -> Matrix
        flattenMatrix [f, s] = Matrix { rows=2, columns=length f, cells= f ++ reverse s, child = Empty}

        flattenMatrix xs =
            let rc = length xs
                cc = length $ head xs
                middlerows = init $ tail xs
                cs = if rc == 1 then head xs else head xs ++ map last middlerows ++ reverse (last xs) ++ reverse (map head middlerows)
                inner = if rc == 1 then [] else map (init . tail) middlerows
            in
                if cc == 2 then
                    Matrix { rows=rc, columns=cc, cells= cs, child = Empty}
                else
                    Matrix { rows=rc, columns=cc, cells= cs, child = if null inner then Empty else flattenMatrix inner}

        rotateMatrix :: Int -> Matrix -> Matrix
        rotateMatrix _ Empty = Empty
        rotateMatrix _ matrix@Matrix{ rows=1 } = matrix
        rotateMatrix _ matrix@Matrix{ columns=1 } = matrix
        rotateMatrix times matrix@Matrix{ rows=rc, columns=cc, cells= cs, child = ch } =
            let
                frc = (rc-1) * 2 + (cc-1) * 2
                times' = if times <= frc then times else times `mod` frc
                cs' = drop times' cs ++ take times' cs
            in
                if times' == 0 then matrix
                else Matrix{ rows=rc, columns=cc, cells= cs', child = rotateMatrix times ch }

        getData :: Matrix -> [[Int]]

        getData Empty = []

        getData Matrix{ rows=1, cells=cs } = [cs]

        getData Matrix{ columns=1, cells=cs} = [[c] | c <- cs]

        getData Matrix{ rows=2, columns=cc, cells=cs} = take cc cs : [reverse $ drop cc cs]

        getData Matrix{ columns=2, rows=rc, cells=cs} =
             [firstrow] ++ zipWith (\a b -> [a, b]) firstcol lastcol ++ [lastrow]
             where
                firstrow = take 2 cs
                lastcol  = take (rc-2) $ drop 2 cs
                lastrow = reverse $ take 2 $ drop rc cs
                firstcol = reverse $ drop (2 + rc) cs


        getData Matrix{ rows=rc, columns=cc, cells= cs, child = ch } =
            let firstrow = take cc cs
                lastcol = map (: []) $ take (rc-2) $ drop cc cs
                lastrow = take cc $ drop (rc-2+cc) cs
                firstcol = map (: []) $ reverse $ drop (cc*2+rc-2) cs
                innerCells = getData ch
            in
                [firstrow] ++
                zipWith3 (\ a b c -> a ++ b ++ c ) firstcol innerCells lastcol
                ++ [reverse lastrow]
