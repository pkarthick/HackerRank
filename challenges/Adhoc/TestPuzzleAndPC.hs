module Test where

size = 32
(r,c) = (28,14)
(r',c') = (1,1)

let x = fillBlocks size (r,c) (r',c')

mapM_ print x
print $ length x
