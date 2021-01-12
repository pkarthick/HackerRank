import Control.Monad

newtype Tree = Node (String, [Tree]) deriving (Show, Eq)

newtype Position = Position (Int, Tree) deriving (Show, Eq)

replaceChild :: Position -> [Position]-> [Position]

replaceChild (Position(0,ch)) [Position(0, Node (pv, ps))] =
    [Position(0, Node (pv, ch: drop 1 ps))]

replaceChild (Position(ci,ch)) [Position(0, Node (pv, ps))] =
    [Position(0, Node (pv, take ci ps ++ (ch: drop (ci+1) ps)))]

replaceChild (Position(0,ch')) (Position(pai, Node (pv, ps)):ds) = do
    let p' = Position(pai, Node (pv, ch': drop 1 ps))
    p':replaceChild p' ds

replaceChild (Position(ci,ch')) (Position(pai, Node (pv, ps)):ds) = do
    let p' = Position(pai, Node (pv, take ci ps ++ (ch': drop (ci+1) ps)))
    p':replaceChild p' ds

replaceChild _ _ = error "Invalid Scenario in replaceChild!"


changeValue :: String -> [Position] -> [Position]
changeValue v [Position(0,Node (_, cs))] = [Position(0,Node (v, cs))]

changeValue v (Position(i,Node (_, cs)):ds) = do
    let ch' = Node (v, cs)
    Position(i,ch'):replaceChild (Position(i,ch')) ds

changeValue _ [] = error "Invalid Scenario in changeValue!"

insertChild :: String -> [Position] -> [Position]
insertChild v [Position(0,Node (rv, cs))] = 
    [Position(0,Node (rv, Node (v, []):cs))]
    
insertChild v (Position(i,Node (cv, cs)):ds) = do
    let c' = Position(i,Node (cv, Node (v, []):cs))
    c':replaceChild c' ds
    
insertChild _ _ = error "Invalid Scenario in insertChild!"

insertRight :: String -> [Position] -> [Position]

insertRight cv (Position(i,ch):Position(pai, Node (pv, ps)):ds) = do
    let p' = Position(pai, Node (pv, take (i+1) ps ++ Node (cv, []): drop (i+1) ps))
    if null ds 
        then [Position(i,ch),p']
        else 
            Position(i, ch):p':replaceChild p' ds

insertRight _ _ = error "Invalid Scenario in insertRight!"

insertLeft :: String -> [Position] -> [Position]

insertLeft v (Position(i,ch):Position(pai, Node (pv, ps)):ds) = do
    let p' = Position(pai, Node (pv, take i ps ++ Node (v, []): drop i ps))
    if null ds 
        then [Position(i+1,ch),p']
        else 
            Position(i+1, ch):p':replaceChild p' ds

insertLeft _ _ = error "Invalid Scenario in insertLeft!"


visitChild :: Int -> [Position] -> [Position]
visitChild ci xs@(Position(_,Node (_, chs)):_) = 
    let ch = chs!!(ci-1)
    in Position(ci-1, ch):xs
visitChild _ _ = error "Invalid Scenario in visitChild!"

visitRight :: [Position] -> [Position]
visitRight (Position(i,_):p@(Position(_,Node (_, ps))):ds) = 
    Position(i+1, ps!!(i+1)):p:ds
visitRight _ = error "Invalid Scenario in visitRight!"

visitLeft :: [Position] -> [Position]
visitLeft (Position(i,_):p@(Position(_,Node (_, ps))):ds) = 
    Position(i-1,ps!!(i-1)):p:ds
visitLeft _ = error "Invalid Scenario in visitLeft!"

visitParent :: [Position] -> [Position]
visitParent (_:p:ds) = p:ds
visitParent _ = error "Invalid Scenario in visitParent!"

deleteChild :: [Position] -> [Position]

deleteChild (Position(0,_):Position(pai, Node (pv, ps)):ds) = 
    let p' = Position (pai, Node (pv, tail ps))
    in p':if null ds then [] else replaceChild p' ds

deleteChild (Position(1,_):Position(pai, Node (pv, ps)):ds) = 
    let p' = Position (pai, Node (pv, head ps : drop 2 ps))
    in p':if null ds then [] else replaceChild p' ds

deleteChild (Position(i,_):Position(pai, Node (pv, ps)):ds) = 
    let p' = Position (pai, Node (pv, take i ps ++ drop (i+1) ps))
    in p':if null ds then [] else replaceChild p' ds

deleteChild _ = error "Invalid Scenario in deleteChild!"

main :: IO ()
main = do
    n <- (read::String->Int) <$> getLine
    ls <- replicateM n getLine
    foldM_ (\cur l -> 
                
            --  do

            --     putStrLn "<<<<<<<<<"
            --     mapM_ print cur
            --     putStrLn ">>>>>>>>>"
            --     putStrLn ""

                case words l of
                    ["change", v] -> return $ changeValue v cur
                    ["print"] -> do
                        let Position(_, Node (v, _)) = head cur
                        putStrLn v
                        return cur
                    ["insert", "child", v] -> return $ insertChild v cur
                    ["insert", "right", v] -> return $ insertRight v cur
                    ["insert", "left", v] -> return $ insertLeft v cur
                    ["visit", "right"] -> return $ visitRight cur
                    ["visit", "left"] -> return $ visitLeft cur
                    ["visit", "child", v] -> return $ visitChild ((read::String->Int) v) cur
                    ["visit", "parent"] -> return $ visitParent cur
                    ["delete"] -> return $ deleteChild cur
                    _ -> error $ "Invalid Input!" ++ l
            ) [Position (0,Node ("0", []))] ls
