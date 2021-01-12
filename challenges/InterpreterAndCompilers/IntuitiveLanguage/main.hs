import System.IO (isEOF)
import qualified Data.Map.Strict as M
import Data.Char
import Data.List 

newtype Value = Value (Int, Int) deriving (Eq)

data FunctionValue = FunctionValue {count::Int, val::Value, coefs::[Value]} deriving (Eq, Show)

data Token = Var String | Val (Int, Int) | Separator Char | Terminator Char | Operator Char | OpenSquareBracket | CloseSquareBracket | OpenBracket | CloseBracket deriving (Eq, Show)

data Function = Function String FunctionValue deriving (Eq, Show)

newtype Argument = Argument [Token] deriving (Eq, Show)

data Expression 
    = Literal Value
    | FuncVal [Value]
    | Expression [Token] 
    | FuncCall Function [Expression]
    | Negative Expression
    | Binary Char Expression Expression
    deriving (Eq, Show)

instance Show Value where
    show (Value (0, 1)) = "0"
    show (Value (x, 1)) = show x 
    show (Value (x, y)) 
        | x `mod` y == 0 = show (x `div` y)
        | y `mod` x == 0 = "1/" ++ show (y `div` x)
        | otherwise = 
            let (x', y') = applyFactor (x, y) 0
            in show x' ++ '/' : show y'

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator _ = False

negateValue :: Value -> Value
negateValue (Value (x,y)) = Value (-x, y)

factors :: Int -> [Int]
factors m = 
    f m (head primes) (tail primes) 
    where
        f m' n ns
            | m' < 2 = []
            | m' < n ^ 2 = [m']   -- stop early
            | m' `mod` n == 0 = n : f (m' `div` n) n ns
            | otherwise = f m' (head ns) (tail ns)

primes :: [Int]
primes = 2 : filter (\n-> head (factors n) == n) [3,5..]    

applyFactor :: (Int, Int) -> Int -> (Int, Int)
applyFactor (x, y) _ =
    let xf = factors x
        yf = factors y
        cf =  xf \\ (xf \\ yf)
        f = product cf
    in 
        if f == 1 then (x, y) else (x `div` f, y `div` f)


simplify :: M.Map String Function -> Expression -> [Value]
simplify _ (Literal v) = [v]
simplify _ (FuncVal vs) = vs
simplify m (Expression ts) = evalTokens m ts
simplify m (FuncCall fn exs) = 
    let vs = map (head . simplify m) exs
    in callFunction fn vs
simplify m (Negative ex) =
    let [v] = simplify m ex
    in [negateValue v]
simplify m (Binary op f s) =
    let [fv] = simplify m f
        [sv] = simplify m s
    in [operate op fv sv]

operate :: Char -> Value -> Value -> Value
operate '*' (Value (x1,y1)) (Value (x2,y2)) =
    Value (x1 * x2, y1 * y2)
operate '/' (Value (x1,y1)) (Value (x2,y2)) =
    Value (x1 * y2, y1 * x2)
operate '+' (Value (x1,y1)) (Value (x2,y2)) =
    Value (x1 * y2, y1 * x2)
operate '-' (Value (x1,y1)) (Value (x2,y2)) =
    Value (x1 * y2, y1 * x2)
operate _ _ _ = error "Error in operate!"

readParameter :: [Token] -> Int -> [Token] -> ([Token], [Token])
readParameter [] _ acc = (reverse acc, [])
readParameter (OpenSquareBracket:Operator '-':Val (x,y):ts) 0 [] = readParameter ts 1 [Val (-x,y)]
readParameter (OpenSquareBracket:ts) 0 [] = readParameter ts 1 []
readParameter ts 0 [] = ([], ts)
readParameter (CloseSquareBracket:ts) 1 acc = (reverse acc,ts)
readParameter (CloseSquareBracket:ts) n acc = readParameter ts (n-1) (CloseSquareBracket:acc)
readParameter (OpenSquareBracket:Operator '-':Val (x,y):ts) n acc = readParameter ts (n+1) (Val (-x,y):OpenSquareBracket:acc)
readParameter (OpenSquareBracket:ts) n acc = readParameter ts (n+1) (OpenSquareBracket:acc)
readParameter (t:ts) n acc = readParameter ts n (t:acc)

readParameters :: [Token] -> [[Token]] -> ([[Token]], [Token])
readParameters xs@(OpenSquareBracket:_) acc =
    let (ts, rts) = readParameter xs 0 []
    in readParameters rts (ts:acc)
readParameters xs acc = (reverse acc, xs)

evaluateVar :: M.Map String Function -> [Token] -> [Token]
evaluateVar m (Var n:tt) =
    let Just fn = M.lookup n m
        (pts, rts) = readParameters tt []
        args = map (head . evalTokens m) pts
        vs = callFunction fn args
        vals = map (\(Value v) -> Val v) vs
    in 
        if null rts then
            vals
        else
            evaluateVar m (head vals:rts)

evaluateVar _ ts = ts

evalBrackets' :: M.Map String Function -> [Token] -> [Token] -> [Token]

evalBrackets' _ [] as = reverse as

evalBrackets' m (OpenBracket:ts) as = 
    let ts' = evalBrackets m ts
    in evalBrackets' m ts' as

evalBrackets' m (CloseBracket:ts) as = 
    let [Value (x,y)] = evalTokens m (reverse as)
    in Val (x,y) : ts 

evalBrackets' m (t:ts) as = evalBrackets' m ts (t:as)


evalBrackets :: M.Map String Function -> [Token] -> [Token]
evalBrackets m ts = evalBrackets' m ts []

evalMulDiv :: M.Map String Function -> [Token] -> [Token] -> [Token]
evalMulDiv _ [] ts = reverse ts

evalMulDiv m [Var n] ys = 
    let Just (Function _ FunctionValue{count=_, val=Value v, coefs=cs}) = M.lookup n m
        vals = map (\(Value v') -> Val v') cs ++ [Val v]
    in evalMulDiv m [] (reverse vals ++ ys)

evalMulDiv m ts@(Var _:_) ys = evalMulDiv m (evaluateVar m ts) ys

evalMulDiv _ [Val (x1,y1)] ts = reverse (Val (x1,y1) : ts)

--evalMulDiv m (Operator '-': Val (x1,y1): xt) ts = evalMulDiv m (Val(-x1,y1):xt) ts

evalMulDiv m (Val (x1,y1): Operator '*': Var n:xt) ts =
    evalMulDiv m (Val (x1,y1): Operator '*':evaluateVar m (Var n:xt)) ts

evalMulDiv m (Val (x1,y1): Operator '/': Var n:xt) ts =
    evalMulDiv m (Val (x1,y1): Operator '/':evaluateVar m (Var n:xt)) ts

evalMulDiv m (Val (x1,y1): Operator '*': Operator '-': Val (x2,y2):xt) ts = 
    evalMulDiv m (Val (applyFactor (x1 * (-x2), y1 * y2) 0):xt) ts

evalMulDiv m (Val (x1,y1): Operator '*': Val (x2,y2):xt) ts = 
    evalMulDiv m (Val (applyFactor (x1 * x2, y1 * y2) 0):xt) ts

evalMulDiv m (Val (x1,y1): Operator '/': Operator '-': Val (x2,y2):xt) ts = 
    evalMulDiv m (Val (applyFactor (x1 * (-y2), y1 * x2) 0):xt) ts

evalMulDiv m (Val (x1,y1): Operator '/': Val (x2,y2):xt) ts = 
    evalMulDiv m (Val (applyFactor (x1 * y2, y1 * x2) 0):xt) ts

evalMulDiv m (t:xt) ts = evalMulDiv m xt (t:ts)

evalAddSub :: M.Map String Function -> [Token] -> [Value]

evalAddSub m [Var n] = 
    let Just (Function _ FunctionValue{count=0, val=v}) = M.lookup n m
    in [v]

evalAddSub m ts@(Var _:_) = evalAddSub m (evaluateVar m ts)

evalAddSub _ [Val (x,y)] 
    | x `mod` y == 0 = [Value (x `div` y, 1)]
    | y `mod` x == 0 = [Value (1, y `div` x)]
    | otherwise = [Value $ applyFactor (x, y) 0]

evalAddSub m (Val (x1,y1): Operator '+': Var n:xt) =
    evalAddSub m (Val (x1,y1): Operator '+':evaluateVar m (Var n:xt))

evalAddSub m (Val (x1,y1): Operator '-': Var n:xt) =
    evalAddSub m (Val (x1,y1): Operator '-':evaluateVar m (Var n:xt))

evalAddSub m (Val (x1,y1): Operator '+': Val (x2,y2):xt) 
    | y1 == y2 = evalAddSub m (Val (x1 + x2, y1):xt)
    | otherwise = evalAddSub m (Val (applyFactor (x1 * y2 + (x2 * y1), y1 * y2) 0):xt)

evalAddSub m (Val (x1,y1): Operator '-': Val (x2,y2):xt) 
    | y1 == y2 = evalAddSub m (Val (x1 - x2, y1):xt)
    | y1 `mod` y2 == 0 = evalAddSub m (Val (x1 - x2 * (y1 `div` y2), y1):xt)
    | y2 `mod` y1 == 0 = evalAddSub m (Val (x1 * (y2 `div` y1) - x2, y2):xt)
    | otherwise = evalAddSub m (Val (applyFactor (x1 * y2 - (x2 * y1), y1 * y2) 0):xt)

evalAddSub m (Operator '-': Val (x1,y1): xt) = evalAddSub m (Val(-x1,y1):xt)

evalAddSub _ ts = map (\(Val v) -> Value v) ts

evalTokens :: M.Map String Function -> [Token] -> [Value]
evalTokens m ts = map (\(Value(x, y)) -> Value (applyFactor (x, y) 0)) $ evalAddSub m $ evalMulDiv m (evalBrackets m ts) []

evalStr :: M.Map String Function -> String -> [Value]
evalStr m s = evalTokens m $ tokenize s []


tokenize :: String -> [Token] -> [Token]

tokenize [] ts = reverse ts

tokenize (' ':xs) ts = tokenize xs ts

tokenize (',':xs) ts = tokenize xs (Separator '.' : ts)

tokenize ('?':xs) ts = tokenize xs (Terminator '?' : ts)
tokenize ('!':xs) ts = tokenize xs (Terminator '!' : ts)
tokenize ('.':xs) ts = tokenize xs (Terminator '.' : ts)


tokenize ('(':xs) ts = tokenize xs (OpenBracket:ts)

tokenize (')':xs) ts = tokenize xs (CloseBracket:ts)

tokenize ('[':xs) ts = tokenize xs (OpenSquareBracket:ts)

tokenize (']':xs) ts = tokenize xs (CloseSquareBracket:ts)

tokenize (x:xt) ts
    | isDigit x = do
        let (ds, nds) = span isDigit (x:xt)
        let n = (read::String->Int) ds
        tokenize nds (Val (n,1): ts)
    | isOperator x =
        tokenize xt (Operator x:ts)
    | isAlpha x = 
        let (var, nds) = span isAlphaNum (x:xt)
        in tokenize nds (Var var: ts)

tokenize xs _ = error $ "Unexpected Tokens!" ++ show xs

calc :: Char -> Int -> Int -> Int
calc '+' x y = x + y
calc '-' x y = x - y
calc '*' x y = x * y
calc '/' x y = x `div` y
calc _ _ _ = error "Unexpected operator!"

name :: Function -> String
name (Function s _) = s

value :: Function -> FunctionValue
value (Function _ v) = v

readInt :: String -> Int
readInt s =
    readInt' s []
    where
        readInt' :: String -> String -> Int
        readInt' [] ys = (read::String->Int) $ reverse ys
        readInt' ['.'] ys = readInt' [] ys
        readInt' [','] ys = readInt' [] ys
        readInt' (x:xs) [] = readInt' xs [x]
        readInt' (x:xs) ys = readInt' xs (x:ys)

readValue :: String -> Value
readValue v =
    let (v1, v2) = break (== '/') v
    in if null v2
        then Value (readInt v, 1)
        else Value (readInt v1, readInt $ tail v2)

readFunctionValue :: M.Map String Function -> [String] -> FunctionValue
readFunctionValue m (c:ts) =
    let cnt = readInt $ init c
        cs = map ((\(Value(x, y)) -> Value (applyFactor (x, y) 0)) . head . evalStr m . init) ts
    in
        FunctionValue {count=cnt, val=last cs, coefs=init cs}
readFunctionValue _ _ = error "Error in readFunctionValue!"

printFunctionValue :: FunctionValue -> IO ()
printFunctionValue FunctionValue{count=0, coefs=[v]} = print v
printFunctionValue FunctionValue{count=_, coefs=vs} = putStrLn $ intercalate ", " $ map show vs


readAssignConstruct :: M.Map String Function -> [String] -> M.Map String Function
readAssignConstruct m [] = m
readAssignConstruct m [ve, "TO", vn] =  M.insert (init vn) (Function (init vn) (FunctionValue {count=0, val=head $ evalStr m ve, coefs=[]})) m
readAssignConstruct m (ve : "TO" : vn : "AND":xs) = 
    readAssignConstruct (M.insert vn (Function vn (FunctionValue {count=0, val=head $ evalStr m ve, coefs=[]})) m) xs

readAssignConstruct m xs =
    let (ts, ys) = span (/= "TO") xs
    in readAssignConstruct m (concat ts: ys)

-- readAssignConstruct _ _ = error "Error in readAssignConstruct!"

readDoConstruct :: M.Map String Function -> [String] -> M.Map String Function
readDoConstruct m [] = m
readDoConstruct m xs = 
    let (es, as) = break (== "ASSIGN") xs
        Value (n, 1) = head $ evalStr m $ tail $ init $ concat es
    in foldl (\m' _ -> readAssignConstruct m' (tail as)) m [1..n]

callFunction :: Function -> [Value] -> [Value]
callFunction (Function _ (FunctionValue 0 v [])) _ = [v]
callFunction (Function _ (FunctionValue n v vs)) args =
    let fvs = take (length args) vs
        vs' = zipWith (operate '*') args fvs
        addValue (Value(x1, y1)) (Value(x2, y2)) = Value $ applyFactor (x1 * y2 + (x2 * y1), y1 * y2) 0
        fv = foldl1 addValue vs'
    in 
        if length args == n then
            [addValue fv v]
        else drop (length args) vs ++ [addValue fv v]

readWhatConstruct :: M.Map String Function -> [String] -> IO ()

readWhatConstruct _ [] = return ()

readWhatConstruct m [x] = 
    let vs = evalStr m (init x)
    in 
        if length vs == 1 then print $ head vs
        else putStrLn $ intercalate ", " $ map show vs


readWhatConstruct m xs = do
    let (ys, zs) = span (/= "AND") xs

    let vs = evalStr m $ concat ys
    
    if length vs == 1 then print $ head vs
    else putStrLn $ intercalate ", " $ map show vs
        
    if null zs then return ()
    else readWhatConstruct m (tail zs)

addFunctions :: M.Map String Function -> String -> IO (M.Map String Function)
addFunctions m s =
    let ws = filter (/= []) $ words $ map toUpper s
    in case ws of
        ("WHAT" : "IS" : xs) -> do
            readWhatConstruct m xs
            return m
        (n:"IS":"FUNCTION":"OF":ts) -> 
            return $ M.insert n (Function n (readFunctionValue m ts)) m
        (n:"IS":v) -> 
            return $ M.insert n (Function n (FunctionValue 0 (head $ evalStr m (init $ unwords v)) [])) m
        ("ASSIGN": _) ->
            return $ readAssignConstruct m (tail ws)
        ("DO": _) -> 
            return $ readDoConstruct m (tail ws)        
        _ -> error $ "Unknown construct : " ++ s

process :: M.Map String Function -> IO ()
process m = do
    end <- isEOF
    if end then
        return ()
    else do
        l <- getLine
        if null l then
            process m
        else do
            fs <- addFunctions m l
            -- print fs
            process (foldl (\m' f -> M.insert (name f) f m') m fs)


main :: IO ()
main = process M.empty
