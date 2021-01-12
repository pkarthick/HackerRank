data Token = OpenBracket | CloseBracket | Num Integer | Operator Char deriving (Eq)

instance Show Token where
    show OpenBracket = "("
    show CloseBracket = ")"
    show (Num n) = show n
    show (Operator op) = [op]

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator _ = False


tokenize :: String -> [Token] -> [Token]
tokenize [] ts = reverse ts
tokenize (' ':xs) ts = tokenize xs ts

tokenize ('(':xs) ts = tokenize xs (OpenBracket : ts)
tokenize (')':xs) ts = tokenize xs (CloseBracket : ts)

tokenize (x:xs) ts 
    | isDigit x = 
        case span isDigit xs of
            ([], ys) -> tokenize ys (Num (read [x]):ts)
            (ns, ys) -> tokenize ys (Num (read (x:ns)):ts)
    | isOperator x = 
        tokenize xs (Operator x : ts)

tokenize xs _ = error $ "Unexpected error" ++ xs

calc :: Char -> Int -> Int -> Int
calc '+' x y = x + y
calc '-' x y = x - y
calc '*' x y = x * y
calc '/' x y = x `div` y
calc _ _ _ = error "Unexpected operator!"

p = 10 ^ 9 + 7

calcPow :: Integer -> Integer -> Integer
calcPow n 1 = n
calcPow n 2 = n * n `mod` p
calcPow n e =
    let halfe = e `div` 2
        half = calcPow n halfe `mod` p
    in 
        mod (if mod e 2 == 0 then half * half else n * half * half) p

calcMod :: Integer -> Integer
calcMod n =
    mod (calcPow n (p-2)) p

evalMulDiv :: [Token] -> [Token] -> [Token] 

evalMulDiv [] as = as
evalMulDiv (Num x:Operator '*':Num y:ts) as = evalMulDiv (Num (y*x `mod` p):ts) as
evalMulDiv (Num x:Operator '/':Num y:ts) as = 
    let (x', y') = if x < 0 && y < 0 then (-x, -y) else (x,y)
        modx' = if x' >= 0 then calcMod x' else - (calcMod (-x'))
    in if abs y' `mod` abs x' == 0
        then evalMulDiv (Num (y' `div` x'):ts) as
        else evalMulDiv (Num ((y' * modx') `mod` p):ts) as
evalMulDiv (t:ts) as = evalMulDiv ts (t:as)

evalPlusMinus :: [Token] -> Integer

evalPlusMinus [Num x] = x `mod` p
evalPlusMinus (Num x:Operator '+':Num y:ts) = evalPlusMinus (Num ((y+x) `mod` p):ts) 
evalPlusMinus (Num x:Operator '-':Num y:ts) = evalPlusMinus (Num ((y-x) `mod` p):ts) 
evalPlusMinus _ = error "Error in evalPlusMinus!"


evalUnary' :: [Token] -> [Token] -> [Token]
evalUnary' [] as = reverse as

evalUnary' (Operator '-':Num y:ts) [] = evalUnary' ts [Num (-y)]

evalUnary' (t:Operator '-':Operator '-':Num y:ts) as = evalUnary' ts (Operator '-':Num (-y):t:as)
evalUnary' (t:Operator '+':Operator '-':Num y:ts) as = evalUnary' ts (Operator '+':Num (-y):t:as)
evalUnary' (t:Operator '-':Operator '+':Num y:ts) as = evalUnary' ts (Operator '-':Num y:t:as)
evalUnary' (Operator op:Operator '-':Num y:ts) as = evalUnary' ts (Num (-y):Operator op:as)
evalUnary' (Operator op:Operator '+':Num y:ts) as = evalUnary' ts (Num y:Operator op:as)

evalUnary' (OpenBracket:Operator '-':Num y:ts) as = evalUnary' ts (Num (-y):OpenBracket:as)
evalUnary' (OpenBracket:Operator '+':Num y:ts) as = evalUnary' ts (Num y:OpenBracket:as)



evalUnary' (t:ts) as = evalUnary' ts (t:as)

evalUnary  :: [Token] -> [Token]
evalUnary ts = evalUnary' ts []

evalBrackets' :: [Token] -> [Token] -> [Token]

evalBrackets' [] as = reverse as

evalBrackets' (OpenBracket:ts) as = 
    let ts' = evalBrackets ts
    in evalBrackets' ts' as

evalBrackets' (CloseBracket:ts) as = 
    let r = eval $ reverse as
    in Num r:ts 

evalBrackets' (t:ts) as = evalBrackets' ts (t:as)

evalBrackets  :: [Token] -> [Token]
evalBrackets ts = evalBrackets' ts []

eval :: [Token] -> Integer
eval ts = 
    let ts' = evalMulDiv (reverse ts) []
        res = evalPlusMinus $ reverse ts'
    in if res > 0 then res `mod` p else if (-res) > p then -((-res) `mod` p) else p - (-1 * res) `mod` p

main :: IO()
main = do
    ex <- getLine
    let uts = evalUnary $ tokenize ex []
    let bts = evalBrackets uts
    let res = eval bts
    print res
