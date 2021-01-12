{-# LANGUAGE PatternSynonyms #-}
module Simplify (simplifyExpression) where

import Control.Monad

data Operand =
    Literal Int
    | Term Int Int
    deriving (Eq, Show)

data Operator = Add | Minus | Multiply | Divide deriving (Eq, Show)

data Token =
    Value Operand
    | Op Operator
    | Open
    | Close
    | Space
    deriving (Eq, Show)

data Expression = 
    Poly Int Int Int Int Int Int
    | Binary Operator Expression Expression
    | Postfix Operator Expression
    | Bracket Expression Expression
    deriving (Eq, Show)

isOper :: String -> Bool
isOper "+" = True
isOper "-" = True
isOper "*" = True
isOper "/" = True
isOper _ = False

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

getOperand :: String -> Operand
getOperand s 
    | null s = Literal 0
    | s == "x" = Term 1 1
    | otherwise =
        case span isDigit s of
            ([],[]) -> Literal 0
            (cs, []) -> Literal (coef cs)
            ([], es) -> let e' = expo es in if e' == 0 then Term 0 1 else Term (expo es) 1
            (cs, es) -> let e' = expo es in if e' == 0 then Term 0 1 else Term (expo es) (coef cs)
        where
            coef cs = read cs :: Int
            expo es = case dropWhile (not . isDigit) es of
                        [] -> if null es then 0 else 1
                        es' -> read es' :: Int

pattern Zero :: Expression
pattern Zero = Poly 0 0 0 0 0 0

getPoly :: Int -> Int -> Expression
getPoly expo coef = 
    case expo of
        0 -> Poly coef 0 0 0 0 0 
        1 -> Poly 0 coef 0 0 0 0 
        2 -> Poly 0 0 coef 0 0 0
        3 -> Poly 0 0 0 coef 0 0 
        4 -> Poly 0 0 0 0 coef 0 
        5 -> Poly 0 0 0 0 0 coef
        _ -> Zero

getExpression :: Operand -> Expression
getExpression (Literal lit) = getPoly 0 lit
getExpression (Term coef expo) = getPoly coef expo

getOperator :: Char -> Operator
getOperator '+' = Add
getOperator '-' = Minus
getOperator '*' = Multiply
getOperator '/' = Divide

getToken :: Char -> Token
getToken ' ' = Space
getToken '(' = Open
getToken ')' = Close
getToken s | isOper [s] = Op $ getOperator s


getTokens :: String -> [Token]
getTokens = tokenize [] []

appendToken :: String -> Token -> [Token] -> [Token]
appendToken [] Space tokens = tokens 
appendToken acc Space tokens = Value (getOperand (reverse acc)) : tokens
appendToken [] token tokens = token : tokens 
appendToken acc token tokens = token : Value (getOperand (reverse acc)) : tokens

tokenize :: String -> [Token] -> String ->  [Token]

tokenize [] tokens [] = reverse tokens
tokenize acc tokens [] = reverse (Value (getOperand (reverse acc)) : tokens)

tokenize acc tokens (h:t) 
    | isDigit h || h == 'x' || h == '^' = tokenize (h:acc) tokens t
    | otherwise = tokenize [] (appendToken acc (getToken h) tokens) t

evaluateBinary :: Expression -> Expression
evaluateBinary (Binary Add expr1 expr2) = add expr1 expr2
evaluateBinary (Binary Minus expr1 expr2) = minus expr1 expr2
evaluateBinary (Binary Multiply expr1 expr2) = multiply expr1 expr2
evaluateBinary (Binary Divide expr1 expr2) = divide expr1 expr2

add :: Expression -> Expression -> Expression
add (Poly a0 a1 a2 a3 a4 a5) Zero = Poly a0 a1 a2 a3 a4 a5
add Zero (Poly a0 a1 a2 a3 a4 a5) = Poly a0 a1 a2 a3 a4 a5
add p@Poly{} (Postfix op expr) = evaluateBinary $ Binary op expr p
add pf@Postfix{} p@Poly{} = add p pf
add (Poly a0 a1 a2 a3 a4 a5) (Poly b0 b1 b2 b3 b4 b5) = Poly (a0+b0) (a1+b1) (a2+b2) (a3+b3) (a4+b4) (a5+b5)

minus :: Expression -> Expression -> Expression
minus (Poly a0 a1 a2 a3 a4 a5) (Poly b0 b1 b2 b3 b4 b5) = Poly (a0-b0) (a1-b1) (a2-b2) (a3-b3) (a4-b4) (a5-b5)

divide :: Expression -> Expression -> Expression
divide (Poly a0 a1 a2 a3 a4 a5) (Poly b0 _ _ _ _ _) = Poly (a0 `div` b0) (a1 `div` b0) (a2 `div` b0) (a3 `div` b0) (a4 `div` b0) (a5 `div` b0)

multiply :: Expression -> Expression -> Expression
multiply (Poly a0 a1 a2 a3 a4 a5) (Poly b0 b1 b2 b3 b4 b5) =
    foldl1 add [pr0, pr1, pr2, pr3, pr4, pr5]
    where
        pr0 = Poly (a0*b0) (a1*b0) (a2*b0) (a3*b0) (a4*b0) (a5*b0)
        pr1 = Poly 0 (a0*b1) (a1*b1) (a2*b1) (a3*b1) (a4*b1) 
        pr2 = Poly 0 0 (a0*b2) (a1*b2) (a2*b2) (a3*b2) 
        pr3 = Poly 0 0 0 (a0*b3) (a1*b3) (a2*b3) 
        pr4 = Poly 0 0 0 0 (a0*b4) (a1*b4) 
        pr5 = Poly 0 0 0 0 0 (a0*b5) 

getString :: Int -> Int -> String
getString _ 0 = ""
getString 0 c = (if c < 0 then " - " else " + ") ++ show (abs c)
getString 1 1 = "x"
getString 1 (-1) = "-x"
getString 1 c = (if c < 0 then " - " else " + ") ++ show (abs c) ++ "x"
getString e c 
    | c == 0 = ""
    | c == -1 = " - " ++ "x^" ++ show e
    | c == 1 = " + " ++ "x^" ++ show e
    | c < 0 = " - " ++ termStr
    | c > 0 = " + " ++ termStr
    where 
        termStr = show (abs c) ++ "x^" ++ show e
getString _ _ = ""

formatExpression :: Expression -> String
formatExpression (Poly a0 a1 a2 a3 a4 a5) = 
    if head str == '-' then 
        '-' : dropWhile (==' ') (tail str)
    else
        str
        where
            exprStr = concat [getString 5 a5, getString 4 a4, getString 3 a3, getString 2 a2, getString 1 a1, getString 0 a0]
            str = dropWhile (`elem` [' ', '+']) exprStr

formatExpression _ = ""


simplify p@Poly{} (Postfix op q@Poly{}) = evaluateBinary $ Binary op q p
simplify p@Poly{} (Postfix op (Bracket prev q)) = Bracket prev (evaluateBinary $ Binary op q p)
simplify p@Poly{} (Bracket prev curr) = Bracket prev $ simplify p curr
simplify p@Poly{} (Postfix pfop (Binary binop q@Poly{} r@Poly{})) = evaluateBinary $ Binary pfop (evaluateBinary $ Binary binop q r) p
simplify p@Poly{} (Postfix pfop (Binary binop expr q@Poly{})) = simplify (evaluateBinary $ Binary pfop q p) (Postfix binop expr)  

reduceClosing :: Expression -> Expression
reduceClosing expr =
    case expr of


        Bracket (Postfix Multiply q@Poly{}) p@Poly{} -> multiply q p

        Bracket (Postfix Divide q@Poly{}) p@Poly{} -> divide q p

        Bracket (Postfix Minus Zero) p@Poly{} -> minus Zero p

        Bracket (Postfix Multiply (Binary binop r@Poly{} q@Poly{})) p@Poly{} ->
            Binary binop r (multiply q p)

        Bracket (Postfix Divide (Binary binop r@Poly{} q@Poly{})) p@Poly{} ->
            Binary binop r (divide q p)
        
        Bracket (Postfix Multiply (Bracket pf@(Postfix _ _) q@Poly{})) p@Poly{} -> 
            Bracket pf (multiply q p)
        
        Bracket (Postfix Divide (Bracket pf@(Postfix _ _) q@Poly{})) p@Poly{} -> 
            Bracket pf (divide q p)
        
        Binary op q@Poly{} p@Poly{} -> evaluateBinary $ Binary op q p

        Binary op (Bracket Zero q@Poly{}) p@Poly{} ->
            evaluateBinary $ Binary op q p

        Binary Add (Bracket (Bracket expr1 Zero) q@Poly{}) p@Poly{} ->
            Bracket expr1 (add q p)

        Binary Minus (Bracket (Bracket expr1 Zero) q@Poly{}) p@Poly{} ->
            Bracket expr1 (minus q p)

        Binary op (Bracket (Postfix pfop expr1) q@Poly{}) p@Poly{} ->
            reduceClosing $ Bracket (Postfix pfop expr1) (evaluateBinary $ Binary op q p)

        Binary op (Binary inop expr1 q@Poly{}) p@Poly{} ->
            reduceClosing $ Binary inop expr1 (evaluateBinary $ Binary op q p)

        _ -> expr

optimize :: [Token] -> Expression -> IO Expression
optimize [] p@Poly{} = return p

optimize [] (Binary op q@Poly{} p@Poly{}) = optimize [] $ evaluateBinary $ Binary op q p

optimize [] (Binary op (Bracket expr q@Poly{}) p@Poly{}) = 
    optimize [] $ Bracket expr (evaluateBinary $ Binary op q p)

optimize [] (Binary op (Binary inop expr q@Poly{}) p@Poly{}) = 
    optimize [] $ Binary inop expr $ evaluateBinary $ Binary op q p

optimize tokens (Postfix Add (Binary op (Binary inop expr q@Poly{}) p@Poly{})) = 
    optimize tokens $ Postfix Add (Binary inop expr $ evaluateBinary $ Binary op q p)

optimize tokens (Postfix Minus (Binary op (Binary inop expr q@Poly{}) p@Poly{})) = 
    optimize tokens $ Postfix Minus (Binary inop expr $ evaluateBinary $ Binary op q p)

-- Bracket Opens
optimize (Open:t) Zero = optimize t (Bracket Zero Zero)
optimize (Open:t) p@Poly{} = optimize (Op Multiply:Open:t) p
optimize (Open:t) bin@Binary{} = optimize (Op Multiply:Open:t) bin
optimize (Open:t) expr = optimize t (Bracket expr Zero)

-- Bracket closes
optimize (Close:t) expr = optimize t $ reduceClosing expr

optimize [] (Bracket prev curr) = optimize [] $ simplify curr prev

optimize (Op Minus:t) Zero = optimize t (Postfix Minus Zero)

optimize (Op operator:t) expr = optimize t (Postfix operator expr)

optimize (Value h:t) Zero = optimize t (getExpression h)

optimize (Value h:t) (Bracket Zero Zero) = optimize t (getExpression h)

optimize (Value h:t) (Postfix Minus Zero) = optimize t (minus Zero (getExpression h))

optimize (Value h:t) (Postfix Divide p@Poly{}) = optimize t (divide p (getExpression h))

optimize (Value h:t) (Postfix Multiply p@Poly{}) = optimize t (multiply p (getExpression h))

optimize (Value h:t) (Postfix Divide (Bracket expr1 p@Poly{})) = optimize t (Bracket expr1 (divide p (getExpression h)))

optimize (Value h:t) (Postfix Multiply (Bracket expr1 p@Poly{})) = optimize t (Bracket expr1 (multiply p (getExpression h)))

optimize (Value h:t) (Postfix op p@Poly{}) = optimize t (Binary op p (getExpression h))

optimize (Value h:t) (Postfix Multiply (Binary op p@Poly{} q@Poly{})) = optimize t (Binary op p (multiply q (getExpression h)))

optimize (Value h:t) (Postfix Divide (Binary op p@Poly{} q@Poly{})) = optimize t (Binary op p (divide q (getExpression h)))

optimize (Value h:t) (Postfix Add (Binary Minus p@Poly{} q@Poly{})) = optimize t (add (minus p q) (getExpression h))

optimize (Value h:t) (Postfix op bin@Binary{}) = optimize t (Binary op bin (getExpression h))

optimize (Value h:t) (Postfix Minus (Bracket (Postfix pfop expr1) q@Poly{})) = optimize t (Bracket (Postfix pfop expr1) (minus q (getExpression h)))

optimize (Value h:t) (Postfix op br@Bracket{}) = optimize t (Binary op br (getExpression h))

optimize (Value h:t) (Bracket prev Zero) = optimize t (Bracket prev (getExpression h))


simplifyExpression :: String -> IO String
simplifyExpression = (formatExpression <$>) . flip optimize Zero . getTokens


main :: IO ()
main = 
    do
        
        count <- readLn
        expressions <- replicateM count getLine
        -- simplified <- mapM ( (formatExpression <$>) . flip optimize (getPoly 0 0) . getTokens) expressions
        simplified <- mapM simplifyExpression expressions
        mapM_ putStrLn simplified

