import System.IO
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Maybe

newtype Statement = Statement String deriving (Eq, Show)

data BinOp = ADD | SUB | MUL | DIV deriving (Eq, Show)
data RelOp = LESSTHAN | GREATERTHAN deriving (Eq, Show)
data BoolOp = AND | OR deriving (Eq, Show)

data Token = 
    BoolLit Bool 
    | Var String | Num Integer 
    | BinOp BinOp | RelOp RelOp | BoolOp BoolOp 
    | OpenBracket | CloseBracket 
    deriving (Eq, Show)

data BinExp = 
    Const Integer
    | Variable String
    | BinExp BinExp BinOp BinExp
    deriving (Eq, Show)

data BoolExp =
    BTrue | BFalse | RelExp BinExp RelOp BinExp | Compound BoolExp BoolOp BoolExp
    deriving (Eq, Show)

data Construct =
    Assignment String BinExp
    | While BoolExp [Construct]
    | If BoolExp [Construct] [Construct]
    deriving (Eq, Show)

trimLeadingSpaces :: String -> String
trimLeadingSpaces [] = []
trimLeadingSpaces (' ':xs) = trimLeadingSpaces xs
trimLeadingSpaces xs = xs

tokenize :: String -> [Token] -> [Token]

tokenize [] ts = reverse ts

tokenize (' ':xt) ts = tokenize xt ts

tokenize ('(':xt) ts = tokenize xt (OpenBracket:ts)
tokenize (')':xt) ts = tokenize xt (CloseBracket:ts)

tokenize ('+':xt) ts = tokenize xt (BinOp ADD:ts)
tokenize ('-':xt) ts = tokenize xt (BinOp SUB:ts)
tokenize ('*':xt) ts = tokenize xt (BinOp MUL:ts)
tokenize ('/':xt) ts = tokenize xt (BinOp DIV:ts)

tokenize ('<':xt) ts = tokenize xt (RelOp LESSTHAN:ts)
tokenize ('>':xt) ts = tokenize xt (RelOp GREATERTHAN:ts)

tokenize (x:xt) ts 
    | isDigit x =
        let (ds, nds) = span isDigit (x:xt)
            n = (read::String->Integer) ds
        in tokenize nds (Num n: ts)
    | isAlpha x = 
        let (var, nds) = span isAlphaNum (x:xt)
        in 
            if var == "true" then tokenize nds (BoolLit True : ts)
            else if var == "false" then tokenize nds (BoolLit False : ts)
            else if var == "and" then tokenize nds (BoolOp AND : ts)
            else if var == "or" then tokenize nds (BoolOp OR: ts)
            else tokenize nds (Var var: ts)

tokenize _ _ = error "Unexpected error in tokenize!"

getVarValue :: Map.Map String Integer -> String -> Integer
getVarValue m var = fromJust $ Map.lookup var m


getEnclosedTokens :: [Token] -> Int -> [Token] -> ([Token], [Token])

getEnclosedTokens [] _ acc = (reverse acc, [])
getEnclosedTokens (CloseBracket:ts) 0 acc = (reverse acc, ts)
getEnclosedTokens (CloseBracket:ts) c acc = getEnclosedTokens (CloseBracket:ts) (c-1) acc 
getEnclosedTokens (OpenBracket:ts) c acc = getEnclosedTokens (OpenBracket:ts) (c+1) acc 
getEnclosedTokens (t:ts) c acc = getEnclosedTokens ts c (t:acc)


createBinExp :: [Token] -> BinExp

createBinExp [Num l] = Const l

createBinExp [Var v] = Variable v

createBinExp (OpenBracket:ts) =
    let (enc, rest) = getEnclosedTokens ts 0 []
        bexp = createBinExp enc
    in 
        if null rest 
            then bexp
            else 
                let BinOp op1:rest' = rest
                in BinExp bexp op1 (createBinExp rest')

createBinExp (Num l : BinOp MUL : Num r : ts) = createBinExp (Num (l*r) : ts)
createBinExp (Num l : BinOp DIV : Num r : ts) = createBinExp (Num (l `div` r) : ts)

createBinExp (Num l : BinOp op : OpenBracket : ts) = 
    let (enc, rest) = getEnclosedTokens ts 0 []
        bexp = BinExp (Const l) op (createBinExp enc)
    in 
        if null rest 
            then bexp
            else 
                let BinOp op1:rest' = rest
                in BinExp bexp op1 (createBinExp rest')

createBinExp (Var v : BinOp op : OpenBracket : ts) = 
    let (enc, rest) = getEnclosedTokens ts 0 []
        brexp = createBinExp enc
    in 
        if null rest 
            then BinExp (Variable v) op brexp
            else 
                let BinOp op1:rest' = rest
                in 
                    if op1 == MUL || op1 == DIV 
                        then BinExp (Variable v) op (BinExp brexp op1 (createBinExp rest')) 
                        else BinExp (BinExp (Variable v) op brexp) op1 (createBinExp rest')

createBinExp (Num l : BinOp op : Num r : ts) = 
    BinExp (Const l) op (createBinExp (Num r : ts))

createBinExp (Var lv : BinOp op : [Var rv]) = 
    BinExp (Variable lv) op (Variable rv)

createBinExp (Var lv : BinOp MUL : Var rv : BinOp op2 : ts) = 
    BinExp (BinExp (Variable lv) MUL (Variable rv)) op2 (createBinExp ts)

createBinExp (Var lv : BinOp DIV : Var rv : BinOp op2 : ts) = 
    BinExp (BinExp (Variable lv) DIV (Variable rv)) op2 (createBinExp ts)

createBinExp (Var lv : BinOp MUL : Num r : BinOp op2 : ts) = 
    BinExp (BinExp (Variable lv) MUL (Const r)) op2 (createBinExp ts)

createBinExp (Var lv : BinOp DIV : Num r : BinOp op2 : ts) = 
    BinExp (BinExp (Variable lv) DIV (Const r)) op2 (createBinExp ts)

createBinExp (Var v : BinOp op : Num r : ts) = 
    BinExp (Variable v) op (createBinExp (Num r : ts))

createBinExp (Num l : BinOp op : Var v : ts) = 
    BinExp (Const l) op (createBinExp (Var v : ts))

createBinExp (Var lv : BinOp op : ts) = 
    BinExp (Variable lv) op (createBinExp ts)

createBinExp ts = 
    error $ "Error in parse tokens!" ++ concatMap show ts


evalBinExp :: Map.Map String Integer -> BinExp -> Integer

evalBinExp _ (Const l) = l

evalBinExp m (Variable v) = getVarValue m v

evalBinExp _ (BinExp (Const l) ADD (Const r)) = l + r
evalBinExp _ (BinExp (Const l) SUB (Const r)) = l - r
evalBinExp _ (BinExp (Const l) MUL (Const r)) = l * r
evalBinExp _ (BinExp (Const l) DIV (Const r)) = l `div` r

evalBinExp m (BinExp (Variable v) op (Const r)) = 
    let l = getVarValue m v
    in evalBinExp m (BinExp (Const l) op (Const r))

evalBinExp m (BinExp (Const l) op (Variable v)) = 
    let r = getVarValue m v
    in evalBinExp m (BinExp (Const l) op (Const r))

evalBinExp m (BinExp (Variable vl) op (Variable vr)) = 
    let l = getVarValue m vl
        r = getVarValue m vr
    in evalBinExp m (BinExp (Const l) op (Const r))

evalBinExp m (BinExp (Const l) op bexp@BinExp{}) = 
    let ev = evalBinExp m bexp
    in evalBinExp m (BinExp (Const l) op (Const ev))

evalBinExp m (BinExp (Variable v) op bexp@BinExp{}) = 
    let l = getVarValue m v
        ev = evalBinExp m bexp
    in evalBinExp m (BinExp (Const l) op (Const ev))

evalBinExp m (BinExp bexp@BinExp{} op (Const r)) = 
    let l = evalBinExp m bexp
    in evalBinExp m (BinExp (Const l) op (Const r))

evalBinExp m (BinExp bexp@BinExp{} op (Variable v)) = 
    let r = getVarValue m v
        ev = evalBinExp m bexp
    in evalBinExp m (BinExp (Const ev) op (Const r))

evalBinExp m (BinExp bexp1@BinExp{} op bexp2@BinExp{}) = 
    let l = evalBinExp m bexp1
        r = evalBinExp m bexp2
    in evalBinExp m (BinExp (Const l) op (Const r))

createBoolExp :: [Token] -> BoolExp

createBoolExp ts = 
    let (xs, ys) = break (\t -> t == BoolOp AND || t == BoolOp OR) ts
    in 
        if null ys 
            then createRelExp xs
            else Compound (createRelExp xs) (if head ys == BoolOp AND then AND else OR) (createBoolExp $ tail ys)

createRelExp :: [Token] -> BoolExp

createRelExp ts = 
    let (xs, RelOp op:ys) = break (\t -> t == RelOp LESSTHAN || t == RelOp GREATERTHAN) ts
    in RelExp (createBinExp xs) op (createBinExp ys)

evalBoolExp :: Map.Map String Integer -> BoolExp -> Bool
evalBoolExp _ BTrue = True
evalBoolExp _ BFalse = False

evalBoolExp m (RelExp bexp1 LESSTHAN bexp2) =
    evalBinExp m bexp1 < evalBinExp m bexp2

evalBoolExp m (RelExp bexp1 GREATERTHAN bexp2) =
    evalBinExp m bexp1 > evalBinExp m bexp2

evalBoolExp m (Compound bexp1 AND bexp2) =
    evalBoolExp m bexp1 && evalBoolExp m bexp2
    
evalBoolExp m (Compound bexp1 OR bexp2) =
    evalBoolExp m bexp1 || evalBoolExp m bexp2

readBlock :: Int -> String -> String -> (String, String)

readBlock  _ [] acc = (reverse acc, [])

readBlock cnt ('{':l) acc =
    readBlock (cnt+1) l ('{':acc)

readBlock 1 ('}':l) acc = (reverse acc, l)

readBlock c ('}':l) acc = 
    readBlock (c-1) l ('}':acc)
    
readBlock cnt (ch:l) acc =
    readBlock cnt l (ch:acc)

readUntil :: String -> String -> (String, String)
readUntil [] acc = (acc, [])
readUntil (')':l) acc = (trimLeadingSpaces $ reverse $ trimLeadingSpaces acc, l)
readUntil (ch:l) [] = readUntil l [ch]
readUntil (ch:l) acc = readUntil l (ch:acc)


getStatements :: String -> String -> [Construct] -> [Construct] 
getStatements [] [] ss = reverse ss
getStatements [] acc ss = 
    let [con] = parseCode (reverse $ trimLeadingSpaces acc) []
    in reverse (con:ss)
    
getStatements (';':xs) acc ss = 
    let [con] = parseCode (reverse $ trimLeadingSpaces acc) []
    in getStatements xs [] (con:ss)

getStatements (' ':xs) [] ss = getStatements xs [] ss
getStatements (ch:xs) acc ss = getStatements xs (ch:acc) ss

readStatementsBlock :: String -> ([Construct], String)
readStatementsBlock xs = 
    let (ss, xs') = readBlock 1 xs []
    in (parseCode ss [], xs')

getAssignment :: String -> String -> (String, String)

getAssignment (':':'=':xs) var = (trimLeadingSpaces $ reverse $ trimLeadingSpaces var, xs)

getAssignment (ch:xs) var = getAssignment xs (ch:var)

getAssignment [] _ = error "Unexpected assignment!"



parseCode :: String -> [Construct] -> [Construct]

parseCode [] acc = reverse acc

parseCode (' ': xt) acc = parseCode xt acc

parseCode ('\t': xt) acc = parseCode xt acc

parseCode (';': xt) acc = parseCode xt acc
    
parseCode ('w' : 'h' : 'i' : 'l' : 'e' : ' ' : xt) acc = 
    let (bexp, xt1) = readUntil (tail $ trimLeadingSpaces xt) []
        ('d':'o':xt2) = trimLeadingSpaces xt1
        ('{':xt3) = trimLeadingSpaces xt2
        (ss, xt4) = readStatementsBlock (trimLeadingSpaces xt3)
    in parseCode xt4 (While (createBoolExp (tokenize bexp [])) ss : acc)

parseCode ('i' : 'f' : ' ' : xt ) acc =
    let (bexp, xt1) = readUntil (tail $ trimLeadingSpaces xt) []
        ('t':'h':'e':'n':xt2) = trimLeadingSpaces xt1
        ('{':xt3) = trimLeadingSpaces xt2
        (ifss, xt4) = readStatementsBlock (trimLeadingSpaces xt3)
        ('e':'l':'s':'e':xt5) = trimLeadingSpaces xt4
        ('{':xt6) = trimLeadingSpaces xt5
        (elsess, xt7) = readStatementsBlock (trimLeadingSpaces xt6)
    in parseCode xt7 (If (createBoolExp (tokenize bexp [])) ifss elsess:acc)

parseCode l acc =
    let (xs, l') = break (== ';') l
        (var, expr) = getAssignment xs []
        ex = createBinExp (tokenize expr [])
    in parseCode l' (Assignment var ex: acc)        


evaluateConstructs :: Map.Map String Integer -> [Construct] -> Map.Map String Integer

evaluateConstructs m [] = m

evaluateConstructs m (Assignment var bexp : cs) =
    let ev = evalBinExp m bexp
    in evaluateConstructs (Map.insert var ev m) cs

evaluateConstructs m (While bexp wcs : cs) =
    if evalBoolExp m bexp 
        then evaluateConstructs (evaluateConstructs m wcs) (While bexp wcs : cs)
        else evaluateConstructs m cs

evaluateConstructs m (If bexp ifcs elsecs : cs) =
    evaluateConstructs 
        (if evalBoolExp m bexp 
            then evaluateConstructs m ifcs
            else evaluateConstructs m elsecs) cs

readAll :: IO String
readAll = do 
    done <- isEOF
    if done
        then return []
        else do inp <- getChar
                (inp :) <$> readAll

printKeyValue :: (String, Integer) -> IO()
printKeyValue (k,v) = do
    putStr k 
    putChar ' '
    print v

main :: IO ()

main = do
    ls <- readAll
    let cs = parseCode (concat $ lines ls) []
    let m = evaluateConstructs Map.empty cs
    mapM_ printKeyValue $ Map.toAscList m
    
