// Learn more about F# at http://fsharp.org

open System

type BinOp = 
    | Add | Sub | Mul | Div

type RelOp = 
    | LessThan | GreatherThan

type BoolOp = 
    | And | Or

type Token = 
    | BoolLit of bool | Var of string | Num of int64 | BinOp of BinOp | RelOp of RelOp | BoolOp of BoolOp | OpenBracket | CloseBracket

type BinExp = 
    | Const of int64
    | Variable of string
    | BinExp of BinExp * BinOp * BinExp

type BoolExp =
    | True
    | False
    | RelExp of BinExp * RelOp * BinExp
    | Compound of BoolExp * BoolOp * BoolExp

type Construct =
    | Assignment of string * BinExp
    | While of BoolExp * Construct list
    | If of BoolExp * Construct list * Construct list

let span pred xs =
    let rec dospan ys acc =
        match ys with
        | [] -> (List.rev acc, ys)
        | y :: yt when pred y -> dospan yt (y::acc)
        | _ -> (List.rev acc, ys)
    dospan xs []

let splitWhen pred xs =
    let rec dobreak ys acc =
        match ys with
        | [] -> (List.rev acc, [])
        | y :: yt when not (pred y) -> dobreak yt (y::acc)
        | _ -> (List.rev acc, ys)
    dobreak xs []

let rec trimLeadingSpaces = function
    | ' ' :: chs -> trimLeadingSpaces chs
    | chs -> chs
    
let getString (chs: char list) =
    chs |> List.toArray |> System.String

let rec tokenize xs acc =
    match xs with
    | [] -> List.rev acc
    | ' ' :: xt -> tokenize xt acc

    | '(' :: xt -> tokenize xt (OpenBracket :: acc)
    | ')' :: xt -> tokenize xt (CloseBracket :: acc)

    | '+' :: xt -> tokenize xt (BinOp Add :: acc)
    | '-' :: xt -> tokenize xt (BinOp Sub :: acc)
    | '*' :: xt -> tokenize xt (BinOp Mul :: acc)
    | '/' :: xt -> tokenize xt (BinOp Div :: acc)
    | '<' :: xt -> tokenize xt (RelOp LessThan :: acc)
    | '>' :: xt -> tokenize xt (RelOp GreatherThan :: acc)
    | x :: xt  when Char.IsDigit x ->
        let n = List.takeWhile Char.IsDigit xs |> getString |> int64
        let nds = List.skipWhile Char.IsDigit xs 
        tokenize nds (Num n :: acc)
    | x :: xt  when Char.IsLetter x ->
        let (ds, nds) = span Char.IsLetter xs
        let var = getString ds
        match var with
        // | ['t';'r';'u';'e'] -> tokenize nds (BoolLit true :: acc)
        // | ['f';'a';'l';'s';'e'] -> tokenize nds (BoolLit false :: acc)
        // | ['a';'n';'d'] ->  tokenize nds (BoolOp And :: acc)
        // | ['o';'r';] ->  tokenize nds (BoolOp Or :: acc)
        | "true" -> tokenize nds (BoolLit true :: acc)
        | "false" -> tokenize nds (BoolLit false :: acc)
        | "and" ->  tokenize nds (BoolOp And :: acc)
        | "or" ->  tokenize nds (BoolOp Or :: acc)
        | _ -> tokenize nds (Var var :: acc)
    | _ -> raise (Exception "Unexpected error in tokenize!")

let rec getVarValue var (varmap:Map<string, int>) =
    Map.find var varmap

let rec getEnclosedTokens ts cnt acc =
    match ts, cnt with
    | ([], _) -> (List.rev acc, [])
    | (CloseBracket :: ts, 0) -> (List.rev acc, ts)
    | (CloseBracket :: ts, c) -> getEnclosedTokens (CloseBracket::ts) (c-1) acc 
    | (OpenBracket::ts, c) -> getEnclosedTokens (OpenBracket::ts) (c+1) acc 
    | (t::tt, c) -> getEnclosedTokens tt c (t::acc)

let rec createBinExp = function 
    | [Num n] -> Const n
    | [Var v] -> Variable v
    | OpenBracket :: ts ->
        let (enc, rest) = getEnclosedTokens ts 0 []
        let bexp = createBinExp enc
    
        match rest with
        | [] -> bexp
        | BinOp op1::rest' -> BinExp (bexp, op1, (createBinExp rest'))
        | _ -> raise (Exception "Error in createBinExp!")

    | (Num l :: BinOp Mul :: Num r :: ts) -> createBinExp (Num (l*r) :: ts)
    | (Num l :: BinOp Div :: Num r :: ts) -> createBinExp (Num (l / r) :: ts)

    | (Num l :: BinOp op :: OpenBracket :: ts) ->
        let (enc, rest) = getEnclosedTokens ts 0 []
        let bexp = BinExp (Const l, op, createBinExp enc)
       
        match rest with
        | [] -> bexp
        | BinOp op1::rest' -> BinExp (bexp, op1, (createBinExp rest'))
        | _ -> raise (Exception "Error in createBinExp!")

    | (Var v :: BinOp op :: OpenBracket :: ts) ->
        let (enc, rest) = getEnclosedTokens ts 0 []
        let brexp = createBinExp enc
   
        match rest with
        | [] -> BinExp (Variable v, op, brexp)
        | BinOp op1::rest' ->
            if op1 = Mul || op1 = Div
                then BinExp (Variable v, op, BinExp (brexp, op1, createBinExp rest')) 
                else BinExp (BinExp (Variable v, op, brexp), op1, createBinExp rest')        
        | _ -> raise (Exception "Error in createBinExp!")

    | (Num l :: BinOp op :: Num r :: ts) -> BinExp (Const l, op, (createBinExp (Num r :: ts)))
    | (Var lv :: BinOp op :: [Var rv]) -> BinExp (Variable lv, op, Variable rv)
    | (Var lv :: BinOp Mul :: Var rv :: BinOp op2 :: ts) -> BinExp (BinExp (Variable lv, Mul, Variable rv), op2, createBinExp ts)
    | (Var lv :: BinOp Div :: Var rv :: BinOp op2 :: ts) -> BinExp (BinExp (Variable lv, Div, Variable rv), op2, createBinExp ts)
    | (Var lv :: BinOp Mul :: Num r :: BinOp op2 :: ts) -> BinExp (BinExp (Variable lv, Mul, Const r), op2, createBinExp ts)
    | (Var lv :: BinOp Div :: Num r :: BinOp op2 :: ts) -> BinExp (BinExp (Variable lv, Div, Const r), op2, createBinExp ts)
    | (Var v :: BinOp op :: Num r :: ts) -> BinExp (Variable v, op, createBinExp (Num r :: ts))
    | (Num l :: BinOp op :: Var v :: ts) -> BinExp (Const l, op, createBinExp (Var v :: ts))
    | (Var lv :: BinOp op :: ts) -> BinExp (Variable lv, op, createBinExp ts)
    | _ -> raise (Exception "Unexpected error in createBinExp!")

let rec evalBinExp map exp =
    match exp with 
    | Const l -> l
    | Variable v -> Map.find v map
    | BinExp (Const l, Add, Const r) -> l + r
    | BinExp (Const l, Sub, Const r) -> l - r
    | BinExp (Const l, Mul, Const r) -> l * r
    | BinExp (Const l, Div, Const r) -> l / r
    | BinExp (Variable v, op, Const r) -> evalBinExp map (BinExp (Const (Map.find v map), op, Const r))
    | BinExp (Variable vl, op, Variable vr) -> evalBinExp map (BinExp (Const (Map.find vl map), op, Const (Map.find vr map)))
    | BinExp (Const l, op, bexp & BinExp(_)) -> 
        let r = evalBinExp map bexp
        evalBinExp map (BinExp (Const l, op, Const r))
    | BinExp (Variable v, op, bexp & BinExp(_)) -> 
        let l = Map.find v map
        let r = evalBinExp map bexp
        evalBinExp map (BinExp (Const l, op, Const r))
    | BinExp (bexp & BinExp(_), op, Const r) -> 
        let l = evalBinExp map bexp
        evalBinExp map (BinExp (Const l, op, Const r))
    | BinExp (bexp & BinExp(_), op, Variable v) -> 
        let l = evalBinExp map bexp
        let r = Map.find v map
        evalBinExp map (BinExp (Const l, op, Const r))
    | BinExp (bexp1 & BinExp(_), op, bexp2 & BinExp(_)) -> 
        let l = evalBinExp map bexp1
        let r = evalBinExp map bexp2
        evalBinExp map (BinExp (Const l, op, Const r))
    | _ -> raise (Exception "Unexpected error in evalBinExp!")

let createRelExp (ts:Token list) = 
    let xs = List.takeWhile (fun t -> t <> RelOp LessThan && t <> RelOp GreatherThan) ts
    let ys = List.skipWhile (fun t -> t <> RelOp LessThan && t <> RelOp GreatherThan) ts

    match ys with
    | [] -> True
    | RelOp relop :: ys -> RelExp (createBinExp xs, relop, createBinExp ys)
    | _ -> raise (Exception "Unexpected error in createRelExp!")

let rec createBoolExp (ts:Token list) = 
    let (xs, ys) = splitWhen (fun t -> t = BoolOp And || t = BoolOp Or) ts
    
    if List.isEmpty ys then createRelExp xs 
    else
        let bexp1 = createRelExp xs
        let bexp2 = createBoolExp (List.tail ys)
        let boolop = if List.head ys = BoolOp And then And else Or
        Compound (bexp1, boolop, bexp2)

let rec evalBoolExp map boolexp =
    match boolexp with 
    | True -> true
    | False -> false
    | RelExp (bexp1, LessThan, bexp2) -> evalBinExp map bexp1 < evalBinExp map bexp2
    | RelExp (bexp1, GreatherThan, bexp2) -> evalBinExp map bexp1 > evalBinExp map bexp2
    | Compound (bexp1, And, bexp2) -> evalBoolExp map bexp1 && evalBoolExp map bexp2
    | Compound (bexp1, Or, bexp2) -> evalBoolExp map bexp1 || evalBoolExp map bexp2

let rec readBlock cnt (s:char list) (acc:char list) =
    match (cnt, s) with
    | (_, []) -> (List.rev acc, [])
    | (_, '{'::ls) -> readBlock (cnt+1) ls ('{'::acc)
    | (1, ('}'::ls)) -> (List.rev acc, ls)
    | (_, ('}'::ls)) -> readBlock (cnt-1) ls ('}'::acc)
    | (_, (ch::ls)) -> readBlock cnt ls (ch::acc)

let rec readUntil s acc =
    match s with 
    | [] -> acc, []
    | (')'::ls) -> acc |> trimLeadingSpaces |> List.rev |> trimLeadingSpaces, ls
    | ch :: ls -> readUntil ls (ch::acc)

let rec parseCode (cs: Construct list) (code:char list) = 

    let rec getStatements (chs:char list) (acc:char list) (cons: Construct list) =
        match (chs, acc) with
        | ([], []) -> List.rev cons
        | ([], acc) -> 
            let cs = parseCode [] (acc |> trimLeadingSpaces |> List.rev) 
            List.rev (List.concat [cs; cons])

        | (';'::xs, acc) -> 
            let cs = parseCode [] (acc |> trimLeadingSpaces |> List.rev) 
            getStatements xs [] (List.concat [cs; cons])
        
        | (' '::xs, []) -> getStatements xs [] cons
        | (ch :: xs, acc) -> getStatements xs (ch::acc) cons

    let readStatementsBlock xs =
        let (ss, xs') = readBlock 1 xs []
        (parseCode [] ss, xs')

    let rec getAssignment (code:char list) (var:char list) =
    
        match code with
        | ':'::'='::xs -> (var |> trimLeadingSpaces |> List.rev |> trimLeadingSpaces, xs)
        | ch :: xs -> getAssignment xs (ch::var)
        | _ -> raise (Exception "Unexpected Assignment!")


    match code with
    | [] -> List.rev cs
    | ' '::xt -> parseCode cs xt
    | ';'::xt -> parseCode cs xt
    | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: ' ' :: xt -> 
        let (bexp, xt1) = readUntil (xt |> trimLeadingSpaces|> List.tail) []
        match trimLeadingSpaces xt1 with
        | 'd'::'o'::xt2 -> 
            match trimLeadingSpaces xt2 with
            | '{'::xt3 -> 
                let (ss, xt4) = readStatementsBlock (trimLeadingSpaces xt3)
                let ts = tokenize bexp []
                parseCode (While(createBoolExp ts, ss) :: cs) xt4
            | _ -> []
        | _ -> []

    | 'i' :: 'f' :: ' ' :: xt ->
        let (bexp, xt1) = readUntil (xt |> trimLeadingSpaces |> List.tail) []
        match trimLeadingSpaces xt1 with
        | 't'::'h'::'e'::'n'::xt2 ->
            match trimLeadingSpaces xt2 with
            | '{'::xt3 -> 
                let (ifss, xt4) = readStatementsBlock (trimLeadingSpaces xt3)
                match trimLeadingSpaces xt4 with
                | 'e'::'l'::'s'::'e'::xt5 ->
                    match trimLeadingSpaces xt5 with
                    | '{'::xt6 -> 
                        let (elsess, xt7) = readStatementsBlock (trimLeadingSpaces xt6)
                        parseCode (If (createBoolExp (tokenize bexp []), ifss, elsess)::cs) xt7   
                    | _ -> raise (Exception "Error in parse code!")
                | _ -> raise (Exception "Error in parse code!")
            | _ -> raise (Exception "Error in parse code!")
        | _ -> raise (Exception "Error in parse code!")
       
    | _ ->
        let (xs, code') = splitWhen (fun ch -> ch = ';') code
        let (var, expr) = getAssignment xs []
        let ex = createBinExp (tokenize expr [])
        parseCode (Assignment(getString var, ex) :: cs) code'


let rec evaluateConstructs (map:Map<string, int64>) cs = 
    match cs with
    | [] -> map
    | Assignment (var, bexp) :: cs' ->
        let ev = evalBinExp map bexp
        evaluateConstructs (Map.add var ev map) cs'
    | While (bexp, wcs) :: cs' ->
        if evalBoolExp map bexp then 
            evaluateConstructs (evaluateConstructs map wcs) (While (bexp, wcs) :: cs')
        else 
            evaluateConstructs map cs'
    | If (bexp, ifcs, elsecs) :: cs ->
        evaluateConstructs 
            (if evalBoolExp map bexp 
                then evaluateConstructs map ifcs
                else evaluateConstructs map elsecs) cs    


[<EntryPoint>]
let main argv =
    let rec readLines (acc: string list) =
        let s = Console.ReadLine()
        if isNull s then 
            acc 
            |> List.rev
            |> List.reduce (+)
            |> (fun s -> s.Trim().ToCharArray())
            |> List.ofArray
        else 
            readLines (s :: acc)

    let rec printKeyValues = function
        | [(k,v)] -> printf "%s %d" k v
        | (k,v) :: kvs -> 
            printfn "%s %d" k v
            printKeyValues kvs
        | [] -> ()

    readLines []
    |> parseCode []
    |> evaluateConstructs (Map.empty<string, int64>) 
    |> Map.toList
    |> List.sortBy (fst)
    |> printKeyValues
    
    0 // return an integer exit code
