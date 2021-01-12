
module WhileLanguage 

open System

type BinOp = 
    | Add | Sub | Mul | Div

type RelOp = 
    | LessThan | GreatherThan

type BoolOp = 
    | And | Or

type Token = 
    | BoolLit of bool | Var of string | Num of int | BinOp of BinOp | RelOp of RelOp | BoolOp of BoolOp

type BinExp = 
    | Const of int
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
        | [] -> (ys, acc)
        | y :: yt when pred y -> dospan yt (y::acc)
        | _ -> (ys, acc)
    dospan xs []
    
let rec tokenize xs acc =
    match xs with
    | [] -> acc
    | ' ' :: xt -> tokenize xt acc
    | '+' :: xt -> tokenize xt (BinOp Add :: acc)
    | '-' :: xt -> tokenize xt (BinOp Sub :: acc)
    | '*' :: xt -> tokenize xt (BinOp Mul :: acc)
    | '/' :: xt -> tokenize xt (BinOp Div :: acc)
    | '<' :: xt -> tokenize xt (RelOp LessThan :: acc)
    | '>' :: xt -> tokenize xt (RelOp GreatherThan :: acc)
    | x :: xt  when Char.IsDigit x ->
        let n = List.takeWhile Char.IsDigit xs |> Convert.ToString |> int
        let nds = List.skipWhile Char.IsDigit xs 
        tokenize nds (Num n :: acc)
    | x :: xt  when Char.IsLetter x ->
        let (ds, nds) = span Char.IsLetterOrDigit xs
        let var = ds.ToString()
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

let rec createBinExp = function 
    | [Num n] -> Const n
    | [Var v] -> Variable v
    | (Num l :: BinOp Mul :: Num r :: ts) -> createBinExp (Num (l*r) :: ts)
    | (Num l :: BinOp Div :: Num r :: ts) -> createBinExp (Num (l / r) :: ts)
    | (Num l :: BinOp op :: Num r :: ts) -> BinExp (Const l, op, (createBinExp (Num r :: ts)))
    | (Var lv :: BinOp op :: [Var rv]) -> BinExp (Variable lv, op, Variable rv)
    | (Var lv :: BinOp Mul :: Var rv :: BinOp op2 :: ts) -> BinExp (BinExp (Variable lv, Mul, Variable rv), op2, createBinExp ts)
    | (Var lv :: BinOp Div :: Var rv :: BinOp op2 :: ts) -> BinExp (BinExp (Variable lv, Div, Variable rv), op2, createBinExp ts)
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
    let xs = List.takeWhile (fun t -> t = RelOp LessThan || t = RelOp GreatherThan) ts
    let ys = List.skipWhile (fun t -> t = RelOp LessThan || t = RelOp GreatherThan) ts

    match ys with
    | [] -> True
    | RelOp relop :: ys -> RelExp (createBinExp xs, relop, createBinExp ys)
    | _ -> raise (Exception "Unexpected error in createRelExp!")

let rec createBoolExp (ts:Token list) = 
    let xs = List.takeWhile (fun t -> t = BoolOp And || t = BoolOp Or) ts
    let ys = List.skipWhile (fun t -> t = BoolOp And || t = BoolOp Or) ts
    
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
    | [] -> (acc, [])
    | (')'::ls) -> 
        (acc
        |> List.skipWhile (fun c -> c = ' ')
        |> List.rev
        |> List.skipWhile (fun c -> c = ' ')
        , ls)
    | ch :: ls -> readUntil ls (ch::acc)

let parseCode acc ls = []
let evaluateConstruct (map:Map<string, int>) cs = map

let public main () =
    let rec readLines (acc: string list) =
        let s = Console.ReadLine()
        if isNull s then 
            acc 
            |> List.rev
            |> List.reduce (+)
        else 
            readLines (s :: acc)

    readLines []
    |> parseCode []
    |> evaluateConstruct (Map.empty<string, int>) 
    |> Map.iter (fun k v -> printf "%s %d" k v) 