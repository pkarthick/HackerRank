open Printf
open List

type binOp = 
    | Add | Sub | Mul | Div

type relOp = 
    | LessThan | GreatherThan

type boolOp = 
    | And | Or

type token = 
    | BoolLit of bool | Var of string | Num of int | BinOp of binOp | RelOp of relOp | BoolOp of boolOp | OpenBracket | CloseBracket

type binExp = 
    | Const of int
    | Variable of string
    | BinExp of binExp * binOp * binExp

type boolExp =
    | True
    | False
    | RelExp of binExp * relOp * binExp
    | Compound of boolExp * boolOp * boolExp

type construct =
    | Assignment of string * binExp
    | While of boolExp * construct list
    | If of boolExp * construct list * construct list

module MyVariables = Map.Make(String) 

type variable = string MyVariables.t

let rec take_while p lst = match lst with 
   [] -> []
  | x::xs -> if p x then x :: (take_while p xs) else []

let rec skip_while p lst = match lst with 
   [] -> []
  | x::xs -> if p x then skip_while p xs else x::xs

let span pred xs =
    let rec loop ys acc =
        match ys with
         [] -> (List.rev acc, ys)
        | y :: yt when pred y -> loop yt (y::acc)
        | _ -> (List.rev acc, ys)
    in loop xs []

let splitWhen pred xs =
    let rec dobreak ys acc =
        match ys with
         [] -> (List.rev acc, [])
        | y :: yt when not (pred y) -> dobreak yt (y::acc)
        | _ -> (List.rev acc, ys)
    in dobreak xs []

let rec startsWith xs ys =
    if xs = [] then true
    else if List.hd xs = List.hd ys then startsWith (List.tl xs) (List.tl ys)
    else false

let rec drop n xs =
    match n with
    0 -> xs
    | _ -> drop (n-1) (List.tl xs)

let explode s = 
    let rec loop acc l =
        match String.length s = l with
         true -> List.rev acc
        | false -> loop (s.[l]::acc) (l+1)
    in loop [] 0

let implode l =
  let result = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;

let rec trimLeadingSpaces = function
    | ' ' :: chs -> trimLeadingSpaces chs
    | chs -> chs
    
let is_alpha = function 'a' .. 'z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let rec tokenize xs acc =
    match xs with
      [] -> List.rev acc
    | ' ' :: xt -> tokenize xt acc

    | '(' :: xt -> tokenize xt (OpenBracket :: acc)
    | ')' :: xt -> tokenize xt (CloseBracket :: acc)

    | '+' :: xt -> tokenize xt (BinOp Add :: acc)
    | '-' :: xt -> tokenize xt (BinOp Sub :: acc)
    | '*' :: xt -> tokenize xt (BinOp Mul :: acc)
    | '/' :: xt -> tokenize xt (BinOp Div :: acc)
    | '<' :: xt -> tokenize xt (RelOp LessThan :: acc)
    | '>' :: xt -> tokenize xt (RelOp GreatherThan :: acc)
    | x :: xt  when is_digit x ->
        let n = take_while is_digit xs |> implode |> int_of_string
        and nds = skip_while is_digit xs 
        in tokenize nds (Num n :: acc)
    | x :: xt  when is_alpha x ->
        let (ds, nds) = span is_alpha xs
        in let var = implode ds
        in match var with
         "true" -> tokenize nds (BoolLit true :: acc)
        | "false" -> tokenize nds (BoolLit false :: acc)
        | "and" ->  tokenize nds (BoolOp And :: acc)
        | "or" ->  tokenize nds (BoolOp Or :: acc)
        | _ -> tokenize nds (Var var :: acc)
    | _ -> raise (Failure "Unexpected error in tokenize!")


let rec getEnclosedtokens ts cnt acc =
    match ts, cnt with
     ([], _) -> (List.rev acc, [])
    | (CloseBracket :: ts, 0) -> (List.rev acc, ts)
    | (CloseBracket :: ts, c) -> getEnclosedtokens (CloseBracket::ts) (c-1) acc 
    | (OpenBracket::ts, c) -> getEnclosedtokens (OpenBracket::ts) (c+1) acc 
    | (t::tt, c) -> getEnclosedtokens tt c (t::acc)

let rec createbinExp xs = 
    if startsWith [Num n] xs then 
        let Num n = List.hd xs 
        in Const n
    else if xs = [Var v] then 
        let Var v = List.hd xs
        in Variable v
    else if startsWith [OpenBracket] xs then
        let (enc, rest) = getEnclosedtokens (List. tl xs) 0 []
        in let bexp = createbinExp enc in
    
        match rest with
         [] -> bexp
        | BinOp op1::rest' -> BinExp (bexp, op1, (createbinExp rest'))
        | _ -> raise (Failure "Error in createbinExp!")

    else if startsWith [Num l :: BinOp Mul :: Num r] xs then
        createbinExp (Num (l*r) :: (drop 3 xs))
    else if startsWith [Num l :: BinOp Div :: Num r] xs then 
        createbinExp (Num (l / r) :: (drop 3 xs))
    else if startsWith [Num l :: BinOp op :: OpenBracket] xs then
        let (enc, rest) = getEnclosedtokens (drop 3 xs) 0 []
        in let bexp = BinExp (Const l, op, createbinExp enc) in
       
        match rest with
         [] -> bexp
        | BinOp op1::rest' -> BinExp (bexp, op1, (createbinExp rest'))
        | _ -> raise (Failure "Error in createbinExp!")

    else if startsWith [Var v :: BinOp op :: OpenBracket] xs then
        let (enc, rest) = getEnclosedtokens (drop 3 xs) 0 []
        and brexp = createbinExp enc in
   
        match rest with
         [] -> BinExp (Variable v, op, brexp)
        | BinOp op1::rest' ->
            if op1 = Mul || op1 = Div
                then BinExp (Variable v, op, BinExp (brexp, op1, createbinExp rest')) 
                else BinExp (BinExp (Variable v, op, brexp), op1, createbinExp rest')        
        | _ -> raise (Failure "Error in createbinExp!")

    else if startsWith [Num l :: BinOp op :: Num r] xs then
        BinExp (Const l, op, (createbinExp (Num r :: (drop 3 xs))))
    else if startsWith [Var lv :: BinOp op :: Var rv] xs then
        BinExp (Variable lv, op, Variable rv)
    else if startsWith [Var lv :: BinOp Mul :: Var rv :: BinOp op2] xs then
        BinExp (BinExp (Variable lv, Mul, Variable rv), op2, createbinExp (drop 4 xs))
    else if startsWith [Var lv :: BinOp Div :: Var rv :: BinOp op2] xs then
        BinExp (BinExp (Variable lv, Div, Variable rv), op2, createbinExp (drop 4 xs))
    else if startsWith [Var lv :: BinOp Mul :: Num r :: BinOp op2] xs then
        BinExp (BinExp (Variable lv, Mul, Const r), op2, createbinExp (drop 4 xs))
    else if startsWith [Var lv :: BinOp Div :: Num r :: BinOp op2] xs then
        BinExp (BinExp (Variable lv, Div, Const r), op2, createbinExp (drop 4 xs))
    else if startsWith [Var v :: BinOp op :: Num r] xs then
        BinExp (Variable v, op, createbinExp (Num r :: (drop 3 xs)))
    else if startsWith [Num l :: BinOp op :: Var v] xs then
        BinExp (Const l, op, createbinExp (Var v :: (drop 3 xs)))
    else if startsWith [Var lv :: BinOp op] xs then
        BinExp (Variable lv, op, createbinExp (drop 2 xs))
    else
        raise (Failure "Unexpected error in createbinExp!")


let findVar (v:string) (map:variable) =
    MyVariables.find v map |> int_of_string

let rec evalBinExp (map:variable) exp =
    match exp with 
     Const l -> l
    | Variable v -> findVar v map
    | BinExp (Const l, Add, Const r) -> l + r
    | BinExp (Const l, Sub, Const r) -> l - r
    | BinExp (Const l, Mul, Const r) -> l * r
    | BinExp (Const l, Div, Const r) -> l / r
    | BinExp (Variable v, op, Const r) -> evalBinExp map (BinExp (Const (findVar v map), op, Const r))
    | BinExp (Variable vl, op, Variable vr) -> evalBinExp map (BinExp (Const (findVar vl map), op, Const (findVar vr map)))
    | BinExp (Const l, op, BinExp(bexp1,bop,bexp2)) -> 
        let r = evalBinExp map (BinExp(bexp1,bop,bexp2))
        in evalBinExp map (BinExp (Const l, op, Const r))
    | BinExp (Variable v, op, BinExp(bexp1,bop,bexp2)) -> 
        let l = findVar v map
        and r = evalBinExp map (BinExp(bexp1,bop,bexp2)) in
        evalBinExp map (BinExp (Const l, op, Const r))
    | BinExp (BinExp(bexp1,bop,bexp2), op, Const r) -> 
        let l = evalBinExp map (BinExp(bexp1,bop,bexp2))
        in evalBinExp map (BinExp (Const l, op, Const r))
    | BinExp (BinExp(bexp1,bop,bexp2), op, Variable v) -> 
        let l = evalBinExp map (BinExp(bexp1,bop,bexp2))
        and  r = findVar v map
        in evalBinExp map (BinExp (Const l, op, Const r))
    | BinExp (BinExp(bexp1,bop,bexp2), op, BinExp(bexp21,bop2,bexp22)) -> 
        let l = evalBinExp map (BinExp(bexp1,bop,bexp2))
        and r = evalBinExp map (BinExp(bexp21,bop2,bexp22))
        in evalBinExp map (BinExp (Const l, op, Const r))
    | _ -> raise (Failure "Unexpected error in evalbinExp!")

let createRelExp (ts:token list) = 
    let xs = take_while (fun t -> t <> RelOp LessThan && t <> RelOp GreatherThan) ts
    and ys = skip_while (fun t -> t <> RelOp LessThan && t <> RelOp GreatherThan) ts

    in match ys with
    | [] -> True
    | RelOp relop :: ys -> RelExp (createbinExp xs, relop, createbinExp ys)
    | _ -> raise (Failure "Unexpected error in createRelExp!")

let rec createboolExp (ts:token list) = 
    let (xs, ys) = splitWhen (fun t -> t = BoolOp And || t = BoolOp Or) ts
    
    in 
    match ys with
     [] -> createRelExp xs 
    | y :: yt ->
        let bexp1 = createRelExp xs
        and bexp2 = createboolExp yt
        and boolop = if y = BoolOp And then And else Or
        in Compound (bexp1, boolop, bexp2)

let rec evalBoolExp map boolexp =
    match boolexp with 
     True -> true
    | False -> false
    | RelExp (bexp1, LessThan, bexp2) -> evalBinExp map bexp1 < evalBinExp map bexp2
    | RelExp (bexp1, GreatherThan, bexp2) -> evalBinExp map bexp1 > evalBinExp map bexp2
    | Compound (bexp1, And, bexp2) -> evalBoolExp map bexp1 && evalBoolExp map bexp2
    | Compound (bexp1, Or, bexp2) -> evalBoolExp map bexp1 || evalBoolExp map bexp2

let rec readBlock cnt (s:char list) (acc:char list) =
    match (cnt, s) with
     (_, []) -> (List.rev acc, [])
    | (_, '{'::ls) -> readBlock (cnt+1) ls ('{'::acc)
    | (1, ('}'::ls)) -> (List.rev acc, ls)
    | (_, ('}'::ls)) -> readBlock (cnt-1) ls ('}'::acc)
    | (_, (ch::ls)) -> readBlock cnt ls (ch::acc)

let rec readUntil s acc =
    match s with 
    [] -> acc, []
    | (')'::ls) -> acc |> trimLeadingSpaces |> List.rev |> trimLeadingSpaces, ls
    | ch :: ls -> readUntil ls (ch::acc)


let rec parseCode (cs: construct list) (code:char list) = 

    let rec getStatements (chs:char list) (acc:char list) (cons: construct list) =
        match (chs, acc) with
         ([], []) -> List.rev cons
        | ([], acc) -> 
            let cs = parseCode [] (acc |> trimLeadingSpaces |> List.rev) 
            in List.rev (List.concat [cs; cons])

        | (';'::xs, acc) -> 
            let cs = parseCode [] (acc |> trimLeadingSpaces |> List.rev) 
            in getStatements xs [] (List.concat [cs; cons])
        
        | (' '::xs, []) -> getStatements xs [] cons
        | (ch :: xs, acc) -> getStatements xs (ch::acc) cons

    in let readStatementsBlock xs =
        let (ss, xs') = readBlock 1 xs []
        in (parseCode [] ss, xs')

    in let rec getAssignment (code:char list) (var:char list) =
    
        match code with
         ':'::'='::xs -> (var |> trimLeadingSpaces |> List.rev |> trimLeadingSpaces, xs)
        | ch :: xs -> getAssignment xs (ch::var)
        | _ -> raise (Failure "Unexpected Assignment!") in

    if List.length code = 0 then 
        List.rev cs
    else if List.hd code = ';' then 
        parseCode cs (List.tl code)
    else if startsWith ['w'; 'h'; 'i'; 'l'; 'e'] code then
        let xt0 = drop 5 code |> trimLeadingSpaces
        in let (bexp, xt1) = readUntil xt0 []
        in match trimLeadingSpaces xt1 with
         ('d'::('o'::xt2)) -> 
            match trimLeadingSpaces xt2 with
             '{'::xt3 -> 
                let (ss, xt4) = readStatementsBlock (trimLeadingSpaces xt3)
                and ts = tokenize bexp []
                in parseCode (While(createboolExp ts, ss) :: cs) xt4
            | _ -> []
        | _ -> []

    else if startsWith [ 'i'; 'f'] code then
        let (_::xt0) = drop 2 code |> trimLeadingSpaces
        in let (bexp, xt1) = readUntil xt0 []
        in match trimLeadingSpaces xt1 with
         't'::'h'::'e'::'n'::xt2 ->
            match trimLeadingSpaces xt2 with
             '{'::xt3 -> 
                let (ifss, xt4) = readStatementsBlock (trimLeadingSpaces xt3)
                in match trimLeadingSpaces xt4 with
                 'e'::'l'::'s'::'e'::xt5 ->
                    match trimLeadingSpaces xt5 with
                     '{'::xt6 -> 
                        let (elsess, xt7) = readStatementsBlock (trimLeadingSpaces xt6)
                        in parseCode (If (createboolExp (tokenize bexp []), ifss, elsess)::cs) xt7   
                    | _ -> raise (Failure "Error in parse code!")
                | _ -> raise (Failure "Error in parse code!")
            | _ -> raise (Failure "Error in parse code!")
        | _ -> raise (Failure "Error in parse code!")
       
    else
        let (xs, code') = splitWhen (fun ch -> ch = ';') code
        in let (var, expr) = getAssignment (trimLeadingSpaces xs) []
        in let ex = createbinExp (tokenize expr [])
        in parseCode (Assignment(implode var, ex) :: cs) code' 
        
let rec evaluateConstructs map cs = 
    match cs with
     [] -> map
    | Assignment (var, bexp) :: cs' ->
        let ev = evalBinExp map bexp
        in evaluateConstructs (MyVariables.add var (string_of_int ev) map) cs'
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



let maybe_read_line () =
  try Some(read_line())
  with End_of_file -> None

let rec loop acc =
  match maybe_read_line () with
   None -> List.rev acc |> String.concat "" 
  | Some(line) -> loop (line :: acc)

let printKeyValue k v =
    print_string k;
    print_string " ";
    print_string v
    

let () = 
    loop []
    |> explode
    |> parseCode []
    |> evaluateConstructs (MyVariables.empty)
    |> MyVariables.iter printKeyValue
