open System
open System.IO

type Ops = | Add | Sub | Mul | Div

type Token = 
    | Num of bigint | Op of Ops | Brackets of Token list 
    
let rec tokenize acc bs neg xs =
    match xs with
    | [] -> acc
    | x :: ' ' :: xt -> tokenize acc bs neg (x::xt)
    | ' ' :: xt -> tokenize acc bs neg xt
    | '+' :: '-' :: xt -> tokenize (Op Add :: acc) bs true xt
    | '-' :: '-' :: xt -> tokenize (Op Sub :: acc) bs true xt
    | '*' :: '-' :: xt -> tokenize (Op Mul :: acc) bs true xt
    | '/' :: '-' :: xt -> tokenize (Op Div :: acc) bs true xt
    | '(' :: '-' :: xt -> tokenize [] (acc :: bs) true xt
    | '(' :: xt -> tokenize [] (acc::bs) false xt
    | ')' :: xt -> 
        if List.isEmpty bs then
            tokenize [Brackets acc] [] false xt
        else
            tokenize (Brackets acc :: (List.head bs)) (List.tail bs) false xt

    | '+' :: xt -> tokenize (Op Add :: acc) bs false xt
    | '-' :: xt -> 
        if List.isEmpty acc then
            tokenize [] bs true xt
        else
            tokenize (Op Sub :: acc) bs false xt
    | '*' :: xt -> tokenize (Op Mul :: acc) bs false xt
    | '/' :: xt -> tokenize (Op Div :: acc) bs false xt
    | x :: xt  when Char.IsDigit x ->
        let s = List.takeWhile Char.IsDigit xs |> String.Concat 
        let n = s |> int64 |> bigint
        let nds = List.skip (s.Length) xs 
        let num = Num (if neg then -n else n)
        tokenize (num :: acc) bs false nds
    | _ -> raise (Exception "Unexpected error in tokenize!")

let readLines () =
    let rec loop (acc: string list) =
        let s = Console.ReadLine()
        if isNull s then 
            acc 
            |> List.rev
            |> List.reduce (+)
        else 
            loop (s :: acc)
    loop []

let p = 1000000007 |> bigint
let pminus2 = 1000000005 |> bigint

let getBigInt (i:int) = i |> bigint
let negate x = x * getBigInt -1

let zero = getBigInt 0 
let one = getBigInt 1
let two = getBigInt 2

let rec calcPow n (e:bigint) : bigint = 
    if e = one then n
    elif e = two then n * n % p
    else
        let halfe = e / two
        let half = calcPow n halfe % p
        (if e % two = zero then half * half else n * half * half) % p

let calcMod n = 
    (calcPow n pminus2) % p

let modDiv (x:bigint) (y:bigint) =

    let (x', y') = if x < zero && y < zero then (negate x, negate y) else (x,y)
    let modx' = if x' >= zero then calcMod x' else negate (calcMod (negate x'))
    if abs y' % abs x' = zero
        then y' / x'
        else (y' * modx') % p

let evalSimple ts =

    let rec evalAddSub ts =

        match ts with
        | [Num n] -> n

        | Num s :: Op Add :: Num f :: tt ->
            evalAddSub (Num (f + s) :: tt) 

        | Num s :: Op Sub :: Num f :: tt ->
            evalAddSub (Num (f - s) :: tt) 
        
        | _ -> failwithf "\r\nUnexpected ts: \r\n%A \r\n " ts

    let rec evalMulDiv ts acc =


        match ts with
        | [] -> evalAddSub (List.rev acc)

        | Num s :: Op Mul :: Num f :: tt ->
            evalMulDiv (Num (f * s % p) :: tt) acc
                        
        | Num s :: Op Div :: Num f :: tt ->
            evalMulDiv (Num (modDiv s f) :: tt) acc
                        
        | Num s :: Op op :: Num f :: tt ->
            evalMulDiv (Num f :: tt) (Op op :: Num s :: acc)

        | t :: tt ->
            evalMulDiv tt (t::acc)


    let rec evalBrackets ts acc =

        match ts with
        | [] -> 
            evalMulDiv (List.rev acc) []

        | Brackets bs :: tt ->
            let r = evalBrackets bs []
            evalBrackets (Num r :: tt) acc

        | t :: tt ->
            evalBrackets tt (t::acc)


    let res = evalBrackets ts []
    if res < getBigInt 0 then p - (-res % p) elif res >= p then res % p else res

[<EntryPoint>]
let main argv =

    let content = File.ReadAllLines(@"C:\Fun\HackerRank\ExpressionsV2\input12.txt") |> String.concat ""

    content.ToCharArray()
    |> List.ofArray
    |> tokenize [] [] false
    //|> List.iter (printfn "%A")
    |> evalSimple
    |> printfn "%A"

    0

