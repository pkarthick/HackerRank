open System
open System.IO

type Ops = | Add | Sub | Mul | Div

type Token = 
    | Num of bigint | Op of Ops | Brackets of Token list 
    
let rec tokenize acc bs neg xs =
    match xs with
    | [] -> acc
    | ' ' :: xt -> tokenize acc bs neg xt
    | '+' :: '-' :: xt -> tokenize (Op Add :: acc) bs true xt
    | '-' :: '-' :: xt -> tokenize (Op Sub :: acc) bs true xt
    | '*' :: '-' :: xt -> tokenize (Op Mul :: acc) bs true xt
    | '/' :: '-' :: xt -> tokenize (Op Div :: acc) bs true xt
    | '(' :: '-' :: xt -> tokenize [] (acc :: bs) true xt
    | '(' :: xt -> tokenize [] (acc::bs) false xt
    | ')' :: xt -> 
        match bs with
        | [] -> tokenize [Brackets acc] [] false xt
        | ys :: yss -> tokenize (Brackets acc :: ys) yss false xt

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

    let operate op f s =
        match op with
        | Add -> (f + s) % p
        | Sub -> (f - s) % p
        | Mul -> (f * s) % p
        | Div -> 
            if abs f = getBigInt 1 then 
                if f < getBigInt 0 then -s else s
            else
                modDiv s f % p


    let rec loop ts =

        match ts with
        | [Num n] -> n

        | Num s :: Op op :: Num f :: [] -> 
            loop [Num (operate op f s)] 

        | Num s :: Op Mul :: Num f :: tt ->
            loop (Num (f * s % p) :: tt) 
                        
        | Num s :: Op Div :: Num f :: tt ->
            loop (Num (modDiv s f) :: tt) 

        | Num x :: Op Add :: Num s :: Op Mul :: Num f :: tt ->
            loop (Num x :: Op Add :: Num (f * s % p) :: tt)

        | Num x :: Op Sub :: Num s :: Op Mul :: Num f :: tt ->
            loop (Num x :: Op Sub :: Num (f * s % p) :: tt)

        | Num x :: Op Add :: Num s :: Op Div :: Num f :: tt ->
            loop (Num x :: Op Add :: Num (modDiv s f) :: tt)

        | Num x :: Op Sub :: Num s :: Op Div :: Num f :: tt ->
            loop (Num x :: Op Sub :: Num (modDiv s f) :: tt)

        | Num s :: Op Sub :: Num f :: Op Sub :: Num x :: tt ->
            loop (Num (f-s)  :: Op Sub :: Num x :: tt) 
        
        | Num s :: Op Add :: Num f :: Op Sub :: Num x :: tt ->
            loop (Num (f+s)  :: Op Sub :: Num x :: tt) 
        
        | Num s :: Op Sub :: Num f :: Op Add :: Num x :: tt ->
            loop (Num (f-s)  :: Op Add :: Num x :: tt) 
        
        | Num s :: Op Add :: Num f :: Op Add :: Num x :: tt ->
            loop (Num (f+s)  :: Op Add :: Num x :: tt) 

        | Num x :: Op Add :: Num s :: Op op :: Brackets bs :: tt ->
            loop (Num x :: Op Add :: Num s :: Op op :: Num (loop bs) :: tt)

        | Num x :: Op Sub :: Num s :: Op op :: Brackets bs :: tt ->
            loop (Num x :: Op Sub :: Num s :: Op op :: Num (loop bs) :: tt)

        | Num s :: Op op :: Brackets bs :: tt ->
            loop (Num s :: Op op:: Num (loop bs) :: tt)

        | Brackets bs :: tt ->
            loop (Num (loop bs) :: tt)

        | _ -> failwithf "\r\nUnexpected ts: \r\n%A \r\n " ts

    let res = loop ts 
    if res < getBigInt 0 then p - (-res % p) elif res >= p then res % p else res

let content = Console.ReadLine()

content.ToCharArray()
|> List.ofArray
|> tokenize [] [] false
|> evalSimple
|> printfn "%A"

