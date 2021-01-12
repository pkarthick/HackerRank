open System

let input1 = Console.ReadLine()
let input2 = Console.ReadLine()

let nq = input1.Split(' ') |> Array.map int
let a = input2.Split(' ') |> Array.map int 

let n = nq.[0]

let q = nq.[1]

let qa = 
    [| 1..q |]
    |> Array.map (fun i ->
        let input = Console.ReadLine()
        input.Split(' ') |> Array.map int 
    )

let  a = [|250; 501; 500; 5; 4|]
let qa = [| [|0; 4; 5; 0|]; [|0; 4; 10; 0|]; [|0; 4; 3; 2|] |]

qa
|> Array.iter (fun [| left; right; x; y |] ->
    let count =
        [|left..right|]
        |> Array.filter(fun ind -> a.[ind] % x = y)
        
    Console.WriteLine(count.Length)
)
        