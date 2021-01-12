open System

let coefs =
    Console.ReadLine().Split(' ')
    |> Array.toList
    |> List.map float

let exps =
    Console.ReadLine().Split(' ')
    |> Array.toList
    |> List.map float

let pairs = List.zip coefs exps

let lr =
    Console.ReadLine().Split(' ')
    |> Array.toList
    |> List.map float


seq { lr.[0] .. 0.001 .. lr.[1] }
|> Seq.map (fun disci ->
    pairs
    |> List.fold (fun st (coef, pwr) -> st + (coef * disci ** pwr)) 0.0)
|> Seq.fold (fun (ar, vl) y -> ar + 0.001 * y, vl + 0.001 * y * y * System.Math.PI) (0.0, 0.0)
|> fun (area, volume) ->
    printfn "%.1f" area
    printfn "%.1f" volume
