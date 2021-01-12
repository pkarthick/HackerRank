open System

let movesArr = [|
    [| 1; 0; 0; 1; |];
    [| 1; 0; 1; 0; 1; |];
    [| 0; 0; 0; 0; 0; 0; |];
    [| 0; 0; 0; 0; 1; 0; |];
    [| 0; 0; 1; 0; 1; 0; |]
|]

let remove (moves:int[]) =

    let len = Array.length moves
    let last = len-1

    let zeroesOnBothSides i =
        if i > last-2 then false
        else moves.[i] = 0 && moves.[i+2] = 0

    let indices =
        moves 
        |> Array.mapi (fun i el -> if zeroesOnBothSides i then Some(i) else None)
        |> Array.filter (fun opt -> opt.IsSome)
        |> Array.length

    indices % 2 = 0

movesArr
|> Array.iter (fun moves -> 
    printfn <| if remove moves then "Bob" else "Alice")


