open System

let isSorted arr =
    arr
    |> Seq.pairwise
    |> Seq.forall (fun pair -> 
        (snd pair - fst pair) = 1 )

let swap (arr:int[]) i =
    let x = arr.[i]
    arr.[i] <- arr.[i+1]
    arr.[i+1] <- x

let rec sort (arr:int[]) i n cnt =

    printfn "sort"

    if i = n-1 && cnt = 0 then isSorted arr
    elif i = n-1 && cnt > 0 then sort arr 0 n 0
    else
        match arr.[i]-arr.[i+1] with
        | 1 | -1 -> 
            swap arr i
            sort arr (i+1) n (cnt+1)
        | _ -> sort arr (i+1) n cnt

// let q = Console.ReadLine() |> int

// [0..q-1]
// |> Seq.iter (fun _ ->
//         let n = Console.ReadLine() |> int
//         let arr = Console.ReadLine().Split( ' ' ) |> Array.map int
//         if sort arr 0 n then printfn "%s" "Yes" else printfn "%s" "No"
//     )

if sort [|1;0;3;2|] 0 4 0 then printfn "%s" "Yes" else printfn "%s" "No"
