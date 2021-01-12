open System
let q = Console.ReadLine() |> int

for i = 1 to q do
    let n = Console.ReadLine() |> int
    let arr = Console.ReadLine().Split( ' ' ) |> Array.map int
    let last = n-1

    let isSorted () =
        arr
        |> Seq.pairwise
        |> Seq.forall (fun pair -> (snd pair - fst pair) = 1 )

    let swap i =
        let x = arr.[i]
        arr.[i] <- arr.[i+1]
        arr.[i+1] <- x

    let rec sort i swapped =
        match i=last, swapped with 
        | true, false -> isSorted ()
        | true, true -> sort 0 false
        | _, _ ->
            match arr.[i]-arr.[i+1] with
            | 1 -> 
                swap i
                sort (i+1) true
            | _ -> sort (i+1) swapped

    if sort 0 false then printfn "%s" "Yes" else printfn "%s" "No"


