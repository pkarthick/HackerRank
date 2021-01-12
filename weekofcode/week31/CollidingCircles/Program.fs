// Learn more about F# at http://fsharp.org

open System

let getComb r =
    let n = Array.length r
    [ 0 .. n-2 ]
    |> Seq.map (fun i -> 
        [i+1 .. n-1]
        |> Seq.map (fun j -> [i; j] )
    )


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
