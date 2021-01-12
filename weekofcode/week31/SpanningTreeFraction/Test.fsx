//#load "Library.fs"
//open Library
open System

// let [| n; m |] = Console.ReadLine().Split(' ') |> Array.map int

// let edges = 
//     [|1..m|] 
//     |> Array.map (fun _ -> Console.ReadLine().Split(' ') |> Array.map int)


let n = 4
let m = 6

//let data = [| [|0; 1; 1; 1|]; [|1; 2; 2; 4|]; [|2; 0; 1; 2|]; |]
let data = [ [|0; 1; 1; 1|]; [|1; 2; 2; 4|]; [|2; 0; 1; 2|]; [|0; 2; 5; 6|];  [|1; 3; 1; 2|]; [|3; 2; 1; 2|]; ]

let edges =
    seq {
        for e in data do
            if e.[0] < e.[1] then
                yield e
            elif e.[0] > e.[1] then
                yield [| e.[1]; e.[0]; e.[2]; e.[3] |]
    }
    |> Seq.groupBy (fun arr -> (arr.[0], arr.[1]))
    |> Seq.map (fun (x, items) -> items |> Seq.maxBy (fun arr -> (float arr.[2]/float arr.[3])))
    |> List.ofSeq
    
let isValid (comb:int[] list) =
   
    let len =
        comb 
        |> List.collect (fun arr -> [ arr.[0]; arr.[1] ])
        |> List.distinct
        |> List.length

    len = n

let rec combinations (nr,dr) size set = 
    seq {
        match size, set with 
        | n, [|s;t;a;b|]::xs -> 
            if n > 0 then yield! combinations (nr+a, dr+b) (n - 1) xs
            if n >= 0 then yield! combinations (nr,dr) n xs 
        | 0, [] -> yield (nr,dr) 
        | _, [] -> () 
    }

let nr, dr =
    edges
    |> combinations (0,0) (n-1) 
    |> Seq.maxBy (fun (x,y) -> (float x/float y))    

printfn "%i/%i" nr dr
