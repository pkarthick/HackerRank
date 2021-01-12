#load "Memoize.fs"

open System
open Cache

let n, k = 3, 1
let r = [| 1; 3; 2; |] |> Array.sort

//let n, k = Console.ReadLine().Split(' ') |> Array.map int |> (fun arr -> arr.[0], arr.[1])
//let r = Console.ReadLine().Split(' ') |> Array.map int
let area r = 
    let rf = float r
    Math.PI * rf * rf 

let calcArea =
    memoize area

let avgArea rs =
    let len = rs |> Array.length |> float
    (rs |> calcArea) / len

let rec getComb k (r:int[]) =
    match k with
    | 0 -> 
        // printfn "r=%A" r
        let x = calcArea r
        // printfn "%f" x
        x
    | _ ->

        let collide i j =
            let r = Array.copy r
            // printfn "i=%i j=%i" i j
            r.[j] <- r.[j] + r.[i]
            let x =
                match i=0 with 
                | true -> r.[i+1..] 
                | false -> Array.concat [ r.[0..i-1]; r.[i+1..] ]
            // printfn "%A" x
            x

        let n = Array.length r
        
        [| 0 .. n-2 |]
        |> Array.collect (fun i -> 
            [| i+1 .. n-1 |]
            |> Array.map (fun j -> getComb (k-1) (collide i j)))
        |> Array.average

let expectedArea n k r =

    match k with 
    | 0 -> avgArea r
    | k when n-k = 1 -> avgArea [| Array.sum r |]
    | _ ->  getComb k r 
    
        


// let rec comb acc r k =


// let exp = expectedArea n k r

// printfn "%.10f" exp


// let inputs = [ 
//     [|6; 5; 7|];
//     [|5; 6; 7|];
//     [|5|]; [|7|]; [|9|];
//     [|5; 9|]; [|7; 9|];
//     [|8|]; [|5; 7; 8; 9|]
// ]

// inputs |> List.iter (avgArea >> printfn "%f")
