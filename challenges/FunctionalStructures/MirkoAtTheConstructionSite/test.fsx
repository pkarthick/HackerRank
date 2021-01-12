open System
open System.IO

let start = DateTime.Now

let lines =
    File.ReadAllLines(@"C:\Mirror\Fun\HackerRank\FunctionalStructures\MirkoAtTheConstructionSite-f#\testcase8.txt")

let line1 = lines.[0]

let nq =
    line1.Split(' ')
    |> Array.map (int)
    |> Array.take 2

// let line2 = Console.ReadLine()
// let line3 = Console.ReadLine()

let (n, qc) = (nq.[0], nq.[1])
// let others = [|1..qc|] |> Array.map (fun _ -> Console.ReadLine())


let line2 = lines.[1]
let line3 = lines.[2]
let others = lines.[3..]


let fs = line2.Split(' ') |> Array.map (int)

let ds = line3.Split(' ') |> Array.map (int)

let qs = others |> Array.map (int) |> List.ofArray 

let maxq = Seq.max qs


let fds =
    Array.mapi (fun i f -> (f, ds.[i], i)) fs
    |> Seq.groupBy (fun (_, d, _) -> d)
    |> Seq.map (snd >> Seq.maxBy (fun (f, _, i) -> (f, i)) >> fun (f, d, i) -> (f, 0, f, d, i))
    |> Seq.sortByDescending (fun (t, _, f, d, i) -> (t,i))
    |> List.ofSeq

let prepareData fds (xf, xd, _) xq =
    fds
    |> Seq.map (
        fun (_, _, f, d, i) ->
            
                (f + d * xq, xq, f, d, i))
    |> Seq.sortByDescending (fun (t, q, f, d, i) -> (t,i))
    |> Seq.take 2
    |> List.ofSeq


let q = 0

let (xf, _, _, xd, xi) = List.head fds

let dqs = (0::qs) |> List.distinct |> List.sort |> List.tail

let rec getTallest fds (maxt, maxi) dqs acc =

    let (xf, _, _, xd, _) = List.head fds

    match dqs with
    | [] -> acc
    | q :: qt ->
        match prepareData fds (xf, xd, q) q with
        | [(xt1, _, _, xd1, xi1); (xt2, _, _, xd2, xi2)] ->
            let qd = (abs(xt2 - xt1) / abs(xd2 - xd1))
            let acc1 = dqs |> List.takeWhile (fun q -> q < qd) |> List.map (fun q -> (q, xi1))
            let tall = xt1 + xd1 * qd
            getTallest fds (tall, 0) (List.skip (List.length acc1) dqs) (List.append acc1 acc)
        | res ->
            printfn "%A" res
            []

let result = getTallest fds (xf, xi) dqs []

printfn "%A" result

let end2 = DateTime.Now

printfn ""
printfn "%A" (end2 - start).TotalMilliseconds

(*
let q = q + 1
let (xf, _, _, xd, _) = List.head fds
let fds1 = prepareData fds (xf, xd, q) 
let fds0 = fds1
*)

