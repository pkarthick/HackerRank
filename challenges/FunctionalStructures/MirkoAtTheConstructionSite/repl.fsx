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
    
let fdis =
    Array.mapi (fun i f -> (f, i+1, ds.[i])) fs
    |> Seq.groupBy (fun (_, _, d) -> d)
    |> Seq.map (snd >> Seq.maxBy (fun (f, i, _) -> (f, i)))
    |> Seq.sortByDescending (fun (f, i, _) -> (f,i))
    |> List.ofSeq

let dqs = (0::qs) |> List.distinct |> List.sort |> List.tail
let maxq = List.last dqs

let (xf0, xi0, xd0) = List.head fdis

let (f1,i1,d1) :: (f2,i2,d2) :: fdit = List.tail fdis

let (pq, maxt, maxd, maxi) = (0, xf0, xi0, xd0)
let q :: qt =  dqs
let acc = [(0,0,xi0)]
let tfdis = []

(*

let (f1,i1,d1) :: (f2,i2,d2) :: fdit = fdis


let fdis1 = (f2,i2,d2) :: fdit
let acc1 = (pq+1,q,i2)::acc


let qs = dqs;;

let acc = acc1
let fdis = fdis1
let (pq, maxt, maxd, maxi) = (q, t2, i2, d2)
let qs = qt
*)
