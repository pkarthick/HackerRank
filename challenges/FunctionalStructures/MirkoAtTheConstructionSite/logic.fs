module Logic

open System
open System.IO

let start = DateTime.Now

let lines =
    File.ReadAllLines(@"C:\Mirror\Fun\HackerRank\FunctionalStructures\MirkoAtTheConstructionSite\testcase8.txt")

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

let qs = others |> Array.map (int) 

let maxq = Seq.max qs

type Building =
    { f0: int // floors count on day 0
      d: int // floors built every day - delta
      i: int } // building number

type BuildingState =
    { b: Building
      fd: int
      d: int }

let fds =
    Array.mapi (fun i f -> (f, ds.[i], i)) fs
    |> List.ofArray
    |> List.groupBy (fun (_, d, _) -> d)
    |> List.map (snd >> Seq.maxBy (fun (f, _, i) -> (f, i)))
    |> List.sortByDescending (fun (f, d, _) -> f + d)


let printTallest () =
    
    let (ht, hd, hi) = List.head fds
    let hq = 0

    let rec processDayN ((hq, ht, hd, hi), fds) cfds q =
        match fds with
        | [] -> 
            ((q, ht, hd, hi), cfds)
        | (f, d, i) :: fdt -> 
            let t = f + d * q

            let cfds' = if d > hd || t > ht then (f,d,i) :: cfds else cfds
            
            if t > ht || t = ht && i >= hi 
            then processDayN ((q, t, d, i), fdt) cfds' q 
            else processDayN ((hq, ht, hd, hi), fdt) cfds' q 

    
    let m = 
        qs 
        |> List.ofArray
        |> List.distinct
        |> List.scan (fun ((hq, ht, hd, hi), fds) q -> processDayN ((hq, ht, hd, hi), fds) [] q) ((hq, ht, hd, hi), fds)
        |> List.map (fst >> fun (hq, ht, hd, hi) -> (hq, hi))
        |> Map.ofList

    for q in qs do
        printfn "%i" (m.[q] + 1)
