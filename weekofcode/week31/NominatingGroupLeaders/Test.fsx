open System
open System.Collections.Generic

let t = 2

let n = 5

let vs = [| [| 0; 1; 4; 0; 3; |] ; [| 4; 3; 0; 0; 0; |]; |]


for i in 0 .. t-1 do
    let v = vs.[i]

    let lrxs = [| [| [| 0; 4; 1; |]; [| 2; 4; 2; |]; |]; [| [| 0; 1; 1; |]; [| 2; 4; 3; |]; |] |].[i]

    let n = Array.length v

    let lr (arr:int[]) = 
        (arr.[0], arr.[1])
    

    let (|Same|_|) ((l1, r1), (l2, r2)) =
        if l1 = l2 && r1 = r2 then Some [(l1,r1)] else None

    let (|Disjoint|_|) ((l1, r1), (l2, r2)) =
        if (r1 < l2 || r2 < l1) then Some [(l1,r1)] else None
    
    let (|Subset|_|) ((l1, r1), (l2, r2)) =
        if l1 >= l2 && r1 <= r2 then Some [(l1,r1)] else None


    let (|Other|_|) ((l1, r1), (l2, r2)) =
        if l1 >= l2 && r1 <= r2 then Some [(l1,r1)] else None

    let (|Superset|_|) ((l1, r1), (l2, r2)) =
        if l1 < l2 && r1 >= r2 then Some [(l1,l2);(l2,r1);] 
        elif l1 = l2 && r1 > r2 then Some [(l1,r2);(r2+1,r1);] 
        else None

    let (|Overlap|_|) ((l1, r1), (l2, r2)) =
        if r1 = l2 then Some [(r1, r1)]
        elif r2 = l1 then Some [(r2, r2)]
        elif r1 <= l2 && l1 < r1 then Some [(l1,r1); (r1+1, r2)]
        elif r2 >= l1 && l2 < l1 then Some [(l2,r2); (r2+1, r1)]
        elif l1 < l2 && r1 < r2 then Some [(l1,l2); (l2+1, r1)]
        else None

    let sorter = function
        | Disjoint list -> 0
        | Same list -> 1
        | Subset list -> 2
        | Superset list -> 3
        | Overlap list -> 4
        | Other list -> 5
        | _ -> 6


    let pairs = lrxs |> Seq.map lr |> Array.ofSeq

    let sorted = 
        lrxs 
        |> Seq.sortBy (fun arr -> 
            let l,r = lr arr
            r-1, l) 

    let getNominee vc countMap =
        let nominee = Map.tryFindKey (fun k v -> v=vc) countMap
        if nominee.IsNone then -1 else nominee.Value

    let comparePairs = function
        | Disjoint list -> list
        | Same list -> list
        | Subset list -> list
        | Superset list -> list
        | Overlap list -> list
        | Other list -> list
        | _ -> failwith "unmatched active pattern" 

    let findBestPair (l1,r1) =
        
        let best =
            sorted
            |> Seq.map (fun arr -> 
                    let (l2, r2) = lr arr
                    let score =
                        if r1 < l2 || r2 < l1 then 0 //disjoint
                        elif (l1,r1) = (l2,r2) then r1-l1  //same
                        elif l1 <= l2 && r2 <= r1 then r2-l2 //subset
                        elif l2 <= l1 && r1 <= r2 then r1-l2 //superset
                        elif l1 < l2 &&  r1 < r2 then r1-l2 //overlap
                        elif l2 < l1 &&  r2 < r1 then l1-r2 //overlap
                        else 0
                    (score, (l2,r2))
                )
            |> Seq.maxBy fst
        
        if fst best > 0 then Some(snd best) else None
        

    let rec getPairs (l1,r1) =

        let res =
            
            let best = findBestPair (l1,r1)

            match best with
            | None -> [(l1,r1)]
            | Some (l2,r2) -> 
                let newList = comparePairs ((l1,r1),(l2,r2)) 
                match newList.Length with
                | 0 | 1 -> [(l1,r1)]
                | _ -> 
                    newList 
                    |> List.fold (fun acc pair -> 
                            if Array.contains pair pairs then pair::acc
                            else List.concat [(getPairs pair); acc] 
                        ) ([])

        if List.length res > 1 then res 
        else [(l1,r1)]
    
    let mutable results = Map.empty

    let cf (l,r,x) =
        let res =
            getPairs (l,r) 
            |> List.fold (fun (acc:Map<int,int>) (l1,r1) -> 
                    
                    let res =
                        v.[l1..r1] |> Seq.countBy (id) 
                        |> Seq.map (fun (k,v) -> 
                            let old = if acc.ContainsKey(k) then acc.[k] else 0
                            (k,v+old)) 
                        |> Map.ofSeq

                    results <- Map.add (l1,r1) res results

                    res

            ) (Map.empty)

        results <- Map.add (l,r) res results
        ()

    Async.Parallel [ for arr in sorted ->  async { cf(arr.[0],arr.[1], arr.[2]) } ]
    |> Async.RunSynchronously
    |> ignore

    lrxs |> Seq.iter (fun arr -> 
        let countMap = Map.find (lr arr) results 
        getNominee arr.[2] countMap |> printfn "%i")
    




