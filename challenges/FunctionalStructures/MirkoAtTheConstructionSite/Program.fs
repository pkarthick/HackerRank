open System
open System.IO

[<EntryPoint>]
let main argv =

        
    let start = DateTime.Now
    
    let lines =
        File.ReadAllLines(@"C:\Mirror\Fun\HackerRank\FunctionalStructures\MirkoAtTheConstructionSite-f#\mytestcase.txt")
    
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
    let dqs = qs |> Seq.sortDescending |> Seq.toList
    let maxq = List.head dqs

    let f0 = Seq.max fs

    let xs = 
        Seq.mapi2 (fun i f d -> (f+d*maxq, f, i+1, d)) fs ds 
        |> Seq.filter (fun (t,f,i,d) -> t >= f0)
        |> Seq.groupBy (fun (_,_,i,d) -> d)
        |> Seq.map(snd >> Seq.maxBy (fun (_,f,_,_) -> f))
        |> Seq.sortByDescending (fun (t,f,i,d) -> (t, i))
        |> Seq.toList

    let tups = 
        xs 
        |> List.sortBy (fun (t,f,i,d) -> (t, i))
        |> List.skipWhile (fun (t,f,i,d) -> f <> f0)

    let rec processTuples acc tups =

        match tups with
        | [] -> List.rev acc

        | [(t,f,i,d)] ->
            List.rev ((t,f,i,d) :: acc)

        | (t,f,i,d) :: tupt ->

            let (_,_,maxi1,_) = tupt |> List.maxBy (fun (t1,f1,_,d1) -> (t1 - t) / (d1 - d))
            
            let tups1 = tupt |> List.skipWhile (fun (t1,_,i1,_) -> i1 <> maxi1)

            processTuples ((t,f,i,d) :: acc) tups1

    let rec getTallest pairs dqs map =
        match pairs with
        | [] -> map 
        | [((t1,f1,i1,d1), (t2,f2,i2,d2))] ->
            let qis =
                dqs
                |> List.map (fun q -> 
                    if f1 + d1 * q > f2 + d2 * q then
                        Some (q, i1)
                    elif f1 + d1 * q = f2 + d2 * q then
                        Some (q, if i1 > i2 then i1 else i2)
                    else
                        None
                )
                |> List.takeWhile (Option.isSome)
                |> List.map (Option.get)

            let map1 = List.fold (fun m (q,i1) -> Map.add q i1 m) map qis
            let dqs1 = List.skip (List.length qis) dqs
            
            dqs1
            |> List.fold (fun m q -> Map.add q i2 m) map1 
            

        | ((t1,f1,i1,d1), (t2,f2,i2,d2)) :: pairt ->

            let qis =
                dqs
                |> List.map (fun q -> 
                    if f1 + d1 * q > f2 + d2 * q then
                        Some (q, i1)
                    else
                        None
                )
                |> List.takeWhile (Option.isSome)
                |> List.map (Option.get)

            let dqs1 = List.skip (List.length qis) dqs
            let qx = List.head dqs1
            let map1 = 
                List.fold (
                    fun m (q,i1) -> 
                        if Map.containsKey q m then
                            let v = Map.find q m
                            if i1 > v then
                                Map.add q i1 m
                            else
                                m
                        else
                            Map.add q i1 m
                ) map qis

            let map2 =
                if f1 + d1 * qx = f2 + d2 * qx then
                    Map.add qx (if i1 > i2 then i1 else i2) map1
                else
                    map1
                        
            getTallest pairt dqs1 map2

    let (t,f,i,d) = List.head tups
    let pairs = tups |> processTuples [] |> List.pairwise
    let m = getTallest pairs (List.sort dqs) ([(0,i)] |> Map.ofList)

    for q in qs do 
        printfn "%A" m.[q]

    let end2 = DateTime.Now
    
    printfn ""
    printfn "%A" (end2 - start).TotalMilliseconds

    0