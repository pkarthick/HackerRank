open System

type Tree = Tree of id: int * sal: int * chsals: Set<int * int> seq *  children: Tree list

[<EntryPoint>]
let main argv =

    let n, q =
        Console.ReadLine()
        |> fun s ->
            s.Split(' ')
            |> Array.map int
            |> fun arr -> arr.[0], arr.[1]

    let rels =
        [ 1 .. n - 1 ]
        |> List.map (fun _ ->
            Console.ReadLine()
            |> fun s -> s.Split(' ')
            |> Array.map int
            |> fun arr -> arr.[1], arr.[0])
        |> List.sort

    let salaries =
        Console.ReadLine()
        |> fun s -> s.Split(' ')
        |> Array.map int

    let rec createChildMap cur rels subs acc =
      match rels with
      | [] -> 
          match subs with
          | [] -> acc |> Map.ofList
          | _ -> (cur, subs |> List.map (fun sub -> (sub, salaries.[sub-1])) |> Set.ofList)::acc |> Map.ofList
      | (sup, sub) :: tail when sup = cur ->
        createChildMap sup tail (sub::subs) acc
      | (sup, sub) :: tail ->
        createChildMap sup tail [sub] ((cur, subs |> List.map (fun sub -> (sub, salaries.[sub-1])) |> Set.ofList)::acc)

    let childMap = createChildMap 1 rels [] []

    let rec createTree acc (sup, sal) =

        match Map.tryFind sup childMap with
        | Some subs ->
            
            let acc1 = subs |> Seq.fold createTree acc

            let children = subs |> Seq.map id

            let grandchildren = subs |> Seq.collect (fun (sub, _) -> Map.find sub acc1)

            Map.add sup (Seq.concat [children; grandchildren] |> Seq.sortBy snd |> List.ofSeq) acc1

        | None ->
            Map.add sup [] acc

    let salMap = createTree Map.empty (1, salaries.[0])
    
    [ 1 .. q ]
    |> List.fold (fun off _ ->
        Console.ReadLine()
        |> fun s -> s.Split(' ')
        |> Array.map int
        |> fun arr ->
            let ind = off + arr.[0]
            let n = arr.[1]
            match Map.tryFind ind salMap with
            | Some sals ->
                let ind = sals |> Seq.item (n-1) |> fst
                ind |> printfn "%i"
                ind
            | _ -> 
                failwith "Unexpected error: Could not find employee id"
            ) 0
    |> ignore

    0 // return an integer exit code