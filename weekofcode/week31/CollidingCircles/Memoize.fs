module Cache

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
            x
            |> Array.fold (fun (l,s) r -> 
                let l' = Array.concat [l ; [|r|]]
                match (!cache).TryFind(l') with
                | Some res -> (l', res)
                | None ->
                    let ra =
                        match (!cache).TryFind([|r|]) with
                        | Some res -> res
                        | None ->
                            let a = f r
                            cache := (!cache).Add([|r|],a)
                            a

                    match l with
                    | [||] -> 
                        ([|r|], ra)
                    | _ ->
                        match (!cache).TryFind(l) with
                        | Some res ->
                            cache := (!cache).Add(l',res+ra)
                            (l', (res+ra))
                        | None -> 
                            failwith "This is unexpected in memozation"
                ) ([||], 0.0)
            |> snd            
            

