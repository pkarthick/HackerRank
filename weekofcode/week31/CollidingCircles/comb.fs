module comb

open System

let rec getCombinations items memo =

    seq {
        for item in items do 
         let remainingItems = List.filter (fun i -> i <> item) items
         match remainingItems with
         | x::xs -> yield! getCombinations remainingItems (List.append memo [item])
         | [] -> yield List.append memo [item]         
    }

//let results = getCombinations ["A"; "B"; "C"; "D"] []
//printfn "%A" results

let rec permutations list taken = 
  seq { if Set.count taken = List.length list then yield [] else
        for l in list do
          if not (Set.contains l taken) then 
            for perm in permutations list (Set.add l taken)  do
              yield l::perm }

let perms = permutations [1..3] Set.empty

printfn "%A" perms