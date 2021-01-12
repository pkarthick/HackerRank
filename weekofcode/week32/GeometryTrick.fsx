open System

// let n = Console.ReadLine() |> int
// let s = Console.ReadLine()

let n = 15
let s = "ccaccbbbaccccca"

let check = function  
| 'a', 'c' -> true
| 'c', 'a' -> true
| _, _ -> false
    
let lim = (float >> sqrt >> int >> (+) 2) n
printfn "%d" lim

let rec factors' j jsq i acc' =
    let jsq = (j+1) * (j+1)
    let k = jsq / i
    if i = j then acc'
    elif jsq = i * k && k < n && check (s.[i-1],s.[k-1]) then factors' j jsq (i+1) (acc'+1)
    else factors' j jsq (i+1) acc'
    
let rec countComb ind acc =
    if ind > lim then acc
    else 
        let jsq = (ind+1) * (ind+1)
        let count = if s.[ind] = 'b' then factors' ind jsq 2 0 else 0
        countComb (ind+1) (acc + count)

countComb 2 0 |> printfn "%d"  