open System

[<EntryPoint>]
let main argv = 

    let input1 = "8 4"
    let input2 = "3 1 7 5"

    //let input1 = Console.ReadLine()
    //let input2 = Console.ReadLine()

    let candiesMinutes = input1.Split(' ') |> Array.map int
    let candiesUsedByMinute = input2.Split(' ') |> Array.map int |> Array.toList

    let candyCount = candiesMinutes.[0]
    let partyDuration = candiesMinutes.[1]
    let threshold = 5
        
    candiesUsedByMinute
    |> List.fold (fun (rem, added) used ->
        let balance = rem - used
        if balance < threshold then (candyCount, candyCount - balance) else (balance, added)) (0, candyCount)
    |> (fun (rem, added) -> Console.WriteLine(added))
    
    Console.ReadKey() |> ignore

    0 // return an integer exit code
