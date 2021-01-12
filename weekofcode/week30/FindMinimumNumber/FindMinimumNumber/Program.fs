open System

[<EntryPoint>]
let main argv = 
    
    let count = Console.ReadLine() |> int

    let rec printMin count =
        
        if count = 2 then 
            Console.Write("min(int, int")
        elif count > 2 then 
            Console.Write("min(int, ")
            printMin (count-1) 
        
        Console.Write(")") 

    printMin count
    Console.Read()




    0 // return an integer exit code
