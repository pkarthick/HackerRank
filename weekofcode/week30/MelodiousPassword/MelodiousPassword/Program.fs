open System

[<EntryPoint>]
let main argv = 
    let count = Console.ReadLine() |> int

    let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]
    let consonants = [ 'b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'j'; 'k'; 'l'; 'm'; 'n'; 'p'; 'q'; 'r'; 's'; 't'; 'v'; 'w'; 'x'; 'z' ]

    let rec printPasswords (vowels: char list) (consonants: char list) (list: char array)  =
        vowels 
        |> List.iter (fun v -> 
            
            if Array.length list = count then
                String.Concat list |> Console.WriteLine
            elif Array.length list = count-1 then
                String.Concat (list) |> Console.Write
                v |> Console.WriteLine
            else
                consonants 
                |> List.iter (fun c ->
                    printPasswords vowels consonants (Array.append list [| v; c |])
                )
            
        )

    printPasswords vowels consonants [| |]
    printPasswords consonants vowels [| |]

    Console.ReadKey() |> ignore

    0 // return an integer exit code
