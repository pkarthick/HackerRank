open System

let isBeautiful (word:string) =

    let (|Vowel|Consonant|) = function
        | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> Vowel
        | _ -> Consonant

    // let len = word.Length
    
    let isMatch = function
        | x, y when x = y -> true 
        | Vowel x, Vowel y -> true
        | _ -> false

    // let isMatch2 x =
    //     printf "%A" x
    //     isMatch x

    // let isMatch1 cur = 
    //     match word.[cur], word.[cur+1] with
    //     | x, y when x = y -> true 
    //     | Vowel x, Vowel y -> true
    //     | _ -> false

    // let rec loop cur =
    //     if cur = len-1 then true
    //     elif isMatch1 cur then false
    //     else loop (cur+1)

    word 
    |> Seq.pairwise 
    |> Seq.exists isMatch
    |> not

    // loop 0

let word = Console.ReadLine()

if isBeautiful word then printfn "%s" "Yes" else printfn "%s" "No"

