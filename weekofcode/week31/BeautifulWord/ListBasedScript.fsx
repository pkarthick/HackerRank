open System

let word = "abacaba"

let isBeautiful word =

    let (|Vowel|Consonant|) = function
        | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> Vowel
        | _ -> Consonant
    
    let rec looplist = function
        | [] -> true 
        | [head] -> true
        | head::next::_ when head = next -> false 
        | Vowel head::Vowel next::_ -> false
        | _::tail -> (looplist tail)

    let rec loop = function
        | [||] -> true 
        | [|head|] -> true
        | [|head;next;_|] when head = next -> false 
        | [|Vowel head;Vowel next;_|] -> false
        | arr -> loop arr.[1..]

    loop word

let x = word.ToCharArray() |> isBeautiful

printfn "%s" (if x = true then "Yes" else "No")

