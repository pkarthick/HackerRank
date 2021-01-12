open System
let n = 3
let q = 3

let s = [| "probieren"; "birkerem"; "sadasment" |]

let j = [ (0,1); (1,2); (0,2) ]

let findLongestSubstring (s1:string) (s2:string) (len1:int) (len2:int) =

    let rec loop (pos1:int) (pos2:int) (ind:int) (max:int) =

        if ind = max/2 then s1.Substring(pos1, ind)
        else
            if s1.[pos1] = s2.[pos2] then 
                if s1.[pos1+max] = s2.[pos2+max] then
                    loop pos1 pos2 (ind+1) max
                else
                    loop pos1 pos2 ind (max-1)
            else
                loop pos1 pos2 (ind+1) max

    loop 0 0 (len1-1)
    
let printLongestSubstring (x,y) =

    let s1 = s.[x]
    let s2 = s.[y]
    let len1 = s1.Length
    let len2 = s2.Length

    let substr = 
        if len1 <= len2 then
            findLongestSubstring s1 s2 
        else
            findLongestSubstring s2 s1 

    substr |> Console.WriteLine

j |> List.iter printLongestSubstring 


