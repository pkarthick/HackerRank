open System
open System.Collections.Generic

// let n = Console.ReadLine() |> int
// let s = Console.ReadLine()

let n = 8
let s = "bccbbbbc"

let palSet = Dictionary<string, bool>()

let prefixSet = HashSet<string>()

let isPalindrome (pal:string) =

    if palSet.ContainsKey(pal) then 
        palSet.[pal]
    else
        
        let len = pal.Length - 1
        let mid = pal.Length / 2
        
        let rec loop i =
            if i = mid then true
            elif pal.[i] = pal.[len-i] then loop (i+1)
            else false
        
        let isPal = loop 0
        palSet.Add(pal, isPal)
        isPal

let palindromes = Dictionary<string, bool>()

let checkPrefix prefix =
    
    let currentInSet = prefixSet.Contains(prefix)
    
    if not currentInSet then 
        prefixSet.Add(prefix) |> ignore
        0
    else
        1

let rec findSubPalindromes (pal:string) (prefix:string) (subPals:string list) =
    

    if pal = "" then subPals 
    else

        let prefix = s.[0..ind]
        let secPrefix = s.[1..ind]
        let suffix = s.[ind+1..]
        
        match suffix with
        | "" -> subPals
        | suf when isPalindrome suf -> findSubPalindromes "" suf subPals
        | _ -> findSubPalindromes 
        

        if ind = 0 then subPals 
        
        else
            let prefix = s.[0..ind]
            let secPrefix = s.[1..ind]
            let current = s.[ind..ind]
            let suffix = s.[ind+1..]

            let pals = List.filter isPalindrome [prefix; suffix]

            findSubPalindromes pal.[1..0] prefix + pal.[0..0] (if List.contains current subPals then List.append pals subPals else current::pals) (ind-1)


let rec specialStrings ind counts =
    
    if ind = n then counts
    elif ind = 0 then 
        let cc = checkPrefix s.[0..0]
        specialStrings 1 [cc]
    else
        
        let prefix = s.[0..ind]
        let current = s.[ind..ind]
        let suffix = s.[ind+1..]

        let cc = checkPrefix prefix

        let rec getLength (pal:string) =
            printfn "subpref = %s" pal
            let len = pal.Length - 1

            List.fold (fun acc j -> 
                let subpre = pal.[j..len]

                let sublen = 
                    if subpre.Length = 1 then 
                        checkPrefix subpre
                    else
                        
                        if isPalindrome subpre then 
                            printfn "palindrome = %s" subpre
                            getLength subpre
                        else
                            checkPrefix subpre

                acc + sublen

            ) 0 [len-1 .. -1 .. 0]
        
        if isPalindrome prefix then 
            let len = getLength prefix 
            printfn "%A" prefixSet
            specialStrings (ind+1) (cc + len::counts)
        
        else
            specialStrings (ind+1) (cc::counts)
            
printfn "%A" <| findSubPalindromes "bccb" [] 0
// printfn "%A" (specialStrings 0 [])


[0..n-1] 
|> List.iter (fun i -> 
        [i.. -1 .. 0]
        |> List.map (fun j -> s.[j..i])
        |> List.filter isPalindrome
        |> List.iter (fun s' -> 
            //printfn "pal = %s" s'
            //printfn "before = %A" prefixSet |> ignore
            [0..(s'.Length - 1)]
            |> List.map (fun xi -> s'.[0..xi])
            |> List.skipWhile (fun str -> palPrefix.Contains(str))
            |> List.iter (fun xi -> 
                printfn "%s" s'.[0..xi]
                palPrefix.Add(s'.[0..xi]) |> ignore ) 
        )
    palPrefix.Count
    |> printfn "%d" 
) 




