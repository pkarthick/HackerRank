open System

let nc = 2
let nb = 2
let balls_ = [|(0,1, false); (1,1,false)|]
let boxcap_ = [|(0,-1,0,false, []); (1,-1,2,false, [])|]
let candies = [|[|3;7|]; [|3;7|]|]


// let [| nc; nb |] = Console.ReadLine().Split(' ') |> Array.map int 

// let readInts () = Console.ReadLine().Split(' ') |> Array.map int

// let balls = readInts()

// let boxcap = readInts()

// let candies = [|1..nc|] |> Array.map (fun _ -> readInts())

let updateBalls (balls':(int * int * bool)[]) color count skip =
    let (color, ballsCount, skip') = balls'.[color]

    if skip then
        balls'.[color] <- (color, ballsCount, true)
    else
        balls'.[color] <- (color, ballsCount - count, skip')
    
    balls'

let updateBox (boxcap':(int * int * int * bool * int list)[]) box color count skip =
    
    let (box, _, boxCount, updated, colorsToSkip) = boxcap'.[box]

    let colorsToSkip' = if skip || count > 0 then (color::colorsToSkip) else colorsToSkip

    if count >= 0 then 
        boxcap'.[box] <- (box, color, boxCount - count, true, colorsToSkip')
    elif count = -1 then 
        boxcap'.[box] <- (box, color, 0, true, colorsToSkip')
    else
        boxcap'.[box] <- (box, color, 0, updated, colorsToSkip')
    
    boxcap'

let square x = x * x

let selectBox (boxcap':(int * int * int * bool * int list)[]) (balls':(int * int * bool)[]) color =
    
    let selectedBoxes =
        boxcap'
        |> Array.filter (fun (_, _, _, updated, colorsToSkip) -> not updated || not (List.contains color colorsToSkip) )
        |> Array.map (fun (box, _, remCount, updated, _) -> 
            
            let (_, ballsAvailable, _) = balls'.[color]
            
            if remCount >= ballsAvailable then 
                (ballsAvailable * candies.[color].[box], (box, color, ballsAvailable, false))
            else
                let excessBalls = ballsAvailable-remCount

                let countToCheck = if excessBalls > candies.[color].[box] - 1 then candies.[color].[box] - 1 else excessBalls

                let ballsToTake = if countToCheck > 0 then [1..countToCheck] |> List.maxBy (fun excess -> candies.[color].[box] * excess - excess * excess) else 0
                
                if ballsToTake > 0 then
                    ((remCount + ballsToTake) * candies.[color].[box] - square (ballsToTake), (box, color, remCount+ballsToTake, true))
                else 
                    (0, (box, color, 0, true)))
    
    if Array.isEmpty selectedBoxes then None 
    else
        let maxForColor = Array.sortByDescending fst selectedBoxes
        let (_,(box, color, _, _)) = Array.head maxForColor
        let maxForBox = 
            balls'
            |> Array.map (fun (color', colorballsCount, _) ->
                let (_, _, remCount, _, _) = boxcap'.[box]
                if colorballsCount <= remCount then
                    (colorballsCount * candies.[color].[box], (box, color', colorballsCount, true) )
                else
                    (remCount * candies.[color].[box], (box, color', remCount, true) )
                )
            |> Array.sortByDescending fst
        if Array.isEmpty maxForBox then None
        else
            let (_,(box, maxcolor, _, _)) = Array.head maxForBox
            if color = maxcolor then Some (Array.head maxForColor)
            else 
                if fst maxForColor.[0] > fst maxForBox.[0] then Some (Array.head maxForColor)
                elif fst maxForColor.[0] < fst maxForBox.[0] then Some (Array.head maxForBox)
                else //equals
                    if ( Array.length maxForColor >= 2 && Array.length maxForBox >= 2 && ((Array.take 2 maxForColor |> Array.sumBy fst)) > ((Array.take 2 maxForBox |> Array.sumBy fst))) then
                        Some (Array.head maxForBox)
                    else
                        Some (Array.head maxForColor)
        
let rec calculateCandies (boxcap':(int * int * int * bool * int list)[]) (balls':(int * int * bool)[]) candiesEarned =
    match Array.tryFind (fun (_, _, skip) -> not skip) balls' with
    | Some (color, remCount, _)  -> 

        match selectBox boxcap' balls' color with
        | Some (candiesToEarn, (box, ballcolor, ballsCount, skip)) -> 
            // printfn "color:%d count:%d candiesToEarn:%d box:%d ballsCount:%d" color remCount candiesToEarn box ballsCount
            let balls'' = if candiesToEarn > 0 then updateBalls (Array.copy balls') ballcolor ballsCount false else (Array.copy balls')
            let boxcap'' = updateBox (Array.copy boxcap') box ballcolor ballsCount (candiesToEarn <= 0)
            calculateCandies boxcap'' balls'' (candiesEarned + candiesToEarn)
            
        | None -> 
            let balls'' = updateBalls (Array.copy balls') color -1 true
            calculateCandies boxcap' balls'' candiesEarned
    | None -> candiesEarned

calculateCandies boxcap_ balls_ 0
|> printfn "%A" 
