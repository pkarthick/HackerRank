namespace Klotski

open System
open System.Collections.Generic
open System.Runtime.ConstrainedExecution

module Puzzle =

    type Direction =
        | Left
        | Right
        | Up
        | Down

    let isOpposite =
        function
        | (Left, Right) -> true
        | (Up, Down) -> true
        | (Right, Left) -> true
        | (Down, Up) -> true
        | _ -> false

    let opposite =
        function 
        | Left -> Right
        | Right -> Left
        | Up -> Down
        | Down -> Up

    let delta =
        function
        | Left -> (0, -1)
        | Right -> (0, 1)
        | Up -> (-1, 0)
        | Down -> (1, 0)

    type MoveState = 
        | Blocked of string list
        | Invalid 
        | Valid 

    type Block = {origin: int; positions: int list; states: Map<Direction, MoveState>}

    type Board = 
        {rows : int; cols : int; blocks : Map<string, Block>}

    type Move =
        | Single of Board * Board * string * Direction
        | Multiple of Board * Board * string * Direction list


    let createBlock rc cc ps cells lbl =

        let orig = 
            let r = List.head ps / cc
            let c = ps |> List.minBy (fun x -> x % cc)
            (r * cc) + (c % cc)
            
        let getMoveState dir =

            let getPos pos = 
                match dir with
                | Left -> (pos % cc > 0 && Option.isSome cells.[pos - orig - 1], cells.[pos - orig - 1])
                | Right -> (pos % cc < cc-1 && Option.isSome cells.[pos - orig + 1], cells.[pos - orig + 1])
                | Down -> (pos < (rc-1) * cc && Option.isSome cells.[pos - orig + cc], cells.[pos - orig + cc])
                | Up -> (pos >= cc && Option.isSome cells.[pos - orig - cc], cells.[pos - orig - cc])

            (dir, ps
            |> List.map getPos 
            |> fun ys -> 
                if List.forall fst ys then 
                    Valid
                elif List.exists (function | (false, None) -> true | _ -> false) ys then 
                    Invalid
                else
                    ys
                    |> List.filter (function | (false, Some l) -> l <> lbl | _ -> false)
                    |> List.map (snd >> Option.get)
                    |> List.distinct
                    |> fun ys' -> if List.isEmpty ys' then Valid else Blocked ys')

        let states =
            List.map getMoveState [Left; Right; Up; Down]
            |> Map.ofList

        (lbl, {origin=orig; positions=ps; states = states})

    let create (boardData : string list list) =
        let createLabel (s : string) =
            if s.[0] = '.' then None
            else Some s

        let rc = List.length boardData

        let cc =
            boardData
            |> List.head
            |> List.length

        let cells = 
            boardData 
            |> List.collect (List.map createLabel)

        let uniqLabels : string list = 
            cells
            |> List.choose id
            |> List.distinct
            |> List.sort

        let positions lbl = 
            cells
            |> List.indexed
            |> List.filter (snd >> (=) (Some lbl))
            |> List.map fst

            


        let blocks = uniqLabels |> List.map (createBlock rc cc cells) |> Map.ofList

        {rows=rc; cols=cc; blocks=blocks; cells= cells }

    let getOrigin board lbl =
        let orig = board.blocks.[lbl].origin
        (orig / board.cols, orig % board.cols)

    let memoize fn =
      let cache = new System.Collections.Generic.Dictionary<_,_>()
      (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ -> let v = fn (x)
                      cache.Add(x,v)
                      v)

    let makeMove board lbl dir moves =

        let block = board.blocks.[lbl]

        let block' =
            match dir with
            | Left -> 
                {block with origin = block.origin - 1; positions = List.map (fun x -> x - 1) block.positions}
            | Right -> 
                {block with origin = block.origin + 1; positions = List.map ((+) 1) block.positions}
            | Down -> 
                {block with origin = block.origin + board.cols; positions = List.map ((+) board.cols) block.positions}
            | Up -> 
                {block with origin = block.origin - board.cols; positions = List.map (fun x -> x - board.cols) block.positions}
            
        let board' = {board with blocks = Map.add lbl block' board.blocks}

        match moves with
        | [] -> [Single (board, board', lbl, dir)]
        | Single (prevboard, _, oldlbl,olddir) :: _ ->
            if oldlbl = lbl then
                Multiple (prevboard, board', lbl, [dir; olddir]) :: (List.tail moves)
            else
                Single (board, board', lbl, dir) :: moves
        | Multiple (prevboard, _, oldlbl, dirs) :: _ ->
            if oldlbl = lbl then
                (Multiple (prevboard, board', lbl, dir :: dirs)) :: List.tail moves
            else
                Single (board, board', lbl, dir) :: moves

    let rec getAvailableMoves board tl (tr, tc) (moves:Move list) (bestMoves: Move list option) =

        let ols = board.blocks |> List.filter (fun bl -> bl.label <> tl) |> List.map (fun bl -> bl.label)

        let getCanditateMoves (l:string) (dirs:Direction list) =
            dirs
            |> List.map (fun d -> (l, d, getMoveState (board, (l, d))))
            |> List.filter (fun (_,_,ms) -> ms <> Invalid)
            |> List.sortBy (fun (_,_,ms) -> 
                match ms with
                | Valid -> 0
                | Blocked _ -> 1
                | Invalid -> 2
            )

        let rec moveInDirections bestMoves' (blds:(string*Direction) list) (lds:(string*Direction*MoveState) list) :Move list option  =

            let rec moveInDirection (l,d,ms) moves = 
                match ms with
                | Invalid -> 
                    None
                | Valid -> 
                    match makeMove board l d moves with
                    | [] -> failwith "This pattern was not expected!"
                    | move::rest -> 
                        match move with
                        | Single (_, board', _, _) ->
                            let blockedMoves = 
                                List.fold (
                                    fun moves' (bl, bd) -> 
                                        moveInDirection (bl, bd, getMoveState (board', (bl, bd)) moves')
                                ) moves blds
                            if List.forall (Option.isSome) blockedMoves then
                                let blmoves = List.map (Option.get) blockedMoves
                                Some (blmoves @ (move :: rest))
                            else
                                None
                        | Multiple (_, board', _, _) ->
                            getAvailableMoves board' tl (tr, tc) (move::rest) bestMoves'
                | Blocked ls -> 
                    ls
                    |> List.filter (fun l' -> blds |> List.exists (fst >> (=) l') |> not)
                    |> List.collect (fun l' -> getCanditateMoves l' (d::List.filter ((<>) d) [Left; Right; Up; Down]))
                    |> fun lds' -> moveInDirections bestMoves' ((l,d)::blds) lds' 
                    
                            
            match lds with
            | [] -> bestMoves'
            | (l, d, ms) :: ldt ->
                match moveInDirection (l, d, ms) with
                | None -> 
                    moveInDirections bestMoves' blds ldt
                | Some moves' ->

                    if List.length moves' = 1 then
                        Some moves'
                    else
                        match bestMoves' with
                        | None -> 
                            Some moves'
                        | Some oldBest -> 
                            if List.length moves' < List.length oldBest then
                                moveInDirections (Some moves') [] ldt
                            else
                                bestMoves'

        let (cr, cc) = getOrigin board tl

        let lastmovedir = 
            match moves with
            | [] -> None
            | Single (_, _, l, d) :: _ -> if l = tl then Some d else None
            | Multiple (_, _, l, ds) :: _ -> if l = tl then Some (List.head ds) else None

        let filterDirs dirs =
            match lastmovedir with
            | None -> dirs
            | Some d' -> List.filter (fun d -> d <> opposite d') dirs

        let moves' =
            let checkRows () = 
                if cr = tr then
                    Some moves
                elif cr > tr then
                    moveInDirections bestMoves [] (getCanditateMoves tl (filterDirs [Up; Right; Left; Down]))
                else
                    moveInDirections bestMoves [] (getCanditateMoves tl (filterDirs [Down; Right; Left; Up;]))

            if cc > tc then
                moveInDirections bestMoves [] (getCanditateMoves tl (filterDirs [Left; Down; Up; Right; ]))
            elif cc < tc then
                moveInDirections bestMoves [] (getCanditateMoves tl (filterDirs [Right; Down; Up; Left; ]))
            else
                checkRows ()

        match moves' with
        | Some _ -> moves'
        | None -> 
            ols
            |> List.collect (fun l -> getCanditateMoves l (if cc > tc then (filterDirs [Left; Right]) else (filterDirs [Right; Left])))
            |> fun olds -> moveInDirections bestMoves [] olds
             

    let printMoves moves =
        match List.rev moves with
        | _ :: moves' ->
            List.iter (fun mv ->
                match mv with
                | Single (src, tgt, lbl, _) ->
                    printfn "%s %A %A" lbl (getOrigin src lbl) (getOrigin tgt lbl)
                | Multiple (src, tgt, lbl, _) ->
                    printfn "%s %A %A" lbl (getOrigin src lbl) (getOrigin tgt lbl)
            ) moves'
        | _ ->
            failwith "This pattern was not expected!"
            
    let solveKlotski board tl (tr,tc) =
        match getAvailableMoves board tl (tr,tc) [] None with
        | Some moves ->
            match List.rev moves with
            | _ :: moves' -> printMoves moves'
            | _ ->
                failwith "This pattern was not expected!"
        | _ ->
            failwith "This pattern was not expected!"

   

    // type Board with
    //     static member public Create (boardData : string list list) =
    //         create boardData

    //     member public this.GetBlock lbl =
    //         getBlock this lbl

    //     member public this.GetOrigin lbl =
    //         getOrigin this lbl

    //     member public this.GetMoveState lbl dir =
    //         getMoveState this lbl dir
