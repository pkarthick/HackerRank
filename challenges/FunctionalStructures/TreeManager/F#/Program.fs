open System

type Tree = Node of string * Tree list

type Position = Position of int * Tree

let rec replaceChild p ps =
    match p, ps with
    | Position(0, ch), [ Position(0, Node(pv, ps)) ] ->
        [ Position(0, Node(pv, ch :: List.skip 1 ps)) ]
    | Position(ci, ch), [ Position(0, Node(pv, ps)) ] ->
        [ Position(0, Node(pv, List.take ci ps @ (ch :: List.skip (ci + 1) ps))) ]
    | Position(0, ch'), (Position(pai, Node(pv, ps)) :: ds) ->
        let p' = Position(pai, Node(pv, ch' :: List.skip 1 ps))
        p' :: replaceChild p' ds
    | Position(ci, ch'), (Position(pai, Node(pv, ps)) :: ds) ->
        let p' =
            Position
                (pai, Node(pv, List.take ci ps @ (ch' :: List.skip (ci + 1) ps)))
        p' :: replaceChild p' ds
    | _ -> failwith "Invalid Scenario in replaceChild!"

let changeValue v ps =
    match ps with
    | [ Position(0, Node(_, cs)) ] -> [ Position(0, Node(v, cs)) ]
    | (Position(i, Node(_, cs)) :: ds) ->
        let ch' = Node(v, cs)
        Position(i, ch') :: replaceChild (Position(i, ch')) ds
    | _ -> failwith "Invalid Scenario in changeValue!"

let insertChild v ps =
    match ps with
    | [ Position(0, Node(rv, cs)) ] ->
        [ Position(0, Node(rv, Node(v, []) :: cs)) ]
    | (Position(i, Node(cv, cs)) :: ds) ->
        let c' = Position(i, Node(cv, Node(v, []) :: cs))
        c' :: replaceChild c' ds
    | _ -> failwith "Invalid Scenario in insertChild!"

let insertRight cv ps =
    match ps with
    | (Position(i, ch) :: Position(pai, Node(pv, ps)) :: ds) ->
        let p' =
            Position (pai,
                 Node (pv,
                      List.take (i + 1) ps
                      @ Node(cv, []) :: List.skip (i + 1) ps))
        if List.isEmpty ds then
            [ Position(i, ch); p' ]
        else Position(i, ch) :: p' :: replaceChild p' ds
    | _ -> failwith "Invalid Scenario in insertRight!"

let insertLeft v ps =
    match ps with
    | (Position(i, ch) :: Position(pai, Node(pv, ps)) :: ds) ->
        let p' =
            Position
                (pai, Node(pv, List.take i ps @ Node(v, []) :: List.skip i ps))
        if List.isEmpty ds then
            [ Position(i + 1, ch); p' ]
        else Position(i + 1, ch) :: p' :: replaceChild p' ds
    | _ -> failwith "Invalid Scenario in insertLeft!"

let visitChild ci ps =
    match ps with
    | Position(_,Node (_, chs))::_ ->
        let ch =  List.item (ci-1) chs
        Position(ci-1, ch)::ps
    | _ -> failwith "Invalid Scenario in visitChild!"

let visitRight ps = 
    match ps with
    | (Position(i,_)::Position(pi,Node (pv, ps))::ds) -> Position(i+1, List.item (i+1) ps)::Position(pi,Node (pv, ps))::ds
    | _ -> failwith "Invalid Scenario in visitRight!"

let visitLeft ps = 
    match ps with
    | (Position(i,_)::Position(pi,Node (pv, ps))::ds) -> Position(i-1,List.item (i-1) ps)::Position(pi,Node (pv, ps))::ds
    | _ -> failwith "Invalid Scenario in visitLeft!"

let visitParent ps = 
    match ps with
    | _::p::ds -> p::ds
    | _ -> failwith "Invalid Scenario in visitParent!"

let deleteChild ps = 
    match ps with
    | (Position(0,_)::Position(pai, Node (pv, _::pt))::ds) ->
        let p' = Position (pai, Node (pv, pt))
        p' :: if List.isEmpty ds then [] else replaceChild p' ds
    | (Position(1,_)::Position(pai, Node (pv, ph::_::pt))::ds) ->
        let p' = Position (pai, Node (pv, ph::pt))
        p' :: if List.isEmpty ds then [] else replaceChild p' ds
    | (Position(i,_)::Position(pai, Node (pv, ps))::ds) ->
        let p' = Position (pai, Node (pv, List.take i ps @ List.skip (i+1) ps))
        p' :: if List.isEmpty ds then [] else replaceChild p' ds
    | _ -> failwith "Invalid Scenario in deleteChild!"

[<EntryPoint>]
let main argv =
    let n = Console.ReadLine() |> int
    [ 1..n ]
    |> List.map (fun _ -> Console.ReadLine().Split(' ') |> List.ofSeq)
    |> List.fold (fun cur ws ->
           
           match ws with
           | [ "change"; v ] -> changeValue v cur
           | [ "print" ] ->
               match cur with
               | Position(_, Node(v, _)) :: _ ->
                   printfn "%s" v
                   cur
               | _ -> failwith "Something wrong in print!"
           | [ "insert"; "child"; v ] -> insertChild v cur
           | [ "insert"; "right"; v ] -> insertRight v cur
           | [ "insert"; "left"; v ] -> insertLeft v cur
           | [ "visit"; "right" ] -> visitRight cur
           | [ "visit"; "left" ] -> visitLeft cur
           | [ "visit"; "child"; v ] -> visitChild (v |> int) cur
           | [ "visit"; "parent" ] -> visitParent cur
           | [ "delete" ] -> deleteChild cur
           | _ -> failwith "Invalid Input!") [ Position(0, Node("0", [])) ]
    |> ignore
    0 // return an integer exit code
