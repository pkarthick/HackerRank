namespace Tests
open Klotski.Puzzle

open NUnit.Framework

type TestClass () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.OriginTest () =
        
        let rows = 3
        let cols = 4

        let boardData =  [ [ "A"; "A"; "C"; "." ]; [ "A"; "B"; "C"; "." ]; [ "B"; "B"; "."; "." ] ]
        
        let board = create boardData

        Assert.AreEqual ((1,0), getOrigin board "B")
        Assert.AreNotEqual ((1,1), getOrigin board "B")


    [<Test>]
        member this.BlockingLabelsTest () =
            
            let rows = 3
            let cols = 4

            let boardData =  [ 
                [ "A"; "A"; "C"; "." ]; 
                [ "A"; "B"; "C"; "." ]; 
                [ "B"; "B"; "."; "." ] ]
            
            let board = create boardData

            Assert.AreEqual (Blocked ["B"], getMoveState (board, ("A", Down)))
            Assert.AreEqual (Blocked ["C"; "B"], getMoveState (board, ("A", Right)))
            Assert.AreEqual (Blocked ["C"], getMoveState (board, ("B", Right)))

            Assert.AreEqual (Invalid, getMoveState (board, ("C", Up)))
            Assert.AreEqual (Blocked ["A"; "B"], getMoveState (board, ("C", Left)))
            Assert.AreEqual (Valid, getMoveState (board, ("C", Right)))
            Assert.AreEqual (Valid, getMoveState (board, ("C", Down)))


    [<Test>]
        member this.Test1 () =
        
            let boardData =  List.map (fun (s:string) -> s.Split(" ") |> List.ofArray ) [ "A A C ."; "A B C ."; "B B . ." ]
        
            let board = create boardData
            let lbl = "B"
            let (tr, tc) = (0,1)
            let res = 2

            let moves = getAvailableMoves board lbl (tr,tc) [] None

            Assert.AreEqual (res, List.length (Option.get moves))


    [<Test>]
        member this.Test2 () =
    
            let boardData =  List.map (fun (s:string) -> s.Split(" ") |> List.ofArray ) [ "A . . . C ."; "A . B . C ."; "D . B . . ." ]
                        
            let board = create boardData
            let lbl = "D"
            let (tr, tc) = (0,5)
            let res = 1

            let moves = getAvailableMoves board lbl (tr,tc) [] None
            
            Assert.AreEqual (res, List.length (Option.get moves))


    [<Test>]
        member this.Test3 () =
    
            let boardData =  List.map (fun (s:string) -> s.Split(" ") |> List.ofArray ) [ "AA AA BB"; "CC .. BB"; "CC DD DD"; ".. .. EE" ]
                        
            let board = create boardData
            let lbl = "EE"
            let (tr, tc) = (1,1)
            let res = 6

            let moves = getAvailableMoves board lbl (tr,tc) [] None
            
            Assert.AreEqual (res, List.length (Option.get moves))
