module Tests

open System
open Xunit
open Klotski
open Klotski.Logic


(*

    A A C .
    A B C .
    B B . .

*)


//    |> Seq.map (fun (id, r, c, res) -> [| id; r; c; res; |] )

[<Fact>]
let ``Left Tests`` () =

    let rc = 3
    let cc = 4

    let data = [| [| "A"; "A"; "C"; "." |]; [| "A"; "B"; "C"; "." |]; [| "B"; "B"; "."; "." |]; |]

    let leftTestData = 
        [
//            ("A",0,0,None);
            ("C",0,2,Some("."));
            // ("A",0,0,None);
            // ("A",0,0,None);
            // ("A",0,0,None);
            // ("A",0,0,None)
        ]

    leftTestData
    |> List.iter (fun (id, r, c, exp) -> 
        let actual = checkRight id r c rc cc data
        Assert.Equal(exp, actual)
    )


// [<Fact>]
// let ``My test`` () =

//     // let board1 = board.Move Direction.Right "C"

//     // let board2 = board1.Value.Move Direction.Right "B"

//     // let board3 = board2.Value.Move Direction.Up "B"

//     let actual = solve data "B" (0,1) 5

//     printfn "here!!!!"

//     let expected = [| 
//         {id="C"; source=(0,2); target=(1,3) }; 
//         {id="B"; source=(2,0); target=(0,1) } 
//         |] 
    
//     //Assert.Equal(true, true)

//     Assert.Equal<Move[]>(expected, actual)
