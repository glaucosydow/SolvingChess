open System

open BoardUnits
open Position
open MoveGen
open MateFinder

//let startpos = { 
//    EmptyBoard with 
//                WhiteKing = H1;
//                WhitePawns = A2 ||| C3 ||| D4 ||| F4 ||| H2;
//                WhiteQueens = G5;
//                WhiteBishops = G6;
//                WhiteRooks = H5;
//                
//                BlackKing = G7;
//                BlackPawns = A7 ||| B6 ||| C7 ||| D5
//                BlackKnights = G4 ||| H6
//                BlackRooks = A8 ||| F8
//}

let startpos = { 
        EmptyBoard with 
                    WhiteKing = F7;
                    WhitePawns = F5 ||| G6;
                    BlackKing = H8;
                    BlackPawns = G7 ||| E5;
                    SideToMove=White
    }

let printmoves history =
    printfn "-----------------"
    history 
    |> Array.iter (fun move -> printfn "%s" (move.ToString()))


[<EntryPoint>]
let main argv = 
    //moves startpos |> Seq.iter(fun move -> printfn "%s" (move.ToString()))
    let mateline = (findMate startpos 0)
    
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"

    0
