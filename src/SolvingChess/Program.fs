open System

open BoardUnits
open Position
open Moves
open MateFinder

let startpos = { 
    EmptyBoard with 
                WhiteKing = H1;
                WhitePawns = A2 ||| C3 ||| D4 ||| F4 ||| H2;
                WhiteQueens = G5;
                WhiteBishops = G6;
                WhiteRooks = H5;
                
                BlackKing = G7;
                BlackPawns = A7 ||| B6 ||| C7 ||| D5;
                BlackKnights = G4 ||| H6;
                BlackRooks = A8 ||| F8;

                SideToMove = White
}



let printmoves history =
    printfn "-----------------"
    history 
    |> Array.iter (fun move -> printfn "%s" (move.ToString()))

open Check
[<EntryPoint>]
let main argv = 
//    moves startpos |> Seq.iter(fun move -> printfn "%s" (move.ToString()))
//    printfn "%A" (isCheckMate startpos)
    let mateline = (findMate startpos 0 8)
    
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"

    printfn "number of calls: %d" numberOfCalls

    0
