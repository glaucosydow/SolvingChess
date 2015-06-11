open System

open BoardUnits
open Position
open MoveGen
open MateFinder

let startpos = { 
    EmptyBoard with 
                WhiteKing = H1;
                WhitePawns = A2 ||| C3 ||| D4 ||| F4 ||| H2;
                WhiteQueens = G5;
                WhiteBishops = G6;
                WhiteRooks = H5;
                
                BlackKing = G7;
                BlackPawns = A7 ||| B6 ||| C7 ||| D5
                BlackKnights = G4 ||| H6
                BlackRooks = A8 ||| F8
}

let printmoves history =
    printfn "-----------------"
    history 
    |> Array.iter (fun move -> printfn "%s" (move.ToString()))


[<EntryPoint>]
let main argv = 
    moves startpos |> Seq.iter(fun move -> printfn "%s" (move.ToString()))
//    let mateline = (findMate startpos Array.empty)
//    
//    match mateline with 
//    | Some(line) -> printmoves line
//    | None -> printfn "There is no answer"

    0
