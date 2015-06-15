open System

open BoardUnits
open Position
open Moves
open MateFinder

let startpos = { 
    EmptyBoard with 
                WhiteKing = G1;
                WhitePawns = A2 ||| C2 ||| E3 ||| F2 ||| G2 ||| H2;
                WhiteBishops = A3;
                WhiteRooks = B1 |||D1;
                WhiteKnights = G7;
                
                BlackKing = F7;
                BlackPawns = A6 ||| B7 ||| E5 ||| F6 ||| G6 ||| H7;
                BlackBishops = C3 ||| C8;
                BlackRooks = A8 ||| H8;
                BlackKnights = G8;

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
//    let mateline = (findMate startpos 0 8)
//    
//    match mateline with 
//    | Some(line) -> printmoves line
//    | None -> printfn "There is no answer"
//
//    printfn "number of calls: %d" numberOfCalls

    0
