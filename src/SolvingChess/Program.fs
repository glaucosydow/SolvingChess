open System

open BoardUnits
open Position
open Moves
open MateFinder

let startpos = { 
    EmptyBoard with 
                WhiteKing = G1;
                WhitePawns = A3 ||| B4 ||| C3 ||| D4 ||| E4 ||| F5 ||| G2 ||| H3;
                WhiteQueens = F2;
                WhiteBishops = E3 ||| H5;
                WhiteRooks = D1 |||D3;
                WhiteKnights = G3 ||| G4;
                
                BlackKing = H7;
                BlackPawns = A4 ||| B5 ||| C7 ||| D6 ||| E5 ||| F6 ||| G5 ||| H6;
                BlackQueens = F8;
                BlackBishops = B7 ||| G7;
                BlackRooks = B8 ||| H8;
                BlackKnights = C6 ||| D7;

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
