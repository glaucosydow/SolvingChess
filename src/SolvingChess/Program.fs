open System

open BoardUnits
open Position
open Moves
open MateFinder

let startpos = { 
    EmptyBoard with 
                WhiteKing = G1;
                WhiteQueens = C2;
                WhitePawns = B3 ||| F2 ||| G3 ||| H2;
                WhiteBishops = B2 ||| G2;
                WhiteRooks = C1 ||| D1;
                
                BlackKing = F7;
                BlackQueens = D8
                BlackPawns = B7 ||| D5 ||| G7 ||| H6;
                BlackBishops = E7 ||| C8;
                BlackKnights = D7;
                BlackRooks = A8 ||| E8;

                SideToMove = White
}



let printmoves history =
    printfn "-----------------"
    history 
    |> Array.iter (fun move -> printfn "%s" (move.ToString()))

open Check
open Attacks
[<EntryPoint>]
let main argv = 
    //moves startpos |> Seq.iter(fun move -> printfn "%s" (move.ToString()))
    let mateline = (findMate startpos 0 8)
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"

    //printfn "number of calls: %d" numberOfCalls
    0
