open System

open BoardUnits
open Position
open Moves
open MateFinder

let startpos = { 
    EmptyBoard with 
                WhiteKing = G1;
                WhiteQueens = H5;
                WhitePawns = A2 ||| B2 ||| C2 ||| G2 ||| H2;
                WhiteBishops = C4 ||| E3;
                WhiteKnights = D2 ||| D4;
                WhiteRooks = A1 ||| F4;
                
                BlackKing = G7;
                BlackQueens = D8;
                BlackPawns = A7 ||| B7 ||| E5 ||| G6 ||| H7;
                BlackBishops = F8 ||| C8;
                BlackKnights = B8 ||| F6;
                BlackRooks = A8 ||| H8;

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

    printfn "number of calls: %d" numberOfCalls
    0
