open System

open BoardUnits
open Position
open Moves
open MateFinder

let startpos = { 
    EmptyBoard with 
                WhiteKing = E1;
                WhiteQueens = C6;
                WhitePawns = F6;
                WhiteBishops = E4;
                WhiteKnights = D5;
                
                BlackKing = B8;
                BlackPawns = A7 ||| C7 ||| F7;
                BlackBishops = D3;
                BlackRooks = E6 ||| C8;

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
    let mateline = (findMate startpos 0 15)
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"

    printfn "number of calls: %d" numberOfCalls
    0
