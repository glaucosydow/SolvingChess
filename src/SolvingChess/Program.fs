open System

open BoardUnits
open Position
open Moves
open MateFinder

//let startpos = { 
//        EmptyBoard with 
//                    WhiteKing = H2;
//                    WhiteQueens = E4;
//                    WhitePawns = F2 ||| G2;
//                    WhiteBishops = F4;
//                    WhiteRooks = D2 ||| E1;
//                
//                    BlackKing = G4;
//                    BlackQueens = A6
//                    BlackPawns = E6 ||| F6 ||| G7 ||| H7;
//                    BlackBishops = A7;
//                    BlackRooks = C8 ||| H8;
//
//                    SideToMove = White
//}

let startpos = {
    EmptyBoard with
        WhiteKing = A2
        WhiteQueens = G5
        WhiteBishops = D4
        WhiteKnights = D1
        WhiteRooks = H1
        WhitePawns = A3 ||| D5 ||| H2

        BlackKing = H8
        BlackQueens = A5
        BlackBishops = A6 ||| E5
        BlackRooks = E8
        BlackPawns = B4 ||| D6 ||| F7 ||| H7
}


let printmoves history =
    printfn "-----------------"
    history 
    |> Array.iter (fun move -> printfn "%s" (move.ToString()))

open Check
open Attacks
open System.Diagnostics
[<EntryPoint>]
let main argv = 
    //moves startpos |> Seq.iter(fun move -> printfn "%s" (move.ToString()))
    let sw = Stopwatch()
    sw.Start()

    let mateline = (findMate startpos 0 15)
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"

    printfn "ply count: %d" numberOfCalls
    printfn "time (ms): %d" (sw.ElapsedMilliseconds)
    0
