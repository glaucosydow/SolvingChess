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

//let startpos = { 
//    EmptyBoard with 
//                    WhiteKing = E1;
//                    WhiteQueens = D2;
//                    WhitePawns = A2 ||| C3 ||| D5 ||| F2 ||| G2;
//                    WhiteBishops = C4
//                    WhiteKnights = F3
//                    WhiteRooks = A1 ||| H1;
//                
//                    BlackKing = G8;
//                    BlackQueens = A5
//                    BlackPawns = A7 ||| B5 ||| C5 ||| G6 ||| H7;
//                    BlackBishops = E6;
//                    BlackKnights = B8;
//                    BlackRooks = A8 ||| D8;
//
//                    SideToMove = White    
//}

let startpos = { 
    EmptyBoard with 
                    WhiteKing = H2;
                    WhitePawns = F4 ||| G2;
                    WhiteBishops = B4
                    WhiteRooks = G5;
                
                    BlackKing = H4;
                    BlackPawns = A6 ||| E6 ||| F5;
                    BlackBishops = D5;
                    BlackRooks = E2;

                    SideToMove = White    
}

let printmoves history =
    printfn "-----------------"
    history 
    |> Array.iter (fun move -> printfn "%s" (move.ToString()))

open Check
open Attacks
open KingSafety
open System.Diagnostics
[<EntryPoint>]
let main argv = 
//    futures startpos |> Seq.iter(fun f -> printfn "%s" (f.M.ToString()))
     
    let sw = Stopwatch()
    sw.Start()

    let mateline = (findMate startpos 0 4)
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"

    printfn "ply count: %d" numberOfCalls
    printfn "time (ms): %d" (sw.ElapsedMilliseconds)
    0
