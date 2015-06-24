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
                    WhiteKing = G1;
                    WhiteQueens = D5;
                    WhitePawns = A2 ||| D4 ||| F2 ||| G2 ||| H2;
                    WhiteRooks = C1 ||| E1;
                
                    BlackKing = C7;
                    BlackQueens = F6
                    BlackPawns = A6 ||| A7 ||| G6 ||| H7;
                    BlackBishops = G7;
                    BlackKnights = C6;
                    BlackRooks = A8 ||| H8;

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
    //futures startpos |> Seq.iter(fun f -> printfn "%s  %d" (f.M.ToString()) (f.BlackRingRisk))
     
    let sw = Stopwatch()
    sw.Start()

    let mateline = (findMate startpos 0 10)
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"

    printfn "ply count: %d" numberOfCalls
    printfn "time (ms): %d" (sw.ElapsedMilliseconds)
    0
