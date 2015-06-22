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
open System.Diagnostics
[<EntryPoint>]
let main argv = 
    //moves startpos |> Seq.iter(fun m -> printfn "%s" (m.ToString()))
     
    let sw = Stopwatch()
    sw.Start()

    let mateline = (findMate startpos 0 8)
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"

    printfn "ply count: %d" numberOfCalls
    printfn "time (ms): %d" (sw.ElapsedMilliseconds)
    0
