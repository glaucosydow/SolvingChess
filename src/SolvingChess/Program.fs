open System

open BoardUnits
open Position
open MateFinder

let startpos = { 
    EmptyBoard with 
                WhiteKing = F7;
                WhitePawns = F5 ||| G6;
                WhiteKnights = G4;
                BlackKing = H8;
                BlackPawns = G7 ||| F6
}

let printmoves history =
    printfn "-----------------"
    history 
    |> Array.iter (fun move -> printfn "%s" (move.ToString()))

[<EntryPoint>]
let main argv = 
    let mateline = (findMate startpos Array.empty)
    
    match mateline with 
    | Some(line) -> printmoves line
    | None -> printfn "There is no answer"


    0
