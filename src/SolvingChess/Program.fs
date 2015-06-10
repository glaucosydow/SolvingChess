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

[<EntryPoint>]
let main argv = 
    findMate startpos Array.empty
    0
