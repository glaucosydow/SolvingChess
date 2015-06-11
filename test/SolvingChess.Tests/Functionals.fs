module Functionals

open Xunit

open BoardUnits
open Position
open MoveGen
open MateFinder

[<Fact>]
let ``puzzle 01``() =
    let startpos = { 
        EmptyBoard with 
                    WhiteKing = F7;
                    WhitePawns = F5 ||| G6;
                    WhiteKnights = G4;
                    BlackKing = H8;
                    BlackPawns = G7 ||| F6
    }

    let mateline = (findMate startpos Array.empty)
    Assert.True(mateline <> None)

    let expected = [| N G4 H6; p G7 H6; p G6 G7; K H8 H7; p G7 G8 |]
    let current = mateline.Value

    Array.iter2 (fun (a: Move) b -> Assert.Equal(a, b)) expected current
