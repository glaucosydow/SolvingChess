module Puzzles

open NUnit.Framework

open BoardUnits
open Position
open MoveGen
open MateFinder

[<Test>]
let ``puzzle 01``() =
    let startpos = { 
        EmptyBoard with 
                    WhiteKing = F7;
                    WhitePawns = F5 ||| G6;
                    WhiteKnights = G4;
                    BlackKing = H8;
                    BlackPawns = G7 ||| F6
    }

    let mateline = (findMate startpos 0)
    Assert.True(mateline <> None)

    let expected = [| N G4 H6; p G7 H6; p G6 G7; K H8 H7; p G7 G8 |]
    let current = mateline.Value

    Assert.AreEqual(expected, current)

[<Test>]
let ``puzzle 02``() =
    let startpos = { 
        EmptyBoard with 
                    WhiteKing = F7;
                    WhitePawns = F5 ||| G6;
                    BlackKing = H8;
                    BlackPawns = G7 ||| E5;
    }

    let mateline = (findMate startpos 0)
    let expected = [| p F5 F6; p G7 F6; p G6 G7; K H8 H7; p G7 G8; K H7 H6; Q G8 G6 |]
    
    Assert.AreEqual(expected, mateline.Value)


open Position
open BitOperations

[<Test>]  
let ``Detecting first occupied square in the north``() = 
    let p = { 
        EmptyBoard with 
                    WhiteKing = H1;
                    WhitePawns = A2 ||| C3 ||| D4 ||| F4 ||| H2;
                    WhiteQueens = G5;
                    WhiteBishops = G6;
                    WhiteRooks = H5;
                
                    BlackKing = G7;
                    BlackPawns = A7 ||| B6 ||| C7 ||| D5
                    BlackKnights = G4 ||| H6
                    BlackRooks = A8 ||| F8
    }

    Assert.AreEqual(H6 ||| H7 ||| H8, rayToNFromSquare H5)
    let occupiedSquaresInTheNorth = (rayToNFromSquare H5) &&& p.AllPieces
    Assert.AreEqual(H6, occupiedSquaresInTheNorth)
    Assert.AreEqual(H6, (((rayToNFromSquare H5) &&& p.AllPieces).lsb() |> sqFromIndex))