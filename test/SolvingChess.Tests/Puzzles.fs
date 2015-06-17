﻿module Puzzles

open NUnit.Framework

open BoardUnits
open Position
open Moves
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

    let mateline = (findMate startpos 0 5)
    Assert.True(mateline <> None)
    printfn "%d" numberOfCalls

    let expected = [| N G4 H6; p G7 H6; p G6 G7; K H8 H7; p G7 G8 |]
    let current = mateline.Value

    Assert.AreEqual(expected, current)

[<Test>]
let ``puzzle 02``() =
    let startpos = { 
        EmptyBoard with 
                    WhiteKing = H1;
                    WhitePawns = A2 ||| C3 ||| D4 ||| F4 ||| H2;
                    WhiteQueens = G5;
                    WhiteBishops = G6;
                    WhiteRooks = H5;
                
                    BlackKing = G7;
                    BlackPawns = A7 ||| B6 ||| C7 ||| D5;
                    BlackKnights = G4 ||| H6;
                    BlackRooks = A8 ||| F8;

                    SideToMove = White
    }

    let mateline = (findMate startpos 0 8)
    Assert.True(mateline <> None)
    printfn "%d" numberOfCalls

    let expected = [| B G6 F5; K G7 F7; Q G5 G6; K F7 E7; Q G6 E6; K E7 D8; Q E6 D7 |]
    let current = mateline.Value

    Assert.AreEqual(expected, current)

[<Test>]
let ``puzzle 03``() =
    let startpos = { 
        EmptyBoard with 
                    WhiteKing = G1;
                    WhitePawns = A3 ||| B4 ||| C3 ||| D4 ||| E4 ||| F5 ||| G2 ||| H3;
                    WhiteQueens = F2;
                    WhiteBishops = E3 ||| H5;
                    WhiteRooks = D1 |||D3;
                    WhiteKnights = G3 ||| G4;
                
                    BlackKing = H7;
                    BlackPawns = A4 ||| B5 ||| C7 ||| D6 ||| E5 ||| F6 ||| G5 ||| H6;
                    BlackQueens = F8;
                    BlackBishops = B7 ||| G7;
                    BlackRooks = B8 ||| H8;
                    BlackKnights = C6 ||| D7;

                    SideToMove = White
    }

    let mateline = (findMate startpos 0 8)
    printfn "%d" numberOfCalls
    Assert.True(mateline <> None)

    let expected = [| B H5 G6; K H7 G8; Q F2 A2; p D6 D5; Q A2 D5; Q F8 F7; Q D5 F7 |]
    let current = mateline.Value

    Assert.AreEqual(expected, current)

[<Test>]
let ``puzzle 04``() = 
    let startpos = { 
        EmptyBoard with 
                    WhiteKing = G1;
                    WhitePawns = A2 ||| C2 ||| E3 ||| F2 ||| G2 ||| H2;
                    WhiteBishops = A3;
                    WhiteRooks = B1 |||D1;
                    WhiteKnights = G7;
                
                    BlackKing = F7;
                    BlackPawns = A6 ||| B7 ||| E5 ||| F6 ||| G6 ||| H7;
                    BlackBishops = C3 ||| C8;
                    BlackRooks = A8 ||| H8;
                    BlackKnights = G8;

                    SideToMove = White
    }

    let mateline = (findMate startpos 0 10)
    printfn "%d" numberOfCalls
    Assert.True(mateline <> None)

    let expected = R B1 B7
    let current = Array.head (mateline.Value)

    Assert.AreEqual(expected, current)

[<Test>]
let ``puzzle 05``() =
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

    let mateline = (findMate startpos 0 8)
    printfn "%d" numberOfCalls
    Assert.True(mateline <> None)

    let expected = [| Q C6 A8; K B8 A8; N D5 B6; K A8 B8; N B6 D7 |]
    let current = mateline.Value

    Assert.AreEqual(expected, current)


[<Test>]
let ``puzzle x02``() =
    let startpos = { 
        EmptyBoard with 
                    WhiteKing = F7;
                    WhitePawns = F5 ||| G6;
                    BlackKing = H8;
                    BlackPawns = G7 ||| E5;
    }

    let mateline = (findMate startpos 0 7)
    printfn "%d" numberOfCalls
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