module TestsOfAttacks

open NUnit.Framework
open BoardUnits
open BitOperations
open Position
open Attacks

[<Test>]
let ``White Pawns at d4, e4 attacks c5, d5, e5,f5``() =
    let pawns = (sq 'd' 4) ||| (sq 'e' 4)
    let attacks = whitePawnsAttacks pawns
    Assert.AreEqual(attacks, (sq 'c' 5) ||| (sq 'd' 5) ||| (sq 'e' 5) ||| (sq 'f' 5))


[<Test>]
let ``Knight at a1 attacks b3, c2``() =
    let knight = (sq 'a' 1)
    let attacks = knightsAttacks knight
    Assert.AreEqual(attacks, (sq 'c' 2) ||| (sq 'b' 3))

[<Test>]
let ``Rook at e5 attacks file e and rank 5 except e5``() =
    let rooks = (sq 'e' 5)
    let attacks = rooksAttacks rooks rooks 0UL
    Assert.AreEqual(attacks, (except rooks (rank 4 ||| file 4)))

[<Test>]
let ``Bishop at e5 attacks related diagonals NW and NW except e5``() =
    let bishops = E5
    let attacks = bishopsAttacks bishops bishops 0UL
    Assert.AreEqual(attacks, (except E5 ((diagonalNWOfSquare E5) ||| (diagonalNEOfSquare E5))))

[<Test>]
let ``Queen at g8 attacks h7``() =
    let queens = G8
    let attacks = queensAttacks queens queens 0UL
    Assert.True(isSet H7 attacks)


[<Test>]
let ``Queen at c6 attacks a8``() =
    let queens = C6
    let attacks = queensAttacks queens queens 0UL
    Assert.True(isSet A8 attacks)


[<Test>]  
let ``Rook attacks considers obstacles``() = 
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

    let result = rooksAttacks p.WhiteRooks p.WhitePieces p.BlackPieces
    let expected = H4 ||| H3 ||| H2 ||| H6 ||| G5
    Assert.AreEqual (expected, result) 


