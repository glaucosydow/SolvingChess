module TestsOfAttacks

open Xunit
open BoardUnits
open BitOperations
open Position
open Attacks

[<Fact>]
let ``White Pawns at d4, e4 attacks c5, d5, e5,f5``() =
    let pawns = (sq 'd' 4) ||| (sq 'e' 4)
    let attacks = whitePawnsAttacks pawns
    Assert.Equal(attacks, (sq 'c' 5) ||| (sq 'd' 5) ||| (sq 'e' 5) ||| (sq 'f' 5))


[<Fact>]
let ``Knight at a1 attacks b3, c2``() =
    let knight = (sq 'a' 1)
    let attacks = knightsAttacks knight
    Assert.Equal(attacks, (sq 'c' 2) ||| (sq 'b' 3))

[<Fact>]
let ``Rook at e5 attacks file e and rank 5 except e5``() =
    let rooks = (sq 'e' 5)
    let attacks = rooksAttacks rooks rooks 0UL
    Assert.Equal(attacks, (except rooks (rank 4 ||| file 4)))

[<Fact>]
let ``Bishop at e5 attacks related diagonals NW and NW except e5``() =
    let bishops = E5
    let attacks = bishopsAttacks bishops bishops 0UL
    Assert.Equal(attacks, (except E5 ((diagonalNWOfSquare E5) ||| (diagonalNEOfSquare E5))))

[<Fact>]
let ``Queen at g8 attacks h7``() =
    let queens = G8
    let attacks = queensAttacks queens queens 0UL
    Assert.True(isSet H7 attacks)


[<Fact>]  
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
    let expected = H4 ||| H3 ||| H6
    Assert.Equal (expected, result) 


