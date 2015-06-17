module TestsOfMoveGen

open NUnit.Framework
open BoardUnits
open Position
open Moves

[<Test>]
let ``Detects white pawns advances``() =
    let position = { EmptyBoard with WhitePawns= (sq 'b' 2) ||| (sq 'c' 3); BlackPawns = (sq 'b' 3) }
    let move = moves position |> Seq.head
    Assert.AreEqual({Piece=Pawn; From=(sq 'c' 3);To=(sq 'c' 4); Promotion=Undefined}, move)

[<Test>]
let ``Advancing Pawn from d7 promotes it to queen``() =
    let position = { EmptyBoard with WhitePawns= D7 }
    let move = moves position |> Seq.head
    Assert.AreEqual({Piece=Pawn; From=D7;To=D8; Promotion=Queen}, move)


[<Test>]
let ``Detects white pawns captures``() =
    let position = { EmptyBoard with WhitePawns= (sq 'b' 2); BlackPawns = (sq 'b' 3)  ||| (sq 'c' 3)}
    let move = moves position |> Seq.head
    Assert.AreEqual({Piece=Pawn; From=(sq 'b' 2);To=(sq 'c' 3); Promotion=Undefined}, move)
    
[<Test>]
let ``Detects black pawns advances``() =
    let position = { EmptyBoard with BlackPawns= B7 ||| C6; WhitePawns = B6; SideToMove=Black }
    let move = moves position |> Seq.head
    Assert.AreEqual({Piece=Pawn; From=C6;To=C5; Promotion=Undefined}, move)


[<Test>]
let ``Detects black pawns captures``() =
    let position = { EmptyBoard with BlackPawns= (sq 'b' 7) ; WhitePawns = (sq 'b' 6) ||| (sq 'c' 6); SideToMove=Black }
    let move = moves position |> Seq.head
    Assert.AreEqual({Piece=Pawn; From=(sq 'b' 7);To=(sq 'c' 6); Promotion=Undefined}, move)
 
[<Test>]
let ``Detects white pawn advance and promotion``() =
    let position = { EmptyBoard with WhiteKing=F7; WhitePawns=F5 ||| G7; BlackKing=H7; BlackPawns=F6 ||| H6 }
    let move = moves position |> Seq.head
    Assert.AreEqual({Piece=Pawn; From=G7; To=G8; Promotion=Queen}, move)

[<Test>]
let ``Detects black pawn advance and pawn capture``() =
    let position = {
        EmptyBoard with WhiteKing=F7; BlackKing = H8; WhitePawns = F6 ||| G6; BlackPawns=E5 ||| G7; SideToMove=Black
    }
    let m = moves position |> Seq.toArray
    Assert.AreEqual([| p E5 E4; p G7 F6 |], m)

[<Test>]
let ``King at D7 escapes from queen at D5 check``() =
    let position = {
        EmptyBoard with BlackKing=D7; WhiteQueens=D5; SideToMove=Black
    }
    let m = moves position |> Seq.toArray
    let k = K D7
    Assert.AreEqual([| k C7; k E7; k C8; k E8 |], m)

[<Test>]
let ``Queen at C6 can go to A8``()=
    let position = {
        EmptyBoard with WhiteQueens = C6
    }
    let m = moves position |> Seq.toArray
    Assert.Contains(Q C6 A8, m)


[<Test>]
let ``Queen at C6 can go to A8 - full position``()=
    let position = { 
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
    let m = moves position |> Seq.toArray
    Assert.Contains(Q C6 A8, m)

