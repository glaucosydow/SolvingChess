module TestsOfMoveApply

open Xunit
open BoardUnits
open Position
open MoveGen
open MoveApply

[<Fact>]
let ``Capture of pawn with knight``() =
    let position = { EmptyBoard with WhiteKnights = A1; BlackPawns = B3 }
    let move = { Piece = Knight; From=A1; To=B3; Promotion=Undefined }
    let expected = { EmptyBoard with WhiteKnights = B3; SideToMove=Black} 
    let result = applyMove move position
    Assert.Equal(expected, result)

[<Fact>]
let ``Promotes a white pawn that gets the eigth rank``() =
    let position = {EmptyBoard with WhitePawns = A7}
    let move = { Piece = Pawn; From=A7; To=A8; Promotion=Queen}
    let expected = {EmptyBoard with WhiteQueens = A8; SideToMove=Black}
    Assert.Equal(expected, applyMove move position)
    

[<Fact>]
let ``Simple black king move``() =
    let position = { EmptyBoard with BlackKing=H8; SideToMove=Black }
    let newposition = applyMove (K H8 H7) position
    Assert.Equal({ EmptyBoard with BlackKing=H7; SideToMove=White}, newposition)

[<Fact>]
let ``Detects white pawn advance and promotion sequence``() =
    let position = { EmptyBoard with WhiteKing=F7; WhitePawns=F5 ||| G7; BlackKing=H8; BlackPawns=F6 ||| H6; SideToMove=Black }
    let move = moves position |> Seq.head
    Assert.Equal((K H8 H7), move)
    let newPosition = applyMove move position
    let expected = { EmptyBoard with WhiteKing=F7; WhitePawns=F5 ||| G7; BlackKing=H7; BlackPawns=F6 ||| H6 }
    Assert.Equal(expected, newPosition)
    let lastmove = moves newPosition |> Seq.head
    Assert.Equal({Piece=Pawn; From=G7; To=G8; Promotion=Queen}, lastmove)


