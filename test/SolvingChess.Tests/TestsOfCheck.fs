module TestsOfCheck

open BoardUnits
open Position
open Check

open NUnit.Framework

[<Test>]
let ``White queen at d7 and black king in d8 is check``() =
    let position = { EmptyBoard with WhiteQueens = D7; BlackKing=D8; SideToMove=Black}
    Assert.True(isCheck position)


[<Test>]
let ``Unprotected White queen at d7 and black king in d8 is not checkmate``() =
    let position = { EmptyBoard with WhiteQueens = D7; BlackKing=D8; SideToMove=Black}
    Assert.False(isCheckMate position)

[<Test>]
let ``Protected White queen at d7 and black king in d8 is checkmate``() =
    let position = { EmptyBoard with WhitePawns=C6; WhiteQueens = D7; BlackKing=D8; SideToMove=Black}
    Assert.True(isCheckMate position)

[<Test>]
let ``White queen covered by the king mate``() =
    let position = { EmptyBoard with WhiteKing=F7; WhitePawns=F5; WhiteQueens=G8; BlackKing=H7; BlackPawns=F6 ||| H6 ; SideToMove = Black}
    Assert.True(isCheckMate position)

[<Test>]
let ``White queen in g8 checks black king in h7``() =
    let position = { EmptyBoard with WhiteKing=F7; WhitePawns=F5; WhiteQueens=G8; BlackKing=H7; BlackPawns=F6 ||| H6 ; SideToMove = Black}
    Assert.True(isCheck position)
