module TestsOfChessShifter

open Xunit

open BoardUnits
open Position
open BoardShifter

[<Fact>]
let ``shifted board applies specified rank and file offsets``() =
    let initialPos = (sq 'a' 1)
    let shiftedPos = initialPos.chessShift 2 2
    Assert.Equal(shiftedPos, (sq 'c' 3))

[<Fact>]
let ``d4 shifted 1, 1 is e5``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift 1 1
    Assert.Equal(shiftedPos, (sq 'e' 5))

[<Fact>]
let ``d4 shifted 1, 0 is d5``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift 1 0
    Assert.Equal(shiftedPos, (sq 'd' 5))


[<Fact>]
let ``d4 shifted -1, 1 is e3``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift -1 1
    Assert.Equal(shiftedPos, (sq 'e' 3))

[<Fact>]
let ``d4 shifted 1, -1 is c5``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift  1 -1
    Assert.Equal(shiftedPos, (sq 'c' 5))

[<Fact>]
let ``d4 shifted -1, -1 is c3``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift -1 -1
    Assert.Equal(shiftedPos, (sq 'c' 3))
    