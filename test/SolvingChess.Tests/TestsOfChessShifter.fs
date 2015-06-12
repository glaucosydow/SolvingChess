module TestsOfChessShifter

open NUnit.Framework

open BoardUnits
open Position
open BoardShifter

[<Test>]
let ``shifted board applies specified rank and file offsets``() =
    let initialPos = (sq 'a' 1)
    let shiftedPos = initialPos.chessShift 2 2
    Assert.AreEqual(shiftedPos, (sq 'c' 3))

[<Test>]
let ``d4 shifted 1, 1 is e5``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift 1 1
    Assert.AreEqual(shiftedPos, (sq 'e' 5))

[<Test>]
let ``d4 shifted 1, 0 is d5``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift 1 0
    Assert.AreEqual(shiftedPos, (sq 'd' 5))


[<Test>]
let ``d4 shifted -1, 1 is e3``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift -1 1
    Assert.AreEqual(shiftedPos, (sq 'e' 3))

[<Test>]
let ``d4 shifted 1, -1 is c5``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift  1 -1
    Assert.AreEqual(shiftedPos, (sq 'c' 5))

[<Test>]
let ``d4 shifted -1, -1 is c3``() =
    let initialPos = (sq 'd' 4)
    let shiftedPos = initialPos.chessShift -1 -1
    Assert.AreEqual(shiftedPos, (sq 'c' 3))
    