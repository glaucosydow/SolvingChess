module TestsOfPosition

open NUnit.Framework
open Position
open BoardUnits

[<Test>]
let ``White Queen + Knight score is 12``() =
    let pos = {EmptyBoard with WhiteQueens=D1; WhiteKnights=B1}
    Assert.AreEqual(pos.score, 12)


[<Test>]
let ``Black Queen + Knight score is -12``() =
    let pos = {EmptyBoard with BlackQueens=D1; BlackKnights=B1}
    Assert.AreEqual(pos.score, -12)

