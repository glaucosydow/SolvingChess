module TestsOfBoardUnits

open NUnit.Framework
open BoardUnits

[<Test>]
let ``rankOfSquare e5 is rank 4``() =
    Assert.AreEqual((sq 'e' 5) |> rankOfSquare, rank 4)

[<Test>]
let ``fileOfSquare e5 is file 4``() =
    Assert.AreEqual((sq 'e' 5) |> fileOfSquare, file 4)

[<Test>]
let ``fileIndexOfSquare e5 is 4``() =
    Assert.AreEqual((sq 'e' 5) |> fileIndexOfSquare, 4)

[<Test>]
let ``rankIndexOfSquare e5 is 4``() =
    Assert.AreEqual((sq 'e' 5) |> rankIndexOfSquare, 4)

[<Test>]
let ``rayToNFromSquare e5 is e6, e7, e8``() =
    Assert.AreEqual((sq 'e' 5) |> rayToNFromSquare, (sq 'e' 6) ||| (sq 'e' 7) ||| (sq 'e' 8))

[<Test>]
let ``rayToSFromSquare e5 is e4, e3, e2, e1``() =
    Assert.AreEqual((sq 'e' 5) |> rayToSFromSquare, (sq 'e' 4) ||| (sq 'e' 3) ||| (sq 'e' 2) ||| (sq 'e' 1))

[<Test>]
let ``rayToEFromSquare e5 is f5, g5, h5``() =
    Assert.AreEqual((sq 'e' 5) |> rayToEFromSquare, (sq 'f' 5) ||| (sq 'g' 5) ||| (sq 'h' 5))

[<Test>]
let ``rayToWFromSquare e5 is d5, c5, b5, a5``() =
    Assert.AreEqual((sq 'e' 5) |> rayToWFromSquare, (sq 'd' 5) ||| (sq 'c' 5) ||| (sq 'b' 5) ||| (sq 'a' 5))

[<Test>]
let ``diagonalNEFromSquare d3 is b1, c2, d3, e4, f5, g6, h7``() =
    let expected = B1 ||| C2 ||| D3 ||| E4 ||| F5 ||| G6 ||| H7
    Assert.AreEqual(expected, D3 |> diagonalNEOfSquare)

[<Test>]
let ``diagonalNWFromSquare d3 is a6, b5, c4, d3, e2, f1``() =
    let expected = A6 ||| B5 ||| C4 ||| D3 ||| E2 ||| F1
    Assert.AreEqual(expected, D3 |> diagonalNWOfSquare)

[<Test>]
let ``rayToNWFromSquare d3 is a6, b5, c4``() =
    let expected = A6 ||| B5 ||| C4
    Assert.AreEqual(expected, D3 |> rayToNWFromSquare)

[<Test>]
let ``rayToNEFromSquare d3 is e4, f5, g6, h7``() =
    let expected = E4 ||| F5 ||| G6 ||| H7
    Assert.AreEqual(expected, D3 |> rayToNEFromSquare)

[<Test>]
let ``rayToSWFromSquare d3 is c2, b1``() =
    let expected = C2 ||| B1
    Assert.AreEqual(expected, D3 |> rayToSWFromSquare)

[<Test>]
let ``rayToSWFromSquare h3 is g2, f1``() =
    let expected = G2 ||| F1
    Assert.AreEqual(expected, H3 |> rayToSWFromSquare)

[<Test>]
let ``rayToSEFromSquare d3 is e2, f1``() =
    let expected = E2 ||| F1
    Assert.AreEqual(expected, D3 |> rayToSEFromSquare)

[<Test>]
let ``rayToSEFromSquare c3 is d2, e1``() =
    let expected = D2 ||| E1
    Assert.AreEqual(expected, C3 |> rayToSEFromSquare)

[<Test>]
let ``rayToSEFromSquare e4 is f3, g2, h1``() =
    let expected = F3 ||| G2 ||| H1
    Assert.AreEqual(expected, E4 |> rayToSEFromSquare)

[<Test>]
let ``rayToSEFromSquare c4 is d3, e2, f1``() =
    let expected = D3 ||| E2 ||| F1
    Assert.AreEqual(expected, C4 |> rayToSEFromSquare)

[<Test>]
let ``diagonalNWFromSquare c4 is a6, b5, c4, d3, e2, f1``() =
    let expected = A6 ||| B5 ||| C4 ||| D3 ||| E2 ||| F1
    Assert.AreEqual(expected, C4 |> diagonalNWOfSquare)

