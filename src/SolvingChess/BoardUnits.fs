module BoardUnits

open BoardShifter
open BitOperations

type Bitboard = uint64

let inline sqFromIndex index = 1UL <<< index

let inline pos (ri) (fi) : Bitboard = (ri * 8 + fi) |> sqFromIndex

let inline sq file rank = pos (rank - 1) (int (byte file) - int (byte 'a'))

let A1 = sq 'a' 1
let A2 = sq 'a' 2
let A3 = sq 'a' 3
let A4 = sq 'a' 4
let A5 = sq 'a' 5
let A6 = sq 'a' 6
let A7 = sq 'a' 7
let A8 = sq 'a' 8

let B1 = sq 'b' 1
let B2 = sq 'b' 2
let B3 = sq 'b' 3
let B4 = sq 'b' 4
let B5 = sq 'b' 5
let B6 = sq 'b' 6
let B7 = sq 'b' 7
let B8 = sq 'b' 8

let C1 = sq 'c' 1
let C2 = sq 'c' 2
let C3 = sq 'c' 3
let C4 = sq 'c' 4
let C5 = sq 'c' 5
let C6 = sq 'c' 6
let C7 = sq 'c' 7
let C8 = sq 'c' 8

let D1 = sq 'd' 1
let D2 = sq 'd' 2
let D3 = sq 'd' 3
let D4 = sq 'd' 4
let D5 = sq 'd' 5
let D6 = sq 'd' 6
let D7 = sq 'd' 7
let D8 = sq 'd' 8

let E1 = sq 'e' 1
let E2 = sq 'e' 2
let E3 = sq 'e' 3
let E4 = sq 'e' 4
let E5 = sq 'e' 5
let E6 = sq 'e' 6
let E7 = sq 'e' 7
let E8 = sq 'e' 8

let F1 = sq 'f' 1
let F2 = sq 'f' 2
let F3 = sq 'f' 3
let F4 = sq 'f' 4
let F5 = sq 'f' 5
let F6 = sq 'f' 6
let F7 = sq 'f' 7
let F8 = sq 'f' 8

let G1 = sq 'g' 1
let G2 = sq 'g' 2
let G3 = sq 'g' 3
let G4 = sq 'g' 4
let G5 = sq 'g' 5
let G6 = sq 'g' 6
let G7 = sq 'g' 7
let G8 = sq 'g' 8

let H1 = sq 'h' 1
let H2 = sq 'h' 2
let H3 = sq 'h' 3
let H4 = sq 'h' 4
let H5 = sq 'h' 5
let H6 = sq 'h' 6
let H7 = sq 'h' 7
let H8 = sq 'h' 8


let inline rank index =
    chessShift index 0 (255UL)

let inline file index = 
    chessShift 0 index (72340172838076673UL)

let inline rankIndexOfSquare sq = ((lsb sq) >>> 3)

let inline rankOfSquare sq =
     sq |> rankIndexOfSquare |> rank


let inline fileIndexOfSquare sq = 
    ((lsb sq) &&& 7)

let inline fileOfSquare sq = 
    sq |> fileIndexOfSquare |> file 

let inline diagonalNEOfSquare sq =
    let index = (sq |> fileIndexOfSquare) - (sq |> rankIndexOfSquare) + 7
    let reference = H8 ||| G7 ||| F6 ||| E5 ||| D4 ||| C3 ||| B2 ||| A1
    match index with 
    | i when i <= 7 -> reference.chessShift (7-i)  0
    | i  -> reference.chessShift 0 (i-7)

let inline diagonalNWOfSquare sq =
    let index = (sq |> fileIndexOfSquare) + (sq |> rankIndexOfSquare) 
    let reference = H1 ||| G2 ||| F3 ||| E4 ||| D5 ||| C6 ||| B7 ||| A8
    match index with 
    | i when i <= 7 -> reference.chessShift (i-7)  0
    | i  -> reference.chessShift 0 (i-7)

let inline rayToNFromSquare sq =
    let f = fileOfSquare sq
    let ri = rankIndexOfSquare sq
    chessShift ri 0 f |> except sq

let inline rayToSFromSquare sq =
    let f = fileOfSquare sq
    let ri = rankIndexOfSquare sq
    chessShift (ri - 7) 0 f |> except sq

let inline rayToEFromSquare sq =
    let r = rankOfSquare sq
    let fi = fileIndexOfSquare sq
    chessShift 0 (fi) r |> except sq

let inline rayToWFromSquare sq =
    let r = rankOfSquare sq
    let fi = fileIndexOfSquare sq
    chessShift 0 (fi - 7) r |> except sq

let rec private upside r =
    match r with 
    | ri when ri > 7 -> 0UL
    | ri -> (rank r) ||| (upside (r + 1))


let rec private downside r =
    match r with 
    | ri when ri < 0 -> 0UL
    | ri -> (rank r) ||| (downside (r - 1))

let rayToNEFromSquarePC = Array.zeroCreate<Bitboard> 64
let rayToNWFromSquarePC = Array.zeroCreate<Bitboard> 64
let rayToSWFromSquarePC = Array.zeroCreate<Bitboard> 64
let rayToSEFromSquarePC = Array.zeroCreate<Bitboard> 64

for index = 0 to 63 do
    let sq = sqFromIndex index
    rayToNEFromSquarePC.[index] <- except sq ((diagonalNEOfSquare sq) &&& (upside (rankIndexOfSquare sq)))
    rayToNWFromSquarePC.[index] <- except sq ((diagonalNWOfSquare sq) &&& (upside (rankIndexOfSquare sq)))
    rayToSWFromSquarePC.[index] <- except sq ((diagonalNEOfSquare sq) &&& (downside (rankIndexOfSquare sq)))
    rayToSEFromSquarePC.[index] <- except sq ((diagonalNWOfSquare sq) &&& (downside (rankIndexOfSquare sq)))

let inline rayToNEFromSquare sq =
    rayToNEFromSquarePC.[lsb sq]

let inline rayToNWFromSquare sq =
    rayToNWFromSquarePC.[lsb sq]

let inline rayToSWFromSquare sq =
    rayToSWFromSquarePC.[lsb sq]

let inline rayToSEFromSquare sq =
    rayToSEFromSquarePC.[lsb sq]
    
let inline sqToString sq =
    sprintf "%c%d" [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'|].[fileIndexOfSquare sq] ( (rankIndexOfSquare sq) + 1)

let rec enumerateSquares bitboard = seq {
    if bitboard <> 0UL then
        let square = bitboard.lsb() |> sqFromIndex;
        yield square
        let remaining = (except square bitboard)
        if remaining > 0UL then yield! enumerateSquares remaining
}

