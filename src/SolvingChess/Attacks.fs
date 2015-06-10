module Attacks

open BoardUnits
open Position
open BoardShifter
open BitOperations

let kingAttacks (kingPosition : Bitboard) = 
    let cs = kingPosition.chessShift
    cs -1 -1 ||| cs -1  0 ||| cs -1 1 ||| cs  0 -1 ||| cs  0 1 |||
                 cs  1 -1 ||| cs  1 0 ||| cs  1 1 

let whitePawnsAttacks (whitePawnsPositions : Bitboard) =
    let cs = whitePawnsPositions.chessShift
    (cs 1 -1 ||| cs 1 1) 

let blackPawnsAttacks (blackPawnsPositions : Bitboard) =
    let cs = blackPawnsPositions.chessShift
    cs -1 -1 ||| cs -1 1

let knightsAttacks (knightsPositions : Bitboard) =
    let cs = knightsPositions.chessShift
    cs 1 2 ||| cs  1 -2 ||| cs  2 1 ||| cs  2 -1 ||| cs -1 2 |||
               cs -1 -2 ||| cs -2 1 ||| cs -2 -1

let rooksAttacks rooksPositions friends enemies =
    let allpieces = friends ||| enemies
    enumerateSquares rooksPositions
    |> Seq.map(fun(sq) -> 
                    rankOfSquare sq ||| fileOfSquare sq 
                    |> except friends
                    |> except (rayToNFromSquare ((rayToNFromSquare sq &&& allpieces).lsb() |> sqFromIndex))
                    |> except (rayToEFromSquare ((rayToEFromSquare sq &&& allpieces).lsb() |> sqFromIndex))
                    |> except (rayToSFromSquare ((rayToSFromSquare sq &&& allpieces).msb() |> sqFromIndex))
                    |> except (rayToWFromSquare ((rayToWFromSquare sq &&& allpieces).msb() |> sqFromIndex))
              )
    |> Seq.fold (fun acc elem -> acc ||| elem) 0UL

let bishopsAttacks bishopsPositions friends enemies =
    let allpieces = friends ||| enemies
    enumerateSquares bishopsPositions
    |> Seq.map(fun(sq) -> 
                    diagonalNWOfSquare sq ||| diagonalNEOfSquare sq 
                    |> except friends
                    |> except (rayToNEFromSquare ((rayToNEFromSquare sq &&& allpieces).lsb() |> sqFromIndex))
                    |> except (rayToNWFromSquare ((rayToNWFromSquare sq &&& allpieces).lsb() |> sqFromIndex))
                    |> except (rayToSEFromSquare ((rayToSEFromSquare sq &&& allpieces).msb() |> sqFromIndex))
                    |> except (rayToSWFromSquare ((rayToSWFromSquare sq &&& allpieces).msb() |> sqFromIndex))
              )
    |> Seq.fold (fun acc elem -> acc ||| elem) 0UL

let queensAttacks queensPositions friends enemies = 
    (rooksAttacks queensPositions friends enemies) ||| (bishopsAttacks queensPositions friends enemies)


let whiteAttacks position = 
    whitePawnsAttacks position.WhitePawns |||
    knightsAttacks position.WhiteKnights  |||
    queensAttacks position.WhiteQueens position.WhitePieces position.BlackPieces |||
    kingAttacks position.WhiteKing        
    

let blackAttacks position =
    blackPawnsAttacks position.BlackPawns ||| 
    kingAttacks position.BlackKing
    