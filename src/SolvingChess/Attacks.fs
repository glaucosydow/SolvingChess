﻿module Attacks

open BoardUnits
open Position
open BoardShifter
open BitOperations

let ka =    Array.zeroCreate<Bitboard> 64
let karea = Array.zeroCreate<Bitboard> 64
let knightAttacksPC =   Array.zeroCreate<Bitboard> 64
let whitePawnAttackersPC = Array.zeroCreate<Bitboard> 64
let blackPawnAttackersPC = Array.zeroCreate<Bitboard> 64
let whiteKingAdvancesPC = Array.zeroCreate<Bitboard> 64

for i = 0 to 63 do
    let s = sqFromIndex i
    let cs = chessShift

    ka.[i] <- cs -1 -1 s ||| cs -1  0 s ||| cs -1 1 s ||| cs  0 -1 s ||| cs  0 1 s |||
                 cs  1 -1 s||| cs  1 0 s ||| cs  1 1 s 
    let a = cs  -1 0 (ka.[i]) ||| cs  1 0 (ka.[i])
    karea.[i] <- cs  0 -1 a ||| cs 0 1 a
    whiteKingAdvancesPC.[i] <- cs 1 -1 s ||| cs 1 0 s ||| cs 1 1 s

    knightAttacksPC.[i] <- cs 1 2 s ||| cs  1 -2 s ||| cs  2 1 s||| cs  2 -1 s||| cs -1 2 s |||
               cs -1 -2 s ||| cs -2 1 s ||| cs -2 -1 s

    whitePawnAttackersPC.[i] <- cs -1 -1 s ||| cs -1 1 s
    blackPawnAttackersPC.[i] <- cs 1 -1 s ||| cs 1 1 s

let inline whitePawnAttackers sq =
    whitePawnAttackersPC.[lsb sq]

let inline blackPawnAttackers sq =
    blackPawnAttackersPC.[lsb sq]

let inline kingAttacks (kingPosition : Bitboard) = 
    ka.[lsb kingPosition]

let inline kingArea (kingPosition: Bitboard) =
    karea.[lsb kingPosition]

let inline whiteKingAdvances kingPosition = 
    whiteKingAdvancesPC.[lsb kingPosition]

let whitePawnsAttacks (whitePawnsPositions : Bitboard) =
    (chessShift  1 -1 whitePawnsPositions) ||| (chessShift 1 1 whitePawnsPositions)

let blackPawnsAttacks (blackPawnsPositions : Bitboard) =
    (chessShift -1 -1 blackPawnsPositions) ||| (chessShift -1 1 blackPawnsPositions)

let inline knightAttacks sq = 
    knightAttacksPC.[lsb sq]

let knightsAttacks (knightsPositions : Bitboard) =
    chessShift  1  2 knightsPositions |||
    chessShift  1 -2 knightsPositions |||
    chessShift  2  1 knightsPositions |||
    chessShift  2 -1 knightsPositions |||
    chessShift -1  2 knightsPositions |||
    chessShift -1 -2 knightsPositions |||
    chessShift -2  1 knightsPositions |||
    chessShift -2 -1 knightsPositions

let inline rookAttacks sq allpieces = 
    let a = except sq (rankOfSquare sq ||| fileOfSquare sq)
    let b = except (rayToNFromSquare (((rayToNFromSquare sq) &&& allpieces).lsb() |> sqFromIndex)) a
    let c = except (rayToEFromSquare (((rayToEFromSquare sq) &&& allpieces).lsb() |> sqFromIndex)) b
    let d = except (rayToSFromSquare (((rayToSFromSquare sq) &&& allpieces).msb() |> sqFromIndex)) c
    except (rayToWFromSquare (((rayToWFromSquare sq) &&& allpieces).msb() |> sqFromIndex)) d

let rooksAttacks rooksPositions friends enemies =
    let allpieces = friends ||| enemies
    enumerateSquares rooksPositions
    |> Seq.map(fun(sq) -> rookAttacks sq allpieces)
    |> Seq.fold (fun acc elem -> acc ||| elem) 0UL

let inline bishopAttacks sq allpieces =
    let a = except sq (diagonalNWOfSquare sq ||| diagonalNEOfSquare sq)
    let b = except (rayToNEFromSquare (((rayToNEFromSquare sq) &&& allpieces).lsb() |> sqFromIndex)) a
    let c = except (rayToNWFromSquare (((rayToNWFromSquare sq) &&& allpieces).lsb() |> sqFromIndex)) b
    let d = except (rayToSEFromSquare (((rayToSEFromSquare sq) &&& allpieces).msb() |> sqFromIndex)) c
    except (rayToSWFromSquare (((rayToSWFromSquare sq) &&& allpieces).msb() |> sqFromIndex)) d

let bishopsAttacks bishopsPositions friends enemies =
    let allpieces = friends ||| enemies
    enumerateSquares bishopsPositions
    |> Seq.map(fun(sq) -> bishopAttacks sq allpieces)
    |> Seq.fold (fun acc elem -> acc ||| elem) 0UL

let inline queenAttacks sq allpieces = 
    (bishopAttacks sq allpieces) ||| (rookAttacks sq allpieces)

let queensAttacks queensPositions friends enemies = 
    (rooksAttacks queensPositions friends enemies) ||| (bishopsAttacks queensPositions friends enemies)

let whiteAttacks (position:Position) ignoresBlackKing =
    let enemies = if not ignoresBlackKing 
                  then position.BlackPieces 
                  else position.BlackPieces &&& ~~~(position.BlackKing)

    queensAttacks       position.WhiteQueens  position.WhitePieces enemies |||
    rooksAttacks        position.WhiteRooks   position.WhitePieces enemies |||
    bishopsAttacks      position.WhiteBishops position.WhitePieces enemies |||
    knightsAttacks      position.WhiteKnights  |||
    whitePawnsAttacks   position.WhitePawns |||
    kingAttacks         position.WhiteKing        

let blackAttacks (position:Position) ignoresWhiteKing =
    let enemies = if not ignoresWhiteKing
                  then position.WhitePieces 
                  else position.WhitePieces &&& ~~~(position.WhiteKing)

    queensAttacks       position.BlackQueens  position.BlackPieces enemies |||
    rooksAttacks        position.BlackRooks   position.BlackPieces enemies |||
    bishopsAttacks      position.BlackBishops position.BlackPieces enemies |||
    knightsAttacks      position.BlackKnights  |||
    blackPawnsAttacks   position.BlackPawns |||
    kingAttacks         position.BlackKing