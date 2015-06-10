module Check

open BitOperations
open Position
open Attacks

let isCheck position = 
    match position.SideToMove with 
    | White -> position.WhiteKing <> 0UL && (isSet position.WhiteKing (blackAttacks position))
    | Black -> position.BlackKing <> 0UL && (isSet position.BlackKing (whiteAttacks position))

let private couldKingMove position =
    match position.SideToMove with 
    | White -> ((kingAttacks position.WhiteKing) &&& ~~~(position.WhitePieces) &&& ~~~(blackAttacks position)) <> 0UL
    | Black -> ((kingAttacks position.BlackKing) &&& ~~~(position.BlackPieces) &&& ~~~(whiteAttacks position)) <> 0UL

let isCheckMate position =
    (isCheck position) && not (couldKingMove position)


