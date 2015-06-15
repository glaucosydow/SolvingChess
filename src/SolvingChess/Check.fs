module Check

open BitOperations
open Position
open Attacks

let isKingUnderAttack side position =
    match side with 
    | White -> position.WhiteKing <> 0UL && (isSet position.WhiteKing (blackAttacks position false))
    | Black -> position.BlackKing <> 0UL && (isSet position.BlackKing (whiteAttacks position false))

let isCheck position = 
    isKingUnderAttack position.SideToMove position

let private couldKingMove position =
    match position.SideToMove with 
    | White -> ((kingAttacks position.WhiteKing) &&& ~~~(position.WhitePieces) &&& ~~~(blackAttacks position false)) <> 0UL
    | Black -> ((kingAttacks position.BlackKing) &&& ~~~(position.BlackPieces) &&& ~~~(whiteAttacks position false)) <> 0UL

let isCheckMate position =
    let a = (isCheck position)
    (isCheck position) && not (couldKingMove position)


