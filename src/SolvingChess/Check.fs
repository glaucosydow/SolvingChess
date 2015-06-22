module Check

open BitOperations
open BoardUnits
open Position
open Attacks

let isBlackKingUnderAttack (position: Position) =
    let allpieces = position.AllPieces

    let condition = 
        ((knightAttacks         position.BlackKing) &&& position.WhiteKnights) <> 0UL ||
        ((whitePawnAttackers    position.BlackKing) &&& position.WhitePawns) <> 0UL ||
        ((kingAttacks           position.BlackKing) &&& position.WhiteKing) <> 0UL 

    if condition 
    then true 
    else 
        let ra = rookAttacks position.BlackKing allpieces
        if ((ra &&& position.WhiteRooks) <> 0UL || (ra &&& position.WhiteQueens) <> 0UL)
        then true
        else
            let ba = bishopAttacks position.BlackKing allpieces
            ((ba &&& position.WhiteBishops) <> 0UL || (ba &&& position.WhiteQueens) <> 0UL)

let isWhiteKingUnderAttack (position: Position) =
    let allpieces = position.AllPieces

    let condition = 
        ((knightAttacks      position.WhiteKing) &&& position.BlackKnights) <> 0UL ||
        ((blackPawnAttackers position.WhiteKing) &&& position.BlackPawns) <> 0UL ||
        ((kingAttacks        position.WhiteKing) &&& position.BlackKing) <> 0UL 

    if condition 
    then true 
    else 
        let ra = rookAttacks position.WhiteKing allpieces
        if ((ra &&& position.BlackRooks) <> 0UL || (ra &&& position.BlackQueens) <> 0UL)
        then true
        else
            let ba = bishopAttacks position.WhiteKing allpieces
            ((ba &&& position.BlackBishops) <> 0UL || (ba &&& position.BlackQueens) <> 0UL)


let isKingUnderAttack side position =
    match side with 
    | White -> position.WhiteKing <> 0UL && isWhiteKingUnderAttack position
    | Black -> position.BlackKing <> 0UL && isBlackKingUnderAttack position

let isCheck position = 
    isKingUnderAttack position.SideToMove position

let private couldKingMove position =
    match position.SideToMove with 
    | White -> ((kingAttacks position.WhiteKing) &&& ~~~(position.WhitePieces) &&& ~~~(blackAttacks position true)) <> 0UL
    | Black -> ((kingAttacks position.BlackKing) &&& ~~~(position.BlackPieces) &&& ~~~(whiteAttacks position true)) <> 0UL

let isCheckMate position =
    (isCheck position) && not (couldKingMove position)


