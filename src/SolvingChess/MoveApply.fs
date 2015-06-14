module MoveApply

open BitOperations
open BoardUnits
open Position
open MoveGen

let inline private a move bitboard =
    if (isSet move.From bitboard) then 
        bitboard &&& ~~~(move.From) ||| move.To
    else
        if (isSet move.To bitboard) then 
            bitboard &&& ~~~(move.To)
        else
            bitboard


let applyMove move position =
    {
        WhiteKing =    a move position.WhiteKing
        WhiteQueens =  a move position.WhiteQueens ||| (if move.Promotion=Queen then move.To &&& (rank 7) else 0UL)
        WhiteBishops = a move position.WhiteBishops
        WhiteKnights = a move position.WhiteKnights
        WhiteRooks   = a move position.WhiteRooks
        WhitePawns   = a move position.WhitePawns &&& ~~~(rank 7)

        BlackKing =    a move position.BlackKing
        BlackQueens =  a move position.BlackQueens ||| (if move.Promotion=Queen then move.To  &&& (rank 0) else 0UL)
        BlackBishops = a move position.BlackBishops
        BlackKnights = a move position.BlackKnights
        BlackRooks   = a move position.BlackRooks
        BlackPawns   = a move position.BlackPawns &&& ~~~(rank 0)
        
        SideToMove = (if position.SideToMove=Black then White else Black)
    }
