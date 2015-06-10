module Position

open BoardUnits

type Sides =
    | White
    | Black

type Position = {
        WhiteKing: Bitboard;
        WhitePawns: Bitboard;
        WhiteKnights: Bitboard;
        WhiteQueens: Bitboard;
        BlackKing: Bitboard;
        BlackPawns: Bitboard;
        SideToMove: Sides;
}
with 
    member x.WhitePieces = x.WhiteKing ||| x.WhitePawns ||| x.WhiteKnights ||| x.WhiteQueens
    member x.BlackPieces = x.BlackKing ||| x.BlackPawns
    member x.AllPieces = x.WhitePieces ||| x.BlackPieces


let EmptyBoard = {
    WhiteKing = 0UL;
    WhitePawns = 0UL;
    WhiteKnights = 0UL;
    WhiteQueens = 0UL;
    BlackKing = 0UL;
    BlackPawns = 0UL;
    SideToMove = White
}


