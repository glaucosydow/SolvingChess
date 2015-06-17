module Position
open BitOperations
open BoardUnits

type Sides =
    | White
    | Black

type Position = {
        WhiteKing: Bitboard;
        WhiteQueens: Bitboard;
        WhitePawns: Bitboard;
        WhiteBishops: Bitboard;
        WhiteKnights: Bitboard;
        WhiteRooks: Bitboard;
        
        BlackKing: Bitboard;
        BlackQueens: Bitboard;
        BlackPawns: Bitboard;
        BlackBishops: Bitboard;
        BlackKnights: Bitboard;
        BlackRooks: Bitboard;

        SideToMove: Sides;
}
with 
    member x.WhitePieces = x.WhiteKing ||| x.WhiteQueens ||| x.WhiteBishops ||| x.WhiteKnights ||| x.WhiteRooks ||| x.WhitePawns 
    member x.BlackPieces = x.BlackKing ||| x.BlackQueens ||| x.BlackBishops ||| x.BlackKnights ||| x.BlackRooks ||| x.BlackPawns 
    member x.AllPieces = x.WhitePieces ||| x.BlackPieces
    
    member x.score = 
        let white = x.WhiteQueens.popcount()  * 9 +
                    x.WhiteBishops.popcount() * 3 +
                    x.WhiteKnights.popcount() * 3 +
                    x.WhiteRooks.popcount()   * 5 +
                    x.WhitePawns.popcount()   * 1

        let black = x.BlackQueens.popcount()  * 9 +
                    x.BlackBishops.popcount() * 3 +
                    x.BlackKnights.popcount() * 3 +
                    x.BlackRooks.popcount()   * 5 +
                    x.BlackPawns.popcount()   * 1

        white - black



let EmptyBoard = {
    WhiteKing = 0UL;
    WhiteQueens = 0UL;
    WhitePawns = 0UL;
    WhiteBishops = 0UL
    WhiteKnights = 0UL;
    WhiteRooks = 0UL;
    
    BlackKing = 0UL;
    BlackQueens = 0UL;
    BlackPawns = 0UL;
    BlackBishops = 0UL
    BlackKnights = 0UL;
    BlackRooks = 0UL;

    SideToMove = White
}


