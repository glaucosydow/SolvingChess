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
        let white = x.WhiteQueens.popcount()  * 9UL +
                    x.WhiteBishops.popcount() * 3UL +
                    x.WhiteKnights.popcount() * 3UL +
                    x.WhiteRooks.popcount()   * 5UL +
                    x.WhitePawns.popcount()   * 1UL

        let black = x.BlackQueens.popcount()  * 9UL +
                    x.BlackBishops.popcount() * 3UL +
                    x.BlackKnights.popcount() * 3UL +
                    x.BlackRooks.popcount()   * 5UL +
                    x.BlackPawns.popcount()   * 1UL

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


