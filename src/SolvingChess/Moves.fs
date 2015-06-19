module Moves

open BitOperations
open BoardShifter
open Position
open BoardUnits
open Attacks
open Check

type Pieces =
    | King
    | Queen
    | Bishop
    | Knight
    | Rook
    | Pawn 
    | Undefined

let pieceToString piece = 
    match piece with 
    | King -> "K"
    | Queen -> "Q"
    | Bishop -> "B"
    | Rook -> "R"
    | Knight -> "N"
    | Pawn -> ""
    | Undefined -> ""
    
let promotionToString promotion = 
    match promotion with 
    | Undefined -> ""
    | _ -> sprintf "=%s" (pieceToString promotion)

type Move = 
    { Piece: Pieces ; From: Bitboard; To: Bitboard; Promotion: Pieces}
    override x.ToString() = 
        sprintf "%s%s-%s%s" (pieceToString x.Piece) (sqToString x.From) (sqToString x.To) (promotionToString x.Promotion)
 
let K origin destiny = 
    {Piece=King; From=origin; To=destiny; Promotion=Undefined}

let R origin destiny = 
    {Piece=Rook; From=origin; To=destiny; Promotion=Undefined}

let B origin destiny = 
    {Piece=Bishop; From=origin; To=destiny; Promotion=Undefined}

let Q origin destiny = 
    {Piece=Queen; From=origin; To=destiny; Promotion=Undefined}

let N origin destiny = 
    {Piece=Knight; From=origin; To=destiny; Promotion=Undefined}

let p origin destiny  =
    let promotion = if ((isSet destiny (rank 0)) || (isSet destiny (rank 7))) then Queen else Undefined 
    {Piece=Pawn; From=origin; To=destiny; Promotion=promotion}

let NullMove = {Piece=Undefined; From=A1; To=A1; Promotion=Undefined}


let applyMove move position =
    let inline a move bitboard =
        if (isSet move.From bitboard) then 
            bitboard &&& ~~~(move.From) ||| move.To
        else
            if (isSet move.To bitboard) then bitboard &&& ~~~(move.To) else bitboard

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


let rec private enumerateMoves piece from destinations =
    enumerateSquares destinations 
    |> Seq.map (fun(sq) -> 
                   if piece = Pawn && (rankIndexOfSquare sq) = 7 then
                      {Piece=piece; From=from; To=sq; Promotion=Queen }
                   else
                      {Piece=piece; From=from; To=sq; Promotion=Undefined }
               )

// ------------------------------

let whiteKingMoves position =
    enumerateMoves King position.WhiteKing ((kingAttacks position.WhiteKing) &&& ~~~(blackAttacks position false) &&& ~~~(position.WhitePieces))

let whiteQueenMoves position =
    enumerateSquares position.WhiteQueens
    |> Seq.map (fun(queen) -> enumerateMoves Queen queen ((queensAttacks queen position.WhitePieces position.BlackPieces)  &&& ~~~(position.WhitePieces)))
    |> Seq.concat


let whiteRookMoves position =
    enumerateSquares position.WhiteRooks
    |> Seq.map (fun(rook) -> enumerateMoves Rook rook ((rooksAttacks rook position.WhitePieces position.BlackPieces)  &&& ~~~(position.WhitePieces)))
    |> Seq.concat

let whiteBishopMoves position =
    enumerateSquares position.WhiteBishops
    |> Seq.map (fun(bishop) -> enumerateMoves Bishop bishop ((bishopsAttacks bishop position.WhitePieces position.BlackPieces)  &&& ~~~(position.WhitePieces)))
    |> Seq.concat

let whiteKnightsMoves (position : Position) =
    enumerateSquares position.WhiteKnights
    |> Seq.map (fun(knight) -> enumerateMoves Knight knight ((knightsAttacks knight) &&& ~~~(position.WhitePieces)))
    |> Seq.concat

let private rank1 = rank 1
let whitePawnMoves (position : Position) =
    enumerateSquares position.WhitePawns
    |> Seq.map(fun(pawn) -> 
        let captures = (pawn.chessShift 1 -1 ||| pawn.chessShift 1 1) &&& position.BlackPieces
        
        let advance1 = 
            (pawn.chessShift 1 0) &&& ~~~position.AllPieces 

        let advance2 = 
            match advance1 with 
            | 0UL -> 0UL
            | _   -> (if (isSet pawn rank1) then pawn.chessShift 2 0 else 0UL) &&& ~~~position.AllPieces 
            
        enumerateMoves Pawn pawn (captures ||| advance1 ||| advance2)
       )
    |> Seq.concat

let whiteMoves position : seq<Move> = seq {
    yield! whiteQueenMoves position
    yield! whiteRookMoves position
    yield! whiteBishopMoves position
    yield! whiteKnightsMoves position
    yield! whitePawnMoves position
    yield! whiteKingMoves position
}

// -------------------------------

let blackKingMoves position inCheck=
    enumerateMoves King position.BlackKing ((kingAttacks position.BlackKing) &&& ~~~(whiteAttacks position inCheck) &&& ~~~(position.BlackPieces))

let blackQueenMoves position =
    enumerateSquares position.BlackQueens
    |> Seq.map (fun(queen) -> enumerateMoves Queen queen ((queensAttacks queen position.BlackPieces position.WhitePieces)  &&& ~~~(position.BlackPieces)))
    |> Seq.concat


let blackRookMoves position =
    enumerateSquares position.BlackRooks
    |> Seq.map (fun(rook) -> enumerateMoves Rook rook ((rooksAttacks rook position.BlackPieces position.WhitePieces)  &&& ~~~(position.BlackPieces)))
    |> Seq.concat

let blackBishopMoves position =
    enumerateSquares position.BlackBishops
    |> Seq.map (fun(bishop) -> enumerateMoves Bishop bishop ((bishopsAttacks bishop position.BlackPieces position.WhitePieces)  &&& ~~~(position.BlackPieces)))
    |> Seq.concat

let blackKnightsMoves (position : Position) =
    enumerateSquares position.BlackKnights
    |> Seq.map (fun(knight) -> enumerateMoves Knight knight ((knightsAttacks knight) &&& ~~~(position.BlackPieces)))
    |> Seq.concat

let rank6 = rank 6
let blackPawnMoves (position : Position) =
    enumerateSquares position.BlackPawns 
    |> Seq.map(fun(pawn) -> 
        let captures = (pawn.chessShift -1 -1 ||| pawn.chessShift -1 1) &&& position.WhitePieces
        
        let advance1 = (pawn.chessShift -1 0) &&& ~~~position.AllPieces

        let advance2 = 
            match advance1 with 
            | 0UL -> 0UL
            | _   -> (if (isSet pawn rank6) then pawn.chessShift -2 0 else 0UL) &&& ~~~position.AllPieces 


        enumerateMoves Pawn pawn (captures ||| advance1 ||| advance2)
       )
    |> Seq.concat

let blackMoves position  = seq {
    yield! blackQueenMoves position
    yield! blackRookMoves position
    yield! blackBishopMoves position
    yield! blackKnightsMoves position
    yield! blackPawnMoves position
    yield! blackKingMoves position true
}
// ------------------------------

let moves position  =
    let alternatives = match position.SideToMove with
                        | White -> whiteMoves position
                        | Black -> blackMoves position
    
    alternatives 
    |> Seq.where(fun move ->
                    let np = applyMove move position
                    not (isKingUnderAttack position.SideToMove np)
                ) 

type Future = { M: Move; P: Position}

let futures position =
    let alternatives = match position.SideToMove with
                        | White -> whiteMoves position
                        | Black -> blackMoves position
    
    
    
    alternatives 
    |> Seq.map(fun m -> {M=m; P=applyMove m position})
    |> Seq.where(fun f -> not (isKingUnderAttack position.SideToMove f.P))
   