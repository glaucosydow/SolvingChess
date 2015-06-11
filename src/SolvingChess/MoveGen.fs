﻿module MoveGen

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

let N origin destiny = 
    {Piece=Knight; From=origin; To=destiny; Promotion=Undefined}

let p origin destiny  =
    let promotion = if ((isSet destiny (rank 0)) || (isSet destiny (rank 7))) then Queen else Undefined 
    {Piece=Pawn; From=origin; To=destiny; Promotion=promotion}

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
    enumerateMoves King position.WhiteKing ((kingAttacks position.WhiteKing) &&& ~~~(blackAttacks position) &&& ~~~(position.WhitePieces))

let whiteQueenMoves position =
    enumerateSquares position.WhiteQueens
    |> Seq.map (fun(queen) -> enumerateMoves Queen queen ((queensAttacks queen position.WhitePieces position.BlackPieces)))
    |> Seq.concat


let whiteRookMoves position =
    enumerateSquares position.WhiteRooks
    |> Seq.map (fun(rook) -> enumerateMoves Rook rook ((rooksAttacks rook position.WhitePieces position.BlackPieces)))
    |> Seq.concat

let whiteBishopMoves position =
    enumerateSquares position.WhiteBishops
    |> Seq.map (fun(bishop) -> enumerateMoves Bishop bishop ((bishopsAttacks bishop position.WhitePieces position.BlackPieces)))
    |> Seq.concat

let whiteKnightsMoves (position : Position) =
    enumerateSquares position.WhiteKnights
    |> Seq.map (fun(knight) -> enumerateMoves Knight knight ((knightsAttacks knight) &&& ~~~(position.WhitePieces)))
    |> Seq.concat

let whitePawnMoves (position : Position) =
    enumerateSquares position.WhitePawns
    |> Seq.map(fun(pawn) -> 
        let captures = (pawn.chessShift 1 -1 ||| pawn.chessShift 1 1) &&& position.BlackPieces
        let advance = (pawn.chessShift 1 0) &&& ~~~position.AllPieces
        enumerateMoves Pawn pawn (captures ||| advance)
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

let blackKingMoves position =
    enumerateMoves King position.BlackKing ((kingAttacks position.BlackKing) &&& ~~~(whiteAttacks position) &&& ~~~(position.BlackPieces))

let blackQueenMoves position =
    enumerateSquares position.BlackQueens
    |> Seq.map (fun(queen) -> enumerateMoves Queen queen ((queensAttacks queen position.BlackPieces position.WhitePieces)))
    |> Seq.concat


let blackRookMoves position =
    enumerateSquares position.BlackRooks
    |> Seq.map (fun(rook) -> enumerateMoves Rook rook ((rooksAttacks rook position.BlackPieces position.WhitePieces)))
    |> Seq.concat

let blackBishopMoves position =
    enumerateSquares position.BlackBishops
    |> Seq.map (fun(bishop) -> enumerateMoves Bishop bishop ((bishopsAttacks bishop position.BlackPieces position.WhitePieces)))
    |> Seq.concat

let blackKnightsMoves (position : Position) =
    enumerateSquares position.BlackKnights
    |> Seq.map (fun(knight) -> enumerateMoves Knight knight ((knightsAttacks knight) &&& ~~~(position.BlackPieces)))
    |> Seq.concat

let blackPawnMoves (position : Position) =
    enumerateSquares position.BlackPawns 
    |> Seq.map(fun(pawn) -> 
        let captures = (pawn.chessShift -1 -1 ||| pawn.chessShift -1 1) &&& position.WhitePieces
        let advance = (pawn.chessShift -1 0) &&& ~~~position.AllPieces
        enumerateMoves Pawn pawn (captures ||| advance)
       )
    |> Seq.concat

let blackMoves position  =
    match (isCheck position) with
    | true -> blackKingMoves position
    | _ -> seq {
                    yield! blackQueenMoves position
                    yield! blackRookMoves position
                    yield! blackBishopMoves position
                    yield! blackKnightsMoves position
                    yield! blackPawnMoves position
                    yield! blackKingMoves position
               }
// ------------------------------

let moves position  =
    match position.SideToMove with
    | White -> whiteMoves position
    | Black -> blackMoves position
 

    