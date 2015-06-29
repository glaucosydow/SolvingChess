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

let whiteQueenMoves (position: Position) =
    let allpieces = position.BlackPieces ||| position.WhitePieces 
    enumerateSquares position.WhiteQueens
    |> Seq.map (fun(queen) -> enumerateMoves Queen queen ((queenAttacks queen allpieces)  &&& ~~~(position.WhitePieces)))
    |> Seq.concat

let whiteRookMoves (position: Position) =
    let allpieces = position.BlackPieces ||| position.WhitePieces 
    enumerateSquares position.WhiteRooks
    |> Seq.map (fun(rook) -> enumerateMoves Rook rook ((rookAttacks rook allpieces)  &&& ~~~(position.WhitePieces)))
    |> Seq.concat

let whiteBishopMoves (position: Position) =
    let allpieces = position.BlackPieces ||| position.WhitePieces 
    enumerateSquares position.WhiteBishops
    |> Seq.map (fun(bishop) -> enumerateMoves Bishop bishop ((bishopAttacks bishop allpieces)  &&& ~~~(position.WhitePieces)))
    |> Seq.concat

let whiteKnightsMoves (position : Position) =
    enumerateSquares position.WhiteKnights
    |> Seq.map (fun(knight) -> enumerateMoves Knight knight ((knightAttacks knight) &&& ~~~(position.WhitePieces)))
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

let private fullBoard = rank 0 ||| rank 1 ||| rank 2 ||| rank 3 ||| rank 4 ||| rank 5 ||| rank 6 ||| rank 7

let blackQueenFilteredMoves (position: Position) hot =
    let allpieces = position.BlackPieces ||| position.WhitePieces 
    enumerateSquares position.BlackQueens
    |> Seq.map (fun(queen) -> enumerateMoves Queen queen ((queenAttacks queen allpieces) &&& hot &&& ~~~(position.BlackPieces)))
    |> Seq.concat

let blackQueenMoves (position: Position) =
    blackQueenFilteredMoves position fullBoard

let blackRookFilteredMoves (position: Position) hot =
    let allpieces = position.BlackPieces ||| position.WhitePieces     
    enumerateSquares position.BlackRooks
    |> Seq.map (fun(rook) -> enumerateMoves Rook rook ((rookAttacks rook allpieces) &&& hot &&& ~~~(position.BlackPieces)))
    |> Seq.concat

let blackRookMoves (position: Position) =
    blackRookFilteredMoves position fullBoard

let blackBishopFilteredMoves (position: Position) hot =
    let allpieces = position.BlackPieces ||| position.WhitePieces 
    enumerateSquares position.BlackBishops
    |> Seq.map (fun(bishop) -> enumerateMoves Bishop bishop ((bishopAttacks bishop allpieces) &&& hot &&& ~~~(position.BlackPieces)))
    |> Seq.concat

let blackBishopMoves (position: Position) =
    blackBishopFilteredMoves position fullBoard

let blackKnightsFilteredMoves (position : Position) hot =
    enumerateSquares position.BlackKnights
    |> Seq.map (fun(knight) -> enumerateMoves Knight knight ((knightAttacks knight) &&& hot &&& ~~~(position.BlackPieces)))
    |> Seq.concat

let blackKnightsMoves (position : Position) =
    blackKnightsFilteredMoves position fullBoard

let rank6 = rank 6
let blackPawnFilteredMoves (position : Position) hot =
    enumerateSquares position.BlackPawns 
    |> Seq.map(fun(pawn) -> 
        let captures = (pawn.chessShift -1 -1 ||| pawn.chessShift -1 1) &&& position.WhitePieces
        
        let advance1 = (pawn.chessShift -1 0) &&& ~~~position.AllPieces

        let advance2 = 
            match advance1 with 
            | 0UL -> 0UL
            | _   -> (if (isSet pawn rank6) then pawn.chessShift -2 0 else 0UL) &&& ~~~position.AllPieces 


        enumerateMoves Pawn pawn ((captures ||| advance1 ||| advance2) &&& hot)
       )
    |> Seq.concat

let blackPawnMoves (position : Position) =
    blackPawnFilteredMoves position fullBoard

let blackEscapes (position: Position) = seq {
    yield! blackKingMoves position true

    let allpieces = position.AllPieces

    let kn = (knightAttacks position.BlackKing) &&& position.WhiteKnights
    let p = (whitePawnAttackers position.BlackKing) &&& position.WhitePawns
    let bqP = (bishopAttacks position.BlackKing allpieces)
    let bq = if (bqP &&& position.WhiteBishops) <> 0UL || (bqP &&& position.WhiteQueens) <> 0UL then bqP else 0UL
    let rqP = (rookAttacks position.BlackKing allpieces)
    let rq = if (rqP &&& position.WhiteRooks) <> 0UL || (rqP &&& position.WhiteQueens) <> 0UL then rqP else 0UL

    let hot = kn ||| p ||| bq ||| rq
    yield! blackPawnFilteredMoves position hot
    yield! blackQueenFilteredMoves position hot
    yield! blackBishopFilteredMoves position hot
    yield! blackKnightsFilteredMoves position hot
    yield! blackRookFilteredMoves position hot
}

let blackMoves position  = seq {
    if isCheck position 
    then 
        yield! blackEscapes position
    else
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

open KingSafety
type Future(m, p) = 
    member x.M = m
    member x.P = p
    

let futures position =
    let alternatives = match position.SideToMove with
                        | White -> whiteMoves position
                        | Black -> blackMoves position
    
    alternatives 
    |> Seq.map(fun m -> Future(m, applyMove m position))
    |> Seq.where(fun f -> not (isKingUnderAttack position.SideToMove f.P))

    