module MoveApply

open BitOperations
open Position
open MoveGen

let private applyMoveOnBitmap move bitmap =
    bitmap &&& ~~~(move.From) ||| move.To

let private applyWhiteKingMove move position =
    if (isSet move.From position.WhiteKing) then 
        {position with WhiteKing = (applyMoveOnBitmap move position.WhiteKing)}
    else
        position

let private applyWhiteQueenMove move position =
    if (isSet move.From position.WhiteQueens) then 
        {position with WhiteQueens = (applyMoveOnBitmap move position.WhiteQueens)}
    else
        if (isSet move.To position.WhiteQueens) then 
            {position with WhiteQueens = position.WhiteQueens &&& ~~~(move.To)}
        else
            position

let private applyWhiteBishopMove move position =
    if (isSet move.From position.WhiteBishops) then 
        {position with WhiteBishops = (applyMoveOnBitmap move position.WhiteBishops)}
    else
        if (isSet move.To position.WhiteBishops) then 
            {position with WhiteBishops = position.WhiteBishops &&& ~~~(move.To)}
        else
            position


let private applyWhiteKnightMove move position =
    if (isSet move.From position.WhiteKnights) then 
        {position with WhiteKnights = (applyMoveOnBitmap move position.WhiteKnights)}
    else
        if (isSet move.To position.WhiteKnights) then 
            {position with WhiteKnights = position.WhiteKnights &&& ~~~(move.To)}
        else
            position


let private applyWhiteRookMove move position =
    if (isSet move.From position.WhiteRooks) then 
        {position with WhiteRooks = (applyMoveOnBitmap move position.WhiteRooks)}
    else
        if (isSet move.To position.WhiteRooks) then 
            {position with WhiteRooks = position.WhiteRooks &&& ~~~(move.To)}
        else
            position

let private applyWhitePawnMove move position =
    if (isSet move.From position.WhitePawns) then 
        match move.Promotion with 
            | Undefined -> {position with WhitePawns = (applyMoveOnBitmap move position.WhitePawns)}
            | Queen -> {position with WhitePawns = position.WhitePawns &&& ~~~(move.From); WhiteQueens = position.WhiteQueens ||| move.To}
    else
        if (isSet move.To position.WhitePawns) then 
            {position with WhitePawns = position.WhitePawns &&& ~~~(move.To)}
        else
            position
// ------



let private applyBlackKingMove move position =
    if (isSet move.From position.BlackKing) then 
        {position with BlackKing = (applyMoveOnBitmap move position.BlackKing)}
    else
        position

let private applyBlackQueenMove move position =
    if (isSet move.From position.BlackQueens) then 
        {position with BlackQueens = (applyMoveOnBitmap move position.BlackQueens)}
    else
        if (isSet move.To position.BlackQueens) then 
            {position with BlackQueens = position.BlackQueens &&& ~~~(move.To)}
        else
            position

let private applyBlackBishopMove move position =
    if (isSet move.From position.BlackBishops) then 
        {position with BlackBishops = (applyMoveOnBitmap move position.BlackBishops)}
    else
        if (isSet move.To position.BlackBishops) then 
            {position with BlackBishops = position.BlackBishops &&& ~~~(move.To)}
        else
            position


let private applyBlackKnightMove move position =
    if (isSet move.From position.BlackKnights) then 
        {position with BlackKnights = (applyMoveOnBitmap move position.BlackKnights)}
    else
        if (isSet move.To position.BlackKnights) then 
            {position with BlackKnights = position.BlackKnights &&& ~~~(move.To)}
        else
            position

let private applyBlackRookMove move position =
    if (isSet move.From position.BlackRooks) then 
        {position with BlackRooks = (applyMoveOnBitmap move position.BlackRooks)}
    else
        if (isSet move.To position.BlackRooks) then 
            {position with BlackRooks = position.BlackRooks &&& ~~~(move.To)}
        else
            position


let private applyBlackPawnMove move position =
    if (isSet move.From position.BlackPawns) then 
        {position with BlackPawns = (applyMoveOnBitmap move position.BlackPawns)}
    else
        if (isSet move.To position.BlackPawns) then 
            {position with BlackPawns = position.BlackPawns &&& ~~~(move.To)}
        else
            position



let oppositeSide side =
    match side with 
    | White -> Black
    | Black -> White

let applyMove move position =
    let partialResult = position 
                        |> applyWhiteKingMove move
                        |> applyWhiteQueenMove move
                        |> applyWhiteBishopMove move
                        |> applyWhiteKnightMove move
                        |> applyWhiteRookMove move
                        |> applyWhitePawnMove move

                        |> applyBlackKingMove move
                        |> applyBlackQueenMove move
                        |> applyBlackBishopMove move
                        |> applyBlackKnightMove move
                        |> applyBlackRookMove move
                        |> applyBlackPawnMove move
                        

    { partialResult with SideToMove = (oppositeSide position.SideToMove) }

