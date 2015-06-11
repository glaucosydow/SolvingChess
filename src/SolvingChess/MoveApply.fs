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

let private applyWhiteKnightMove move position =
    if (isSet move.From position.WhiteKnights) then 
        {position with WhiteKnights = (applyMoveOnBitmap move position.WhiteKnights)}
    else
        if (isSet move.To position.WhiteKnights) then 
            {position with WhiteKnights = position.WhiteKnights &&& ~~~(move.To)}
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

let private applyBlackKingMove move position =
    if (isSet move.From position.BlackKing) then 
        {position with BlackKing = (applyMoveOnBitmap move position.BlackKing)}
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
                        |> applyWhiteKnightMove move
                        |> applyWhitePawnMove move
                        |> applyBlackKingMove move
                        |> applyBlackPawnMove move
                        

    { partialResult with SideToMove = (oppositeSide position.SideToMove) }

