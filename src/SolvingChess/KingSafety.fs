module KingSafety

open Position
open Attacks
open BitOperations
open BoardUnits

let blackKingAttacksScorePerSquare kingSq (position: Position) =

    let allpieces = position.AllPieces &&& (~~~position.BlackKing)

    let an = ((knightAttacks kingSq) &&& position.WhiteKnights)
    let nn = popcount an

    let ap = ((whitePawnAttackers kingSq) &&& position.WhitePawns)
    let np = popcount ap
    
    let ak = ((kingAttacks kingSq) &&& position.WhiteKing)
    let nk = popcount ak
    
    let ra = rookAttacks kingSq allpieces
    
    let ar = (ra &&& position.WhiteRooks) 
    let nr = popcount ar

    let ba = bishopAttacks kingSq allpieces
    let ab = (ba &&& position.WhiteBishops)
    let nb = popcount ab

    let aq = ((ba &&& position.WhiteQueens) ||| (ra &&& position.WhiteQueens))
    let nq = popcount aq

    (
        nn * 20 + np * 10 + nk * 5 + nr * 40 + nb * 20 + nq * 80,  // attack strength
        an ||| ap ||| ak ||| ar ||| ab ||| aq                      // attackers
    )


let blackKingAreaAttacksScore (position: Position) =
    let destinations = (kingAttacks position.BlackKing) &&& ~~~(position.BlackPieces)
    let computation = enumerateSquares destinations
                      |> Seq.map(fun sq -> blackKingAttacksScorePerSquare sq position)
                      |> Seq.fold(fun (a,b) (c,d) -> (a+c, b ||| d)) (0, 0UL)

    let (valueOfAttacks, attackers) = computation
    let attackersCount = popcount attackers
    let attackWeight = match attackersCount with
                        | 0 -> 0
                        | 1 -> 0
                        | 2 -> 50
                        | 3 -> 75
                        | 4 -> 88
                        | 5 -> 94
                        | 6 -> 97
                        | _ -> 99
    
    valueOfAttacks * attackWeight

    
