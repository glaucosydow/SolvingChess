module MateFinder

open Position
open MoveGen
open MoveApply
open Check

type Record = { 
    M: Move; 
    P: Position; 
}
with 
    member x.Ms = (moves x.P) |> Seq.toArray

let printmoves history =
    printfn "-----------------"
    history 
    |> Array.iter (fun move -> printfn "%s" (move.ToString()))

let rec findMate position (history : array<Move>) =
    if history.Length < 5 then 
        moves position
        |> Seq.map(fun(move) -> {M=move; P=applyMove move position })
        |> Seq.sortBy (fun record -> record.Ms.Length) 
        |> Seq.iter(fun record -> 
                        let newhistory = Array.append history [| record.M |]
                        if record.Ms.Length = 0 && isCheckMate record.P then
                            printmoves newhistory
                        else
                            findMate record.P newhistory
                    )

    




