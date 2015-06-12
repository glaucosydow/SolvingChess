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
    member x.hasResponses = x.Ms.Length > 0
    member x.checkMate = x.Ms.Length = 0 && isCheckMate x.P



let rec findMate position (history : array<Move>) = 
    if history.Length < 5 then 

        let continuations = 
            moves position
            |> Seq.map(fun(move) -> {M=move; P=applyMove move position })
            |> Seq.sortBy (fun record -> record.Ms.Length) 
            |> Seq.toArray

        let mate = 
            continuations
            |> Array.takeWhile (fun record -> not record.hasResponses)
            |> Array.tryFind   (fun record -> record.checkMate)

        match mate with
        | Some(record) ->  Some (Array.append history [| record.M |])
        | None ->
            let alternatives = 
                continuations 
                |> Array.skipWhile (fun alternative -> not alternative.hasResponses)

            let future = 
                    alternatives
                    |> Array.map (fun alternative -> findMate alternative.P (Array.append history [| alternative.M |]))

            match position.SideToMove with
            | Black ->
                let isThereEscapes = future |> Array.exists(fun f -> f = None)

                if isThereEscapes then
                    None
                else
                    future |> Array.head
            
            | White -> 
                let mateLine = future |> Array.tryFind(fun f -> f <> None)
                match mateLine with 
                    | None -> None
                    | Some(x) -> x
    
    else
        None
            
