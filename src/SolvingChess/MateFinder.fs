module MateFinder

open Position
open Moves
open Check

type Record = { 
    M: Move; 
    P: Position; 
}
with 
    member x.Ms = (moves x.P) |> Seq.toArray
    member x.hasResponses = x.Ms.Length > 0
    member x.checkMate = x.Ms.Length = 0 && isCheckMate x.P
    override x.ToString() = x.M.ToString()

let rec findMate position depth maxdepth = 
    
    if depth < maxdepth then

        let c1 = 
            moves position
            |> Seq.map(fun(move) -> {M=move; P=applyMove move position })
            |> Seq.toArray

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
        | Some(record) ->  Some ([| record.M |])
        | None ->
            let alternatives = 
                continuations 
                |> Array.skipWhile (fun alternative -> not alternative.hasResponses)

            let future = 
                    alternatives
                    |> Array.map (fun alternative -> 
                                    let line = findMate alternative.P (depth + 1) maxdepth
                                    match line with 
                                    | Some(x) -> Some(Array.append [| alternative.M |] x)
                                    | None -> None
                                  )

            match position.SideToMove with
            | Black ->
                let isThereEscapes = future |> Array.exists(fun f -> f = None)

                if isThereEscapes then
                    None
                else
                    future 
                    |> Array.sortByDescending(fun f -> f.Value.Length)
                    |> Array.head
            
            | White -> 
                let mateLine = 
                    future 
                    |> Array.where(fun f -> f <> None)
                    |> Array.sortBy(fun f -> f.Value.Length)
                    |> Array.tryHead

                match mateLine with 
                    | None -> None
                    | Some(x) -> x
    
    else
        None
            
