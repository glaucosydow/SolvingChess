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

let mutable numberOfCalls = 0

open System.Collections.Generic

let rec findMate position depth maxdepth = 
    numberOfCalls <- if depth = 0 then 0 else numberOfCalls + 1
       
    if depth < maxdepth then

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
        | Some(record) ->  
            Some ([| record.M |])
        | None ->
            let alternatives = 
                continuations 
                |> Array.skipWhile (fun alternative -> not alternative.hasResponses)


            match position.SideToMove with
            | Black ->
                
                
                let rec explore (enumerator:IEnumerator<Move[] option>): Move[] option =
                    if enumerator.MoveNext() 
                    then
                        let c1 = enumerator.Current
                        if c1 = None 
                        then None
                        else
                            let c2 = explore enumerator
                            match c1, c2 with
                            | None, _ -> None
                            | _, None -> None
                            | Some(a), Some(b) -> Some(if a.Length > b.Length then a else b)
                    else
                        Some(Array.empty<Move>)
                   
                let future = 
                    alternatives
                    |> Seq.map (fun alternative -> 
                                    let line = findMate alternative.P (depth + 1) maxdepth
                                    match line with 
                                    | Some(x) -> Some(Array.append [| alternative.M |] x)
                                    | None -> None
                                  )

                explore (future.GetEnumerator())

            | White -> 
                let future = 
                    alternatives
                    |> Array.where(fun alternative -> alternative.Ms.Length <= 3)
                    |> Array.map (fun alternative -> 
                                    //if (depth <= 6) then printfn "%s%s" (String.replicate (depth + 1) " ") (alternative.ToString())                                    
                                    let line = findMate alternative.P (depth + 1) maxdepth
                                    match line with 
                                    | Some(x) -> Some(Array.append [| alternative.M |] x)
                                    | None -> None
                                  )

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
            
