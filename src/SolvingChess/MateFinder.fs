module MateFinder

open Position
open Attacks
open Moves
open Check

type Record(m,p) =  
    let ms = (futures p) |> Seq.toArray
    let chk = (isCheck p)

    member x.M = m
    member x.P = p
    member x.Ms = ms
    
    member x.hasResponses = x.Ms.Length > 0
    member x.check = chk
    member x.withNoKingMoves = not (Array.exists (fun future -> future.M.Piece = King) x.Ms )
    member x.checkMate = x.Ms.Length = 0 && x.check
    override x.ToString() = x.M.ToString()


open System.Collections.Generic
open BitOperations

let mutable numberOfCalls = 0

let rec private internalFindMate (seed: Record) depth maxdepth = 
    numberOfCalls <- if depth = 0 then 0 else numberOfCalls + 1
       
    if depth < maxdepth then

        let continuations = 
            seed.Ms
            |> Seq.map(fun(f) -> Record(f.M, f.P))
            |> Seq.sortBy (fun record -> record.Ms.Length) 
            |> Seq.toArray

        let mate = 
            continuations
            |> Array.takeWhile (fun record -> not record.hasResponses)
            |> Array.tryFind   (fun record -> record.checkMate)

        match mate with
        | Some(record) ->  
            match seed.P.SideToMove with
            | Black -> None
            | White ->
                 //printfn "%s%s" (String.replicate (depth + 1) " ") (record.ToString())
                 Some ([| record |])
        | None ->
            let alternatives = 
                continuations 
                |> Array.skipWhile (fun alternative -> not alternative.hasResponses)

            match seed.P.SideToMove with
            | Black ->
                let rec explore (enumerator:IEnumerator<Record[] option>): Record[] option =
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
                            | Some(a), Some(b) -> 
                                Some(
                                    if a.Length > b.Length 
                                    then a
                                    elif a.Length = b.Length then 
                                        (
                                            if ((Array.last a).P.score) > ((Array.last b).P.score) 
                                            then b 
                                            else a
                                        )
                                    else b
                                )
                    else
                        Some(Array.empty<Record>)
                   
                let future = 
                    alternatives
                    |> Seq.map (fun alternative -> 
                                    //printfn "%s%s" (String.replicate (depth + 1) " ") (alternative.ToString())
                                    let line = internalFindMate alternative (depth + 1) maxdepth
                                    match line with 
                                    | Some(x) -> Some(Array.append [| alternative |] x)
                                    | None -> None
                                  )

                explore (future.GetEnumerator())

            | White -> 
                let rec explore (enumerator:IEnumerator<Record>) (maxdepth) : Record[] option =
                    if enumerator.MoveNext() 
                    then
                        let move = enumerator.Current
                        //printfn "%s%s" (String.replicate (depth + 1) " ") (move.ToString())
                        let line1 = internalFindMate enumerator.Current (depth + 1) maxdepth
                        let line2 = explore enumerator (if line1 = None then maxdepth else depth + line1.Value.Length)
                        match line1, line2 with 
                        | Some(x), None -> Some(Array.append [| move |] x)
                        | None, Some(x) -> Some(x)
                        | Some(x), Some(y) -> 
                            Some (
                                let rx = (Array.append [| move |] x)
                                if rx.Length < y.Length 
                                then rx 
                                elif rx.Length = y.Length then 
                                        (
                                            if ((Array.last rx).P.score) > ((Array.last y).P.score) 
                                            then rx 
                                            else y
                                        )
                                else y
                            )
                        | None, None -> None
                    else
                        None


                let e (a: IEnumerable<Record>) pre =
                    if pre = None
                    then
                        explore (a.GetEnumerator()) maxdepth
                    else
                        pre

                let blackKingArea = kingArea seed.P.BlackKing

                let kaAlternatives = alternatives |> Array.where(fun a -> (a.M.To &&& blackKingArea) = a.M.To )
                let nkaAlternatives = alternatives |> Array.where(fun a -> (a.M.To &&& blackKingArea) <> a.M.To )
                
                e (kaAlternatives |> Seq.where    (fun a -> a.Ms.Length = 1))  None
                |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length = 1)) 
                |> e (kaAlternatives |> Seq.where (fun a -> a.Ms.Length = 2)) 
                |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length = 2)) 

                |> e (kaAlternatives |> Seq.where (fun a -> a.Ms.Length > 2 && a.check))
                |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length > 2 && a.check))
                |> e (kaAlternatives |> Seq.where (fun a -> a.Ms.Length > 2 && (not a.check) && a.withNoKingMoves))
                |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length > 2 && (not a.check) && a.withNoKingMoves))
                
    else
        None
            
let rec findMate position depth maxdepth = 
    match internalFindMate (Record (NullMove, position)) depth maxdepth with
    | None -> None
    | Some(line) -> Some (line |> Array.map(fun record -> record.M) )
    

