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
    member x.withNoKingMoves = not (Array.exists (fun (future: Future) -> future.M.Piece = King) x.Ms )
    member x.checkMate = x.Ms.Length = 0 && chk
    override x.ToString() = x.M.ToString()


open System.Collections.Generic
open BitOperations

let mutable numberOfCalls = 0

let rec private internalFindMate (seed: Record) alpha beta depth maxdepth = 
    numberOfCalls <- if depth = 0 then 0 else numberOfCalls + 1
    if (numberOfCalls % 100) = 0 then printfn "%d plies" numberOfCalls
      
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
                 //printfn "%s%s++ (ply %d)" (String.replicate (depth + 1) " ") (record.ToString()) (numberOfCalls)
                 Some ([| record |])
        | None ->
            let alternatives = 
                continuations 
                |> Array.skipWhile (fun alternative -> not alternative.hasResponses)

            match seed.P.SideToMove with
            | Black ->
                let rec explore accum index (alternatives: Record[]) alpha beta =
                    if (index = alternatives.Length) then accum
                    else
                        let alternative = alternatives.[index]
                        //printfn "%s%s" (String.replicate (depth + 1) " ") (alternative.ToString())
                        let line = internalFindMate alternative alpha beta (depth + 1) maxdepth
                        let local = match line with 
                                    | Some(x) -> Some(Array.append [| alternative |] x)
                                    | None -> None
                    
                        if local=None then
                            None
                        elif index = 0 then 
                            explore local (index + 1) alternatives alpha beta
                        else
                            let a = accum.Value;
                            let b = local.Value;
                            let newaccum = 
                                                Some(
                                                    if   a.Length > b.Length then a
                                                    elif a.Length < b.Length then b
                                                    else if ((Array.last a).P.score) > ((Array.last b).P.score) then b else a
                                                )
                            
                            let v = depth + newaccum.Value.Length
                            let newalpha = max alpha v
                            if (beta < newalpha)
                            then newaccum
                            else explore newaccum (index + 1) alternatives newalpha beta

                explore None 0 alternatives alpha beta

            | White -> 
                let rec explore accum (enumerator:IEnumerator<Record>) (maxdepth) alpha beta : Record[] option =
                    if enumerator.MoveNext() then
                        let move = enumerator.Current
                        //printfn "%s%s" (String.replicate (depth + 1) " ") (move.ToString())
                        let line = internalFindMate move alpha beta (depth + 1) maxdepth
                        let newaccum = 
                            match line, accum with 
                            | None, Some(y) -> Some(y)
                            | Some(x), None -> Some(Array.append [| move |] x)
                            | Some(x), Some(y) -> 
                                let rx = (Array.append [| move |] x)
                                Some (
                                    if   rx.Length < y.Length then rx 
                                    elif rx.Length > y.Length then y
                                    else if ((Array.last rx).P.score) > ((Array.last y).P.score) then rx else y
                                )
                            | None, None -> None
                        
                        let v = if newaccum = None then beta else (depth + newaccum.Value.Length)
                        let newbeta = min beta v
                        if (newbeta < alpha)
                        then newaccum
                        else explore newaccum enumerator (if newaccum = None then maxdepth else depth + newaccum.Value.Length) alpha newbeta
                    else
                        accum


                let e (a: IEnumerable<Record>) pre =
                    match pre with
                    | None -> explore None (a.GetEnumerator()) maxdepth alpha beta
                    | Some(x) -> Some(x)

                let blackKingArea = kingArea seed.P.BlackKing


                let kaAlternatives = alternatives |> Array.where(fun a -> (a.M.To &&& blackKingArea) = a.M.To )
                let nkaAlternatives = alternatives |> Array.where(fun a -> (a.M.To &&& blackKingArea) <> a.M.To )
                
                if (depth > maxdepth - 3) 
                then 
                    e    (kaAlternatives  |> Seq.where(fun a -> a.Ms.Length = 1 && a.check))  None
                    |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length = 1 && a.check)) 
                    |> e (kaAlternatives  |> Seq.where(fun a -> a.Ms.Length = 2 && a.check)) 
                    |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length = 2 && a.check)) 
                    |> e (kaAlternatives  |> Seq.where(fun a -> a.Ms.Length > 2 && a.check))
                    |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length > 2 && a.check))
                else
                    e    (kaAlternatives  |> Seq.where(fun a -> a.Ms.Length = 1))  None
                    |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length = 1)) 
                    |> e (kaAlternatives  |> Seq.where(fun a -> a.Ms.Length = 2)) 
                    |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length = 2)) 
                    |> e (kaAlternatives  |> Seq.where(fun a -> a.Ms.Length > 2 && a.check))
                    |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length > 2 && a.check))
                    |> e (kaAlternatives  |> Seq.where(fun a -> a.Ms.Length > 2 && (not a.check) && a.withNoKingMoves))
                    |> e (nkaAlternatives |> Seq.where(fun a -> a.Ms.Length > 2 && (not a.check) && a.withNoKingMoves))
                
    else
        None
            
let rec findMate position depth maxdepth = 
    match internalFindMate (Record (NullMove, position)) 0 (maxdepth + 1)  depth maxdepth with
    | None -> None
    | Some(line) -> Some (line |> Array.map(fun record -> record.M) )
    

