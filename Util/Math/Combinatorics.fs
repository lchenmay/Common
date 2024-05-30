module Util.Combinatorics

open System.Collections.Generic



let permutation(bag:'T[], n:int) =

    let res = new ResizeArray<'T[]>()

    if(bag.Length >= n) then

        let array = new ResizeArray<int>()
        let rec recurser() =
            if(array.Count = n) then
                array.ToArray() |> Seq.map(fun i -> bag.[i]) |> Seq.toArray |> res.Add
            else
                [0..bag.Length - 1] |> Seq.iter(fun i ->
                    if(array.Contains(i) = false) then
                        array.Add(i)
                        recurser()
                        array.RemoveAt(array.Count - 1) |> ignore)

        recurser()

    res.ToArray()

let combination(bag:'T[], n:int) =

    let ls = new List<'T[]>()

    let equals(a:'T[], b:'T[]) =
        let mutable e = true
        [0..a.Length - 1] |> Seq.iter(fun i ->
            if(a.[i] <> b.[i]) then
                e <- false)
        e

    let contains(a) =
        let mutable c = false
        ls |> Seq.iter(fun b ->
            if(equals(a, b) = true) then
                c <- true)
        c

    let ps = permutation(bag, n)
    ps |> Seq.iter(fun p ->
        let sorted = Util.Collection.sortarray p
        if(contains(sorted) = false) then
            ls.Add(sorted))

    ls.ToArray()