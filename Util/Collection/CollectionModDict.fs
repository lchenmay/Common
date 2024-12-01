module Util.CollectionModDict

open System
open System.Numerics
open System.Collections.Generic
open System.Threading

open Util.Math
open Util.Concurrent

type Stats = {
mutable totalAccess:int64
mutable maxConcurrency:int64 }

let empty__Stats() = {
    totalAccess = 0L
    maxConcurrency = 0L }

let exp2_mods<'k,'v>
    (comparer:IEqualityComparer<'k>)
    exp2 =
    let mutable partition = int32Pow 2 exp2
    [| 0 .. partition - 1 |]
    |> Array.map(fun i -> new Dictionary<'k,'v>(comparer))

type ModDict<'k,'v> = 
    {
        localizer: 'k -> int
        mutable exp2: int
        comparer:IEqualityComparer<'k>
        mutable partitions: Dictionary<'k,'v>[]
        mutable count: int
        stats: Stats
        mutable concurrency_limit:int
        mutable max_concurrency:int
        mutable current_concurrency:int
        mutable timeout:int }

    member this.modulo k =
        Interlocked.Increment(&this.stats.totalAccess) |> ignore
        let p = this.localizer k
        p,this.partitions[p]

    member this.Item
        with get k =
            let p,partition = this.modulo k
            partition[k]
        and set k v =
            let p,partition = this.modulo k
            lock partition (fun _ ->
                if partition.ContainsKey k = false then
                    Interlocked.Increment(&this.count) |> ignore
                partition[k] <- v)

    member this.Keys
        with get() = 
            this.partitions
            |> Array.map(fun m -> lock m (fun _ -> m.Keys |> Seq.toArray))
            |> Array.concat

    member this.Values
        with get() = 
            this.partitions
            |> Array.map(fun m -> lock m (fun _ -> m.Values |> Seq.toArray))
            |> Array.concat

    member this.ToArray() =
        this.partitions
        |> Array.map(fun m -> lock m (fun _ -> m |> Seq.toArray))
        |> Array.concat

    member this.Clear() =
        lock this (fun _ ->
            this.partitions
            |> Array.iter(fun m -> lock m (fun () -> m.Clear()))
            this.count <- 0)

    member this.ContainsKey k =
        let p,partition = this.modulo k
        lock partition (fun _ -> partition.ContainsKey k)

    member this.TryGet key =
        let p,partition = this.modulo key
        lock partition (fun ()->
            if partition.ContainsKey key then
                Some partition[key]
            else
                None)

    member this.GetOrAdd key key__o = 
        let p,partition = this.modulo key
        lock partition (fun ()->
            if partition.ContainsKey key then
                Some partition[key]
            else
                let o = key__o key
                match o with
                | Some v -> 
                    partition[key] <- v
                    Interlocked.Increment(&this.count) |> ignore
                | None -> ()
                o)

    member this.Remove key =
        let p,partition = this.modulo key
        lock partition (fun ()->
            if partition.ContainsKey key then
                partition.Remove key |> ignore
                Interlocked.Decrement(&this.count) |> ignore)

    member this.Reset exp2 = 
        lock this (fun _ -> 

            let ary = this.ToArray()

            this.exp2 <- exp2
            this.partitions <- exp2_mods<'k,'v> this.comparer this.exp2
            this.Clear()

            ary
            |> Array.iter(fun kvp -> this[kvp.Key] <- kvp.Value))

let ModDict_empty() = 
    let md = {
        localizer = (fun id -> 0)
        exp2 = 0
        partitions = [| |]
        comparer = null
        count = 0 
        stats = empty__Stats() 
        concurrency_limit = 0
        max_concurrency = 0
        current_concurrency = 0
        timeout = 1000 }
    md.Reset 1
    md

type ModDictInt64<'v> = ModDict<int64,'v>
type ModDictStr<'v> = ModDict<string,'v>

type private ComparerInt64() =
    interface IEqualityComparer<int64> with
        member this.Equals(a:int64,b:int64):bool = (a=b)
        member this.GetHashCode(x):int = 0
let private comparerInt64 = new ComparerInt64()

let createModDictInt64<'v> exp2: ModDictInt64<'v> = 
    let mods = exp2_mods<int64,'v> comparerInt64 exp2
    {
        localizer = (fun id -> moduloInt64 id mods.LongLength |> int)
        exp2 = exp2
        comparer = comparerInt64
        partitions = mods
        count = 0 
        stats = empty__Stats() 
        concurrency_limit = 0
        max_concurrency = 0
        current_concurrency = 0
        timeout = 1000 }

type ComparerString() =
    interface IEqualityComparer<String> with
        member this.Equals(a:string,b:string):bool = (a = b)
        member this.GetHashCode x:int = 0

let comparerString = new ComparerString()
//let private md5 = new System.Security.Cryptography.MD5CryptoServiceProvider()
//let private md5 = System.Security.Cryptography.MD5.Create()

let createModDictStr<'v> exp2: ModDictStr<'v> =
    let mods = exp2_mods<string,'v> comparerString exp2
    let m = BigInteger mods.Length
    
    {
        localizer = (fun (k:string) ->
            let bin = k |> System.Text.Encoding.UTF8.GetBytes
            let i = BigInteger bin
            moduloBigNumber (BigInteger bin) m |> int)
        exp2 = exp2
        comparer = comparerString
        partitions = mods 
        count = 0 
        stats = empty__Stats() 
        concurrency_limit = 0
        max_concurrency = 0
        current_concurrency = 0
        timeout = 1000 }

let ModDictInt64__dict (md:ModDictInt64<'v>) = 
    new Dictionary<int64,'v>(md.ToArray(),comparerInt64)

let dict__ModDictInt64 (dict:Dictionary<int64,'v>) = 
    let md = createModDictInt64 4
    dict
    |> Seq.iter(fun kvp -> md[kvp.Key] <- kvp.Value)
    md

let ModDictStr__dict (md:ModDictStr<'v>) = 
    new Dictionary<string,'v>(md.ToArray(),comparerString)

let dict__ModDictStr (dict:Dictionary<string,'v>) = 
    let md = createModDictStr 4
    dict
    |> Seq.iter(fun kvp -> md[kvp.Key] <- kvp.Value)
    md

let array__ModDictInt64 exp v__k items = 
    let md = createModDictInt64 exp
    items
    |> Array.iter(fun v -> 
        let k = v__k v
        md[k] <- v)
    md

let array__ModDictStr exp v__k items = 
    let md = createModDictStr exp
    items
    |> Array.iter(fun v -> 
        let k = v__k v
        md[k] <- v)
    md

