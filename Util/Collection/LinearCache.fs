module Util.LinearCache

open System
open System.Collections.Generic
open System.Collections.Concurrent


type Item<'v> = {
    mutable accessedat: DateTime
    mutable reloadedat: DateTime
    mutable datao: 'v option }

let adder dt k v = 
    {
        accessedat = dt
        reloadedat = dt
        datao = Some v }

let updater dt v k item = 
    item.accessedat <- dt
    item.datao <- Some v
    item

type LinearCache<'k,'kk,'v> = 
    {
        name: string
        storage: ConcurrentDictionary<'kk,Item<'v>>
        k__kk: 'k -> 'kk
        getCount: ref<int64>
        hitCount: ref<int64>
        reloadCount: ref<int64>
        mutable loadero: ('k -> 'v option) option
        mutable quitingo: ConcurrentDictionary<'kk,Item<'v>> option
        limit: int
        truncate: int }
        
    member this.checkLimit () =
        if this.quitingo.IsSome && this.storage.Count > this.limit then
            let quiting = this.quitingo.Value
            this.storage.ToArray()
            |> Array.sortBy(fun i -> i.Value.accessedat)
            |> Array.truncate (this.storage.Count - this.limit + this.truncate)
            |> Array.iter(fun i ->
                if this.storage.TryRemove i then
                    quiting.AddOrUpdate(i.Key,i.Value,fun _ _ -> i.Value) |> ignore
            )
        
    member this.Item
        with get k =

            let now = DateTime.UtcNow
            System.Threading.Interlocked.Increment this.getCount |> ignore
            
            let key = this.k__kk k
            if this.storage.ContainsKey key then
                System.Threading.Interlocked.Increment this.hitCount |> ignore
                let v = this.storage.[key]
                v.accessedat <- now
                v.datao
            else
                System.Threading.Interlocked.Increment this.reloadCount |> ignore
                match this.loadero with
                | Some loader -> 

                    match loader k with
                    | Some v ->
                        let key = this.k__kk k
                        let i = this.storage.AddOrUpdate(key,adder now k v,updater now v)
                        i.accessedat <- now
                        i.reloadedat <- now
                        this.checkLimit ()
                        Some v

                    | None -> None
                | None -> None

        and set k v = 
            let now = DateTime.UtcNow

            let key = this.k__kk k
            let i = this.storage.AddOrUpdate(key,adder now k v,updater now v )
            i.accessedat <- now
            this.checkLimit ()

let createLinearCacheWithStringKey<'v>
    (name,limit,truncate) 
    loadero = {
        name = name
        storage = new ConcurrentDictionary<string,Item<'v>>(500000,500000)
        k__kk = fun s -> s
        getCount = ref 0L
        hitCount = ref 0L
        reloadCount = ref 0L
        loadero = loadero
        quitingo = None
        limit = limit
        truncate = truncate }

let createLinearCacheHashed<'k,'v>
    (name,limit,truncate) 
    loadero 
    (k__hash:'k -> string) = {
        name = name
        storage = new ConcurrentDictionary<string,Item<'v>>()
        k__kk = k__hash
        getCount = ref 0L
        hitCount = ref 0L
        reloadCount = ref 0L
        loadero = loadero
        quitingo = None
        limit = limit
        truncate = truncate }

