module Util.CollectionModDict

open System
open System.Collections.Generic
open System.Threading

open Util.Concurrent


type ModDictItem<'k,'v> = 
    {
        key:'k;
        data:'v;
        lockee:Object; 
        mutable lock_since:DateTime;
        mutable locked:bool }

let private data__ModDictItem(key,data) = 
    {
        key = key;
        data = data;
        lockee = new Object();
        lock_since = DateTime.UtcNow;
        locked = false }

type ModDictTaskMode = 
| Block
| TryOnce

type ModDictTaskRes = 
| Accomplished
| Exn
| InUse
| Concurrency
| Timeout

type ModDictTaskStats = 
    {
        mutable total:int64;
        mutable accomplished:int64;
        mutable exn:int64;
        mutable inuse:int64;
        mutable concurrency:int64;
        mutable timeout:int64 }
    
    member this.pending() = this.total-this.accomplished-this.exn-this.inuse-this.concurrency-this.timeout

let empty__ModDictTaskStats() = 
    {
        total=0L;
        accomplished=0L;
        exn=0L;
        inuse=0L;
        concurrency=0L;
        timeout=0L}

type ModDict<'k,'v> =
    {
        localizer: 'k->int;
        mods: Dictionary<'k,ModDictItem<'k,'v>>[];
        mutable count: int;
        taskstats:ModDictTaskStats;
        mutable concurrency_limit:int;
        mutable max_concurrency:int;
        mutable current_concurrency:int;
        mutable timeout:int }

    member this.modulo k =
        let p = this.localizer k
        p,this.mods.[p]

    member this.keys() =
        let array = new List<'k>()
        this.mods
        |> Array.iter(fun m -> lock m (fun () -> array.AddRange(m.Keys)))
        array

    member this.array_mdi() =
        let array = new List<ModDictItem<'k,'v>>()
        this.mods
        |> Array.iter(fun m -> lock m (fun () -> array.AddRange(m.Values)))
        array.ToArray()

    member this.array() = 
        this.array_mdi()
        |> Array.map(fun item -> item.data)

    member this.iter iterator = 
        this.mods
        |> Array.iter(fun m -> 
            lock m (fun () -> 
                m.Values
                |> Seq.iter(fun item -> iterator(item.key,item.data))))

    member this.ContainsKey key =
        let p,partition = this.modulo key
        partition.ContainsKey(key)

    member this.Item
        with get key =
            let p,partition = this.modulo key
            partition.[key].data
        and set key value =
            let p,partition = this.modulo key
            lock partition (fun ()->
                if(partition.ContainsKey key)then
                    partition.[key] <- (key,value) |> data__ModDictItem
                else
                    partition.Add(key,(key,value) |> data__ModDictItem)
                    this.count <- System.Threading.Interlocked.Increment(&this.count))

    member this.tryget_mdi key =
        let p,partition = this.modulo key
        lock partition (fun ()->
            if(partition.ContainsKey key)then
                Some(partition.[key])
            else
                None)

    member this.tryget key =
        let p,partition = this.modulo key
        lock partition (fun ()->
            if partition.ContainsKey key then
                Some partition.[key].data
            else
                None)

    member this.getOrAdd key key__o = 
        let p,partition = this.modulo key
        lock partition (fun ()->
            if partition.ContainsKey key then
                Some partition.[key].data
            else
                key__o key)

    member this.remove key =
        let p,partition = this.modulo key
        lock partition (fun ()->
            if partition.ContainsKey key then
                partition.Remove key |> ignore
                this.count <- System.Threading.Interlocked.Decrement(&this.count))

    member this.tryfind predicate = 
        let mutable found = None
        this.mods
        |> Array.iter(fun m ->
            if found.IsNone then
                lock m (fun () ->
                    if m.Count > 0 && found.IsNone then
                        let fo = m.Values |> Seq.tryFind(fun mdi -> predicate mdi.data)
                        if fo.IsSome then
                            found <- fo.Value.data |> Some))
        found

    member this.filter predicate = 
        let found = new List<'v>()
        this.array_mdi()
        |> Array.iter(fun mdi ->
            if predicate mdi.data then
                mdi.data |> found.Add
                ())
        found.ToArray()

    member this.loadOrCreate(key,creator) = 
        let p,partition = this.modulo key
        lock partition (fun ()->
            if partition.ContainsKey key then
                partition.[key]
            else
                let item = creator key
                let mdi = (key,item) |> data__ModDictItem
                partition.Add(key,mdi)
                this.count <- System.Threading.Interlocked.Increment(&this.count)
                mdi)

    member this.monitor(key) = 
        [|  "{\"Key\":\"" + key + "\"";
            ",\"Mod\":" + this.mods.Length.ToString();
            ",\"Count\":" + this.count.ToString() + "}"  |]
        |> String.Concat

    // === Concurrent

    member this.lock exnloggero handler (mdi,prms) =
            
        let mutable res = ModDictTaskRes.Timeout

        if(this.concurrency_limit>0 && this.current_concurrency >= this.concurrency_limit)then
            res <- ModDictTaskRes.Concurrency
        else
            lock(mdi.lockee)(fun() ->

                this.current_concurrency <- System.Threading.Interlocked.Increment(&this.current_concurrency)
                if(this.max_concurrency < this.current_concurrency)then
                    this.max_concurrency <- this.current_concurrency

                mdi.locked <- true
                mdi.lock_since <- DateTime.UtcNow

                res <- 
                    try
                        prms |> handler(mdi)
                        ModDictTaskRes.Accomplished
                    with
                    | ex -> 
                        ex.ToString() |> logsome(exnloggero)
                        ModDictTaskRes.Exn

                this.current_concurrency <- System.Threading.Interlocked.Decrement(&this.current_concurrency)

                mdi.locked <- false)

        res

    member this.concurrent_task exnloggero (mode,creator,handler)(key,prms) =
            
        let mutable res = ModDictTaskRes.Timeout

        try
            //let cts = Async.DefaultCancellationToken
            let cts = new CancellationTokenSource()
            cts.CancelAfter(this.timeout)
            let task = 
                async{
                    let mdi = this.loadOrCreate(key,creator)
                    match mode with
                    | Block -> 
                        res <- (mdi,prms) |> this.lock(exnloggero)(handler)
                    | TryOnce -> 
                        if(mdi.locked = false) then
                            res <- (mdi,prms) |> this.lock(exnloggero)(handler)
                        else
                            res <- ModDictTaskRes.InUse
                    ()}
            Async.RunSynchronously(task,this.timeout,cts.Token)
        with
        | ex -> 
            this.taskstats.timeout <- System.Threading.Interlocked.Increment(&this.taskstats.timeout)
            this.current_concurrency <- System.Threading.Interlocked.Decrement(&this.current_concurrency)

        match res with
        | ModDictTaskRes.Accomplished -> this.taskstats.accomplished <- System.Threading.Interlocked.Increment(&this.taskstats.accomplished)
        | ModDictTaskRes.Exn -> this.taskstats.exn <- System.Threading.Interlocked.Increment(&this.taskstats.exn)
        | ModDictTaskRes.InUse -> this.taskstats.inuse <- System.Threading.Interlocked.Increment(&this.taskstats.inuse)
        | ModDictTaskRes.Concurrency ->
            this.taskstats.inuse <- System.Threading.Interlocked.Increment(&this.taskstats.concurrency)
            this.current_concurrency <- System.Threading.Interlocked.Decrement(&this.current_concurrency)
        | ModDictTaskRes.Timeout -> this.taskstats.timeout <- System.Threading.Interlocked.Increment(&this.taskstats.timeout)
        this.taskstats.total <- System.Threading.Interlocked.Increment(&this.taskstats.total)


let create_mods<'k,'v>
    (comparer:IEqualityComparer<'k>)
    exp2:Dictionary<'k,ModDictItem<'k,'v>>[] =
    let mutable partition =
        let mutable p = 1
        [| 0..exp2-1 |]
            |> Array.iter(fun i -> p<-p*2)
        p
    [| 0..partition-1 |]
        |> Array.map(fun i->new Dictionary<'k,ModDictItem<'k,'v>>(comparer))

type private ComparerInt64() =
    interface IEqualityComparer<int64> with
        member this.Equals(a:int64,b:int64):bool = (a=b)
        member this.GetHashCode(x):int = 0
let private comparerInt64 = new ComparerInt64()

let createMDInt64<'v> exp2 =
    let mods = create_mods<int64,'v> comparerInt64 exp2
    let localizer id = int(id % mods.LongLength)
    {
        localizer = localizer; 
        mods = mods; 
        count = 0; 
        taskstats = empty__ModDictTaskStats(); 
        concurrency_limit = 0;
        max_concurrency = 0;
        current_concurrency = 0;
        timeout = 1000 }

type private ComparerString() =
    interface IEqualityComparer<String> with
        member this.Equals(a:string,b:string):bool = (a=b)
        member this.GetHashCode(x):int = 0
let private comparerString = new ComparerString()
//let private md5 = new System.Security.Cryptography.MD5CryptoServiceProvider()
//let private md5 = System.Security.Cryptography.MD5.Create()

let create_mdIntString<'v> exp2 =
    let mods = create_mods<string,'v> comparerString exp2
    let localizer(k:string) =
        let array =
            k
            |> System.Text.Encoding.UTF8.GetBytes
            |> System.Convert.ToBase64String
            |> System.Text.Encoding.ASCII.GetBytes
            
        let mutable hash = 0
        [| 0..array.Length - 1 |]
        |> Array.iter(fun i -> 
            let v = (array.[i]|>int)
            let mutable w = 1
            [| 0..(i%4) |]
            |> Array.iter(fun k ->
                w <- w * 128)
            hash <- hash + w * v)
        hash <- hash % mods.Length
        if(hash < 0) then
            hash + mods.Length
        else
            hash
    {
        localizer = localizer; 
        mods = mods; 
        count = 0; 
        taskstats = empty__ModDictTaskStats(); 
        concurrency_limit = 0;
        max_concurrency = 0;
        current_concurrency = 0;
        timeout = 1000 }

