module Util.Perf

open System
open System.Collections.Concurrent
open System.Threading


let timeout f mil h = 
    let since = DateTime.UtcNow
    let res = f()
    if (DateTime.UtcNow.Subtract since).TotalMicroseconds > mil then
        h()
    res

//██████████████████████████████████████████████████████████████████████████████

type CwRecord = {
    mutable count:int64
    mutable max:int64
    mutable sum:int64
    mutable start:int64
    mutable last:Int64
    histogram:int64[]
}

let cws = new ConcurrentDictionary<string, CwRecord>()
let queue = new ConcurrentQueue<string * DateTime * TimeSpan>()

let stats filtero =

    cws.Keys
    |> Seq.toArray
    |> Array.sort
    |> Array.choose (fun k ->
        if (match filtero with
            | Some f -> f k
            | None -> true) then

            let v = cws.[k]
            let elapse = (TimeSpan.FromTicks v.sum).TotalMilliseconds
            let max = (TimeSpan.FromTicks v.max).TotalMilliseconds
            let mil = (TimeSpan.FromTicks v.sum).TotalMilliseconds / (float v.count)

            let histogram = 
                v.histogram
                |> Array.map(fun i -> i.ToString())
                |> String.concat "|"

            Some(k,v,elapse,max,mil,histogram)
        else None)

let statTextLines filtero = 
    stats filtero
    |> Array.map(fun line -> 
        let k,v,elapse,max,mil,histogram = line

        let total = v.sum |> TimeSpan.FromTicks
        let mean = 
            (float v.sum) / (float v.count) 
            |> int64 
            |> TimeSpan.FromTicks

        [|  ("[" + k + "] " ).PadRight(50,'_')
            " "
            total.TotalMilliseconds.ToString("0.000").PadLeft(12) + " mil = "
            mean.TotalMilliseconds.ToString("0.000").PadLeft(8) + " mil x "
            v.count.ToString() |]
        |> String.Concat)

let stat__array filtero = 
    stats filtero
    |> Array.map(fun line -> 
        let k,v,elapse,max,mil,histogram = line

        let total = v.sum |> TimeSpan.FromTicks
        let mean = 
            (float v.sum) / (float v.count) 
            |> int64 
            |> TimeSpan.FromTicks
        let rps =
            let measurespan = (v.last - v.start) |> TimeSpan.FromTicks
            (float v.count) / measurespan.TotalSeconds

        sprintf "{\"key\":\"%s\",\"count\":%d,\"totalMil\":\"%s\",\"avgMil\":\"%s\",\"rps\":\"%s\"}"
                k 
                v.count
                (total.TotalMilliseconds.ToString("0.000").PadLeft(12))
                (mean.TotalMilliseconds.ToString("0.000").PadLeft(8))
                (rps.ToString("0.000").PadLeft(8)))

let perfEcho filter =
    filter
    |> Some
    |> stat__array
    |> String.concat ","
    |> sprintf "{\"Error\":\"OK\",\"perfs\":[%s]}"

let scale = 4

let recordCw(key,(since:DateTime),ticks,o) = 

    let r = cws.GetOrAdd(key, fun _ -> 
        { 
            count = 0L
            max = 0L
            sum = 0L
            start = DateTime.UtcNow.Ticks
            last = DateTime.UtcNow.Ticks
            histogram = Array.zeroCreate (64 * scale) })

    r.sum <- r.sum + ticks
    r.count <- r.count + 1L
    if r.max < ticks then
        r.max <- ticks
    r.last <- since.Ticks

    match o with
    | Some counter -> ()
    | None -> ()

    let log = 
        let mutable v = 
            (float scale) * (ticks |> float |> Math.Log2)
            |> int
        if v > r.histogram.Length - 1 then
            r.histogram.Length - 1
        elif v < 0 then 0
        else
            v
    if log >= 0 && log < r.histogram.Length then
        r.histogram.[log] <- r.histogram.[log] + 1L

    if key.StartsWith "*" = false then
        queue.Enqueue(key,since,TimeSpan.FromTicks ticks)
        if queue.Count > 10000 then
            queue.TryDequeue() |> ignore

let resetCw (key:String) =
    cws.TryRemove key |> ignore

type CodeWrapper(key) =
    let since = DateTime.UtcNow
    interface IDisposable with
        member this.Dispose() = recordCw(key,since,DateTime.UtcNow.Ticks - since.Ticks,None)

//██████████████████████████████████████████████████████████████████████████████

type ConcurrencyRecord = {
    mutable count:ref<int>
    mutable max:int
}

let crs = new ConcurrentDictionary<string, ConcurrencyRecord>()

type ConcurrencyWrapper(key) =
    let rcd = crs.GetOrAdd(key, fun _ -> { 
        count = ref 0
        max = 0 })
    let concurrency = Interlocked.Increment rcd.count
    interface IDisposable with
        member this.Dispose() = 
            if rcd.max < concurrency then
                rcd.max <- concurrency
            Interlocked.Decrement rcd.count |> ignore

//██████████████████████████████████████████████████████████████████████████████

type IntRecord = {
    mutable count:int64
    mutable max:int64
    mutable sum:int64
    histogram:int64[]
}

let iws = new ConcurrentDictionary<string, IntRecord>()

let recordIw(key,since,v) = 

    let r = iws.GetOrAdd(key, fun _ -> 
        { 
            count = 0L
            max = 0L
            sum = 0L
            histogram = Array.zeroCreate 64 })

    r.sum <- r.sum + v
    r.count <- r.count + 1L
    if r.max < v then
        r.max <- v

    let log = 
        let mutable v = 
            (v |> float |> Math.Log2)
            |> int
        if v > r.histogram.Length - 1 then
            r.histogram.Length - 1
        elif v<0 then 0
        else
            v
    if log >=0 && log < r.histogram.Length then
        r.histogram.[log] <- r.histogram.[log] + 1L

type IntWrapper(key,v) =
    let since = DateTime.UtcNow
    interface IDisposable with
        member this.Dispose() = recordIw(key,since,v)

//██████████████████████████████████████████████████████████████████████████████

type MaRecord = {
    mutable count:int64;
    mutable live:int64;
    mutable max_live:int64;
    mutable sum:int64 }

let h_ma = new ConcurrentDictionary<string, MaRecord>()

let log_ma(key, ticks) =

    let r = h_ma.GetOrAdd(key, fun key -> 
        { 
            count = 0L; 
            live = 0L;
            max_live = 0L;
            sum = 0L })

    r.live <- r.live - 1L

    r.sum <- r.sum + ticks
    r.count <- r.count + 1L

type MemAlloc(key) =

    let since = DateTime.UtcNow

    let r = 
        
        let r = h_ma.GetOrAdd(key, fun key -> 
            { 
                count = 0L; 
                live = 0L;
                max_live = 0L;
                sum = 0L })

        r.count <- r.count + 1L
        r.live <- r.live + 1L
        if(r.max_live < r.live)then
            r.max_live <- r.live

        r

    override this.Finalize() = 
        log_ma(key, DateTime.UtcNow.Ticks - since.Ticks)

