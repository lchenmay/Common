module Util.Concurrent

open System
open System.Threading
open System.Threading.Tasks
open System.Linq.Expressions

open Microsoft.FSharp.Control
open Microsoft.FSharp.Linq.RuntimeHelpers
open FSharpx.Control

let await task = 
    async{
        return! Async.AwaitTask task
    }
    |> Async.RunSynchronously

let lambda__linqExp<'T> lambda = 
    lambda
    |> LeafExpressionConverter.QuotationToExpression
    |> unbox<Expression<'T>>

let fun__Func<'T> (body:'T -> unit) = 
    let f = 
        (fun input ->
            Task.Run(fun _ -> body input))
    new Func<'T,Task>(f)

let fun__FuncUnit (body:unit -> unit) = 
    let f = 
        (fun input ->
            Task.Run(fun _ -> body()))
    new Func<Task>(f)

let wait(criterion:unit -> bool, timeout, ratio)(logging:string -> unit):bool =

    let since = DateTime.UtcNow
    let mutable expiry = 1.0

    let mutable success_timeout = false

    let mutable cycle = true
    while(cycle) do

        if(criterion() = false) then
            success_timeout <- true
            cycle <- false
        else
            let checkpoint = since.AddSeconds(expiry)
            if(DateTime.UtcNow.Subtract(since).TotalMinutes < timeout) then
                if(DateTime.UtcNow.Ticks >= checkpoint.Ticks) then
                    logging("Retry next " + expiry.ToString("0.00") + "s ...")
                    expiry <- expiry * ratio
                System.Threading.Thread.Sleep(1000)
            else
                cycle <- false

    success_timeout


// ref http://www.fssnip.net/d4/title/Asynchronous-cancellation-of-a-workflow
let internal synchronize f = 
    let ctx = System.Threading.SynchronizationContext.Current 
    f (fun g ->
    let nctx = System.Threading.SynchronizationContext.Current 
    if ctx <> null && ctx <> nctx then ctx.Post((fun _ -> g()), null)
    else g() )

//type Microsoft.FSharp.Control.Async with 
//    static member GuardedAwaitObservable (ev1:IObservable<'T1>) guardFunction =
//        synchronize (fun f ->
//            Async.FromContinuations((fun (cont,econt,ccont) -> 
//                let rec finish cont value = 
//                    remover.Dispose()
//                    f (fun () -> cont value)
//                and remover : IDisposable = 
//                    ev1.Subscribe({ new IObserver<_> with
//                        member x.OnNext(v) = finish cont v
//                        member x.OnError(e) = finish econt e
//                        member x.OnCompleted() = 
//                            let msg = "Cancelling the workflow, because the Observable awaited using AwaitObservable has completed."
//                            finish ccont (new System.OperationCanceledException(msg)) }) 
//                guardFunction() )))

//let StartCancellable work = async {
//    let cts = new CancellationTokenSource()
//    let evt = new Event<_>()
//    Async.Start(Async.TryCancelled(work, ignore >> evt.Trigger), cts.Token)
//    let waitForCancel = Microsoft.FSharp.Control.Async.GuardedAwaitObservable evt.Publish cts.Cancel
//    return async.TryFinally(waitForCancel, cts.Dispose) }

//let exe_timeout(timeout:int)(body) = 

//    async {
//        let! cancelToken = StartCancellable(body)
//        do! Async.Sleep(timeout)
//        do! cancelToken 
//    }
//    |> Async.Start


//let exe_timeout_test() = 

//    async{
//        let mutable i = 0
//        while(i < 300) do
//            System.Console.WriteLine("- Tick " + i.ToString())
//            i <- i + 1
//            System.Threading.Thread.Sleep(300)
//    }
//    |> exe_timeout(3000)

//    while(true) do
//        System.Console.WriteLine(" ... ")
//        System.Threading.Thread.Sleep(1000)

let waitSleep (interval:int) predicate = 
    while predicate() do
        System.Threading.Thread.Sleep interval

let asyncCycler body = 
    async{
        while true do
            body()
    }
    |> Async.Ignore
    |> Async.Start

let asyncCyclerInterval (interval:int) body = 
    async{
        while true do
            body()
            System.Threading.Thread.Sleep interval
    }
    |> Async.Ignore
    |> Async.Start

let asyncCyclerIntervalTS (interval:TimeSpan) body = 
    async{
        while true do
            body()
            Thread.Sleep interval
    }
    |> Async.Ignore
    |> Async.Start

let asyncCyclerTry exception_handler body = 
    async{
        while true do
            try
                body()
            with
            | ex -> ex |> exception_handler
    }
    |> Async.Ignore
    |> Async.Start

let asyncCyclerIntervalTry interval exception_handler body = 
    async{
        while true do
            try
                body()
            with
            | ex -> ex |> exception_handler

            if interval > 0 then
                System.Threading.Thread.Sleep interval
    }
    |> Async.Ignore
    |> Async.Start

let threadCyclerIntervalTry priority interval exception_handler body = 

    let f() =
        while true do
            try
                body()
            with
            | ex -> ex |> exception_handler

            if interval > 0 then
                System.Threading.Thread.Sleep interval

    let t = new Thread(f)
    t.Priority <- ThreadPriority.Highest
    t.Start()


let asyncInNewThread body = 
    async {
        do! Async.SwitchToNewThread()
        do! async { body() }
    }
    |> Async.StartImmediate

let asyncTryInNewThread interval exception_handler body = 
    async {
        do! Async.SwitchToNewThread()
        do! async { 
            while true do
                try
                    body()
                with
                | ex -> ex |> exception_handler
                
                if interval > 0 then
                    System.Threading.Thread.Sleep interval }
    }
    |> Async.StartImmediate

let asyncProcess body = 
    async { body() } 
    |> Async.Start

let logsome(o:(string->unit)option) s = 
    if o.IsSome then
        o.Value s
    else
        ()

let cycle_randomly(elapsor:TimeSpan -> float,threshould)(timestamp:DateTime)(handler) = 
    let now = DateTime.UtcNow
    if(elapsor(now.Subtract(timestamp)) > threshould*(1.0+float(now.Millisecond)/1000.0))then
        handler(now)

let cycle_randomly_min(threshould)(timestamp)(handler) = cycle_randomly((fun ts -> ts.TotalMinutes),threshould)(timestamp)(handler)
    
let cycle_randomly_sec(threshould)(timestamp)(handler) = cycle_randomly((fun ts -> ts.TotalSeconds),threshould)(timestamp)(handler)

let asyncWithTimeout timeout mainProcess exProcess = 
    let task = 
        async {
            mainProcess()
        }  
    try 
        Async.RunSynchronously(task, timeout)
    with ex -> exProcess ex
            
let processMultipleOrWait handler (idle:int) (items:'T[]) = 
    if items.Length > 0 then
        items
        |> Array.Parallel.iter handler
    else
        Thread.Sleep idle
