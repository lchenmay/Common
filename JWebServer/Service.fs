module JWebServer.Service

open System.Net
open System.Net.Sockets
open System.Threading
open System.Collections.Concurrent

open Util.CollectionModDict
open Util.Concurrent
open Util.Bin

open JWebServer.Common
open JWebServer.Net

let prepEngine 
    output
    echo
    folder
    defaultHtml
    h404o
    runtime
    wsHandler
    port = 

    let buffers = new ConcurrentStack<Buffer>()
    [| 0 .. 1000 - 1|]
    |> Array.iter(fun i -> 
        let buffer = {
            bb = new BytesBuilder()
            bin = Array.zeroCreate bufferLength }
        buffers.Push buffer)

    {
        output = output
        echo = echo
        folder = folder
        defaultHtml = defaultHtml
        h404o = h404o
        runtime = runtime
        wsHandler = wsHandler
        port = port
        listener = new TcpListener(IPAddress.Any, port)
        connId = ref 0L
        queue = createMDInt64<Conn> 8
        keeps = createMDInt64<Conn> 8 }

let startEngine engine = 

    "Listening at: " + engine.port.ToString()
    |> engine.output

    engine.listener.Start()

    let exHandler thread (ex:exn) =
        ex.ToString() |> engine.output
        thread |> engine.output

    threadCyclerIntervalTry ThreadPriority.Highest 30 (exHandler "Accept") (cycleAccept engine)
    threadCyclerIntervalTry ThreadPriority.AboveNormal 30 (exHandler "Rcv") (cycleRcv engine)
    threadCyclerIntervalTry ThreadPriority.AboveNormal 30 (exHandler "WS") (cycleWs engine)
    

