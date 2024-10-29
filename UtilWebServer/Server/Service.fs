module UtilWebServer.Server.Service

open System.Net
open System.Net.Sockets
open System.Threading
open System.Collections.Concurrent
open System.Collections.Generic
open System.Linq

open Util.CollectionModDict
open Util.Concurrent
open Util.Bin
open Util.Json

open UtilWebServer.Common
open UtilWebServer.Server.Common
open UtilWebServer.Server.Net
open Fleck

    
let PushAll runtime json = 
    runtime.keeps.Values
    |> Array.Parallel.iter(sndJson runtime json)

let startEngine runtime = 

    let buffers = new ConcurrentStack<Buffer>()
    [| 0 .. 1000 - 1|]
    |> Array.iter(fun i -> 
        let buffer = {
            bb = new BytesBuilder()
            bin = Array.zeroCreate bufferLength }
        buffers.Push buffer)

    runtime.listener <- new TcpListener(IPAddress.Any, runtime.host.port)

    "Listening at: " + runtime.host.port.ToString()
    |> runtime.output

    runtime.listener.Start()

    let exHandler thread (ex:exn) =
        ex.ToString() |> runtime.output
        thread |> runtime.output

    threadCyclerIntervalTry ThreadPriority.Highest 30 (exHandler "Accept") (cycleAccept runtime)
    threadCyclerIntervalTry ThreadPriority.AboveNormal 30 (exHandler "Rcv") (cycleRcv runtime)
    threadCyclerIntervalTry ThreadPriority.AboveNormal 30 (exHandler "WS") (cycleWs runtime)

let startWebSocket runtime =
    
    let allSockets = new List<IWebSocketConnection>()
    let server = new WebSocketServer("ws://127.0.0.1:5045")

    server.Start(fun socket ->
        socket.OnOpen <- fun _ ->
            ("Open") |> runtime.output
            allSockets.Add(socket)

        socket.OnClose <- fun _ ->
            ("Close") |> runtime.output
            allSockets.Remove(socket) |>ignore

        socket.OnMessage <- fun message ->
            message |> runtime.output
            allSockets.ToList().ForEach(fun s -> s.Send(message) |>ignore)

        socket.OnError <- fun error ->
            error.Message |> runtime.output
    )

