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

let startEngine listener = 

    let buffers = new ConcurrentStack<Buffer>()
    [| 0 .. 1000 - 1|]
    |> Array.iter(fun i -> 
        let buffer = {
            bb = new BytesBuilder()
            bin = Array.zeroCreate bufferLength }
        buffers.Push buffer)

    "Listening at: " + listener.port.ToString()
    |> output

    listener.socket.Start()

    let exHandler thread (ex:exn) =
        ex.ToString() |> output
        thread |> output

    threadCyclerIntervalTry ThreadPriority.Highest 30 (exHandler "Accept") (cycleAccept listener)
    threadCyclerIntervalTry ThreadPriority.AboveNormal 30 (exHandler "Rcv") (cycleRcv listener)
    threadCyclerIntervalTry ThreadPriority.AboveNormal 30 (exHandler "WS") (cycleWs listener)

let startWebSocket listener =
    
    let allSockets = new List<IWebSocketConnection>()
    let server = new WebSocketServer("ws://127.0.0.1:5045")

    server.Start(fun socket ->
        socket.OnOpen <- fun _ ->
            ("Open") |> listener.output
            allSockets.Add(socket)

        socket.OnClose <- fun _ ->
            ("Close") |> listener.output
            allSockets.Remove(socket) |>ignore

        socket.OnMessage <- fun message ->
            message |> listener.output
            allSockets.ToList().ForEach(fun s -> s.Send(message) |>ignore)

        socket.OnError <- fun error ->
            error.Message |> listener.output
    )

