module JWebServer.Net

open LanguagePrimitives

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading
open System.Net.WebSockets
open System.Collections.Generic
open System.Collections.Concurrent

open Util.Cat
open Util.ADT
open Util.CollectionModDict
open Util.Concurrent
open Util.Bin
open Util.Text
open Util.Perf
open Util.Http
open Util.HttpServer
open Util.WebSocket
open Util.Concurrent

open JWebServer.Common
open JWebServer.File

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

let outputHex engine caption hexData = 
    caption |> engine.output

    hexData
    |> hex
    |> engine.output

let read conn = 

    try
        let bb = new BytesBuilder()

        let bs = Array.zeroCreate conn.client.Available
        let count = conn.ns.Read(bs, 0, bs.Length)
        bb.append bs

        let bin = bb.bytes()

        bin
        |> Some

     with
    | ex -> None

let rcv engine conn = 

    async{
        [|  "Conn [" + conn.id.ToString() + "]"
            " = " + conn.state.ToString() |]
        |> String.Concat
        |> engine.output

        "Conn [" + conn.id.ToString() + "] Receving ..."
        |> engine.output
    

        match read conn with
        | Some incoming -> 

            "Incomining " + incoming.Length.ToString() + " bytes"
            |> engine.output

            incoming
            |> hex
            |> engine.output

            match incomingProcess incoming with
            | HttpRequestWithWS.Echo (reqo,(headers,body)) ->

                match reqo with
                | Some req ->

                    let outgoing =
                        req
                        |> engine.echo
                        |> oPipelineNone (fun _ -> 
                            fileService 
                                engine.folder 
                                engine.defaultHtml 
                                req)
                        |> oPipelineNoneHandlero [||] engine.h404o
                        |> Option.get

                    try
                        conn.ns.Write(outgoing,0,outgoing.Length)
                    with
                    | ex -> ()

                | None -> ()

            | HttpRequestWithWS.WebSocketUpgrade upgrade -> 

                "Upgrade"
                |> engine.output

                conn.ns.Write(upgrade,0,upgrade.Length)

                conn.state <- ConnState.Keep
                engine.keeps[conn.id] <- conn

                "Rcv -> Keep"
                |> engine.output

            | _ -> ()
        | None -> ()
        
        if conn.state <> ConnState.Keep then
            drop engine (Some engine.queue) conn
    }

let cycleAccept engine = fun () ->
    "Accept" |> engine.output

    let client = engine.listener.AcceptTcpClient()
    let id = Interlocked.Increment engine.connId

    let conn = checkoutConn engine id client
    engine.queue[conn.id] <- conn

    "Client accepted [" + id.ToString() + "], Queue = " + engine.queue.count.ToString()
    |> engine.output

    //s.Close()

let cycleRcv engine = fun () ->
    engine.queue.array()
    |> Array.filter(fun conn -> conn.ns.DataAvailable)
    |> Array.iter(fun conn -> rcv engine conn |> Async.Start)

let cycleWs engine = fun () ->

    engine.keeps.array()
    |> Array.filter(fun conn -> conn.ns.DataAvailable)
    |> Array.Parallel.iter(fun conn -> 
    
        match read conn with
        | Some incoming ->
            incoming |> outputHex engine "WS Incoming Raw:"
            match wsDecode incoming with
            | Some decoded -> 
                decoded |> outputHex engine "WS Incoming Decoded:"
                match engine.wsHandler decoded with
                | Some rep ->
                    rep |> outputHex engine "WS Outgoging Raw:"
                    try
                        let encoded = rep |> wsEncode
                        encoded |> outputHex engine "WS Outgoging Encoded:"
                        encoded |> conn.ns.Write
                    with
                    | ex -> drop engine (Some engine.keeps) conn
                | None -> ()
            | None -> engine.output "Decode failed"
        | None -> drop engine (Some engine.keeps) conn)

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
    

