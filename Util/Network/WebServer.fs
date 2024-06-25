module Util.WebServer

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

type ConnState = 
| Idle
| Rcv
| Snd
| Keep

let bufferLength = 16 * 1024

type Buffer = {
bb: BytesBuilder
bin: byte[] }

type Conn = {
since: DateTime
id: int64 
client: TcpClient
ns: NetworkStream
mutable idleSince: DateTime
mutable state: ConnState }

type Engine<'Runtime> = {
output: string -> unit
echo: HttpRequest -> byte[] option
folder: string
defaultHtml: string
h404o: (unit -> byte[]) option
runtime: 'Runtime
wsHandler: byte[] -> byte[] option
port:int
listener: TcpListener
connId: ref<int64>
queue: ModDict<int64,Conn>
keeps: ModDict<int64,Conn> }
with override this.ToString() = 
        [|  "ID " + this.connId.Value.ToString() 
            "ConnRcv " + this.queue.count.ToString() |]
        |> String.concat crlf

let engine__monitor engine = 
    [|  ("connId", engine.connId.ToString() |> Json.Str)
        ("queue", engine.queue.count.ToString() |> Json.Str)
        ("keeps", engine.keeps.count.ToString() |> Json.Str) |]
    |> Json.Braket

let bufferAsNull = {
    bb = new BytesBuilder()
    bin = [||] }

let checkoutConn engine id client = 

    {
        since = DateTime.UtcNow
        id = id
        client = client 
        ns = client.GetStream()
        idleSince = DateTime.UtcNow
        state = ConnState.Idle }

let drop engine (collectiono:ModDict<int64,Conn> option) conn = 

    match collectiono with
    | Some collection ->
        if collection.ContainsKey conn.id then
            collection.remove conn.id
    | None -> ()

    try
        conn.ns.Close()
    with
    | ex -> ()

    try
        conn.client.Close()
    with
    | ex -> ()


let fileService root defaultHtml req = 

    if req.pathline = "/" then
        Path.Combine(root, defaultHtml)
        |> IO.File.ReadAllText
        |> str__StandardResponse "text/html"
        |> Some
    elif req.path.Length > 0 then
        let file = Array.append [|root|] req.path |> Path.Combine
        if File.Exists file then
            let filename = req.path[req.path.Length - 1]

            let fileext = 
                req.path[req.path.Length - 1]
                |> Path.GetExtension

            let mime = 
                match fileext.ToLower() with
                | ".jpg" as ext -> 
                    "image/jpeg"
                | ".svg" | ".svgz" as ext -> 
                    "image/svg+xml"
                | ".jpeg" | ".png" | ".gif" | ".ico" | ".webp" as ext -> 
                    "image/" + (ext.Substring 1)
                | ".css" | ".html" | ".javascript" | ".txt" | ".xml" as ext ->
                    "text/" + (ext.Substring 1)
                | ".js" ->
                    "text/javascript; charset=utf-8"
                | ".mp4" ->
                    "video/mp4"
                | _ -> ""

            file
            |> IO.File.ReadAllBytes
            |> bin__StandardResponse mime
            |> Some

        else None
    else None


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

let cycleAccept engine = 

    "Accept" |> engine.output

    let client = engine.listener.AcceptTcpClient()
    let id = Interlocked.Increment engine.connId

    let conn = checkoutConn engine id client
    engine.queue[conn.id] <- conn

    "Client accepted [" + id.ToString() + "], Queue = " + engine.queue.count.ToString()
    |> engine.output

    //s.Close()

let cycleRcv engine = 

    let all = engine.queue.array()

    let items = 
        all
        |> Array.filter(fun conn -> conn.ns.DataAvailable)

    //[|  "Rcv " + items.Length.ToString() 
    //    " / "
    //    + all.Length.ToString() |]
    //|> String.Concat
    //|> engine.output

    items
    |> Array.iter(fun conn -> 
        rcv engine conn
        |> Async.Start)

let cycleWs engine =

    let all = 
        engine.keeps.array()
        |> Array.filter(fun conn -> conn.ns.DataAvailable)

    let items = 
        all
        |> Array.filter(fun conn -> conn.ns.DataAvailable)
    
    //[|  "Ws " + items.Length.ToString() 
    //    " / "
    //    + all.Length.ToString() |]
    //|> String.Concat
    //|> engine.output

    items
    |> Array.Parallel.iter(fun conn -> 
        "Conn [" + conn.id.ToString() + "] Receving ..."
        |> engine.output
    
        match read conn with
        | Some incoming ->

            "Incomining " + incoming.Length.ToString() + " bytes"
            |> engine.output

            incoming
            |> hex
            |> engine.output

            match wsDecode incoming with
            | Some bs -> 
                engine.output "Decoded:"
                bs
                |> hex
                |> engine.output
            | None -> engine.output "Decode failed"
                    
            match engine.wsHandler incoming with
            | Some rep -> 
                try
                    rep
                    |> wsEncode
                    |> conn.ns.Write
                with
                | ex -> drop engine (Some engine.keeps) conn
            | None -> ()
            
        | None -> drop engine (Some engine.keeps) conn)

let startEngine engine = 

    "Listening at: " + engine.port.ToString()
    |> engine.output

    engine.listener.Start()

    let exHandler note (ex:exn) =
        ex.ToString() |> engine.output
        note |> engine.output
        ()

    (fun _ -> cycleAccept engine)
    |> threadCyclerIntervalTry ThreadPriority.Highest 30 (exHandler "Accept")
    
    (fun _ -> cycleRcv engine)
    |> threadCyclerIntervalTry ThreadPriority.AboveNormal 30 (fun ex ->
        ex.ToString() |> engine.output
        "Rcv" |> engine.output
        ())


    (fun _ -> cycleWs engine)
    |> threadCyclerIntervalTry ThreadPriority.AboveNormal 30 (fun ex ->
        ex.ToString() |> engine.output
        "WS" |> engine.output
        ())
    

