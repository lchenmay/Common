﻿module Util.WebServer

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
id: int64 
client: TcpClient
buffer: Buffer
mutable idleSince: DateTime
mutable state: ConnState }

type Engine<'Runtime> = {
output: string -> unit
plugino: (HttpRequest -> byte[] option) option
folder: string
defaultHtml: string
runtime: 'Runtime
wsHandler: byte[] -> byte[] option
port:int
listener: TcpListener
buffers: ConcurrentStack<Buffer>
connId: ref<int64>
queue: ModDict<int64,Conn>
keeps: ConcurrentDictionary<int64,Conn> }
with override this.ToString() = 
        [|  "ID " + this.connId.Value.ToString() 
            "ConnRcv " + this.queue.count.ToString() |]
        |> String.concat crlf

let engine__monitor engine = 
    [|  ("connId", engine.connId.ToString() |> Json.Str)
        ("queue", engine.queue.count.ToString() |> Json.Str)
        ("keeps", engine.keeps.Count.ToString() |> Json.Str) |]
    |> Json.Braket

let bufferAsNull = {
    bb = new BytesBuilder()
    bin = [||] }

let checkoutConn engine id client = 

    {
        id = id
        client = client 
        buffer = 
            let r = ref bufferAsNull
            while engine.buffers.TryPop r = false do
                ()
            r.Value

        idleSince = DateTime.UtcNow
        state = ConnState.Idle }

let drop engine (stream:Stream) conn = 
    stream.Close()
    conn.client.Close()
                
    conn.buffer
    |> engine.buffers.Push


let fileService root defaultHtml req = 

    if req.pathline = "/" then
        Path.Combine(root, defaultHtml)
        |> IO.File.ReadAllText
        |> str__StandardResponse "text/html"
    elif req.path.Length > 0 then
        let file = Array.append [|root|] req.path |> Path.Combine
        if File.Exists file then
            let filename = req.path[req.path.Length - 1]
            match (Path.GetExtension filename).ToLower() with
            | ".jpg" | ".png" | ".gif" | ".ico" as ext -> 
                file
                |> IO.File.ReadAllBytes
                |> bin__StandardResponse ("image/" + (ext.Substring 1))
            | ".css" | ".html" | ".javascript" | ".txt" | ".xml" as ext ->
                file
                |> IO.File.ReadAllText
                |> str__StandardResponse ("text/" + (ext.Substring 1))
            | ".js" ->
                file
                |> IO.File.ReadAllText
                |> str__StandardResponse "text/javascript; charset=utf-8"
            | ".mp4" ->
                file
                |> IO.File.ReadAllBytes
                |> bin__StandardResponse "video/mp4"
            | _ -> [||]
        else [||]
    else [||]


let prepEngine 
    output
    plugino
    folder
    defaultHtml
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
        plugino = plugino
        folder = folder
        defaultHtml = defaultHtml
        runtime = runtime
        wsHandler = wsHandler
        port = port
        listener = new TcpListener(IPAddress.Any, port)
        buffers = buffers
        connId = ref 0L
        queue = createMDInt64<Conn> 8
        keeps = new ConcurrentDictionary<int64,Conn>() }

let recv conn = 

    let bb,bin = conn.buffer.bb,conn.buffer.bin
    let stream = conn.client.GetStream() // :> Stream

    while stream.DataAvailable = false do
        ()

    let bs = Array.zeroCreate conn.client.Available
    let count = stream.Read(bs, 0, bs.Length)
    bb.append bs

    let bin = bb.bytes()
    bb.clear()
    stream,bin


let recvIncoming engine conn = 

    async{
        [|  "Conn [" + conn.id.ToString() + "]"
            " = " + conn.state.ToString() |]
        |> String.Concat
        |> engine.output

        "Conn [" + conn.id.ToString() + "] Receving ..."
        |> engine.output
    
        let stream,incoming = recv conn

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
                        
                    let repo = 
                        match engine.plugino with
                        | Some plugin -> plugin req
                        | None -> None

                    match repo with
                    | Some v -> v
                    | None -> 
                        fileService 
                            engine.folder 
                            engine.defaultHtml 
                            req

                stream.Write(outgoing,0,outgoing.Length)
                stream.Flush()

            | None -> ()

            "Drop"
            |> engine.output

            drop engine stream conn

        | HttpRequestWithWS.WebSocketUpgrade upgrade -> 

            "Upgrade"
            |> engine.output

            stream.Write(upgrade,0,upgrade.Length)
            stream.Flush()

            conn.state <- ConnState.Keep
            engine.keeps[conn.id] <- conn

            "Rcv -> Keep"
            |> engine.output

        | _ -> 

            "Drop"
            |> engine.output

            drop engine stream conn
    }

let cycleAccept engine = 
    async{
        while true do
            try
                let client = engine.listener.AcceptTcpClient()
                let id = Interlocked.Increment engine.connId

                let conn = checkoutConn engine id client
                engine.queue[conn.id] <- conn

                "Client accepted [" + id.ToString() + "], Queue = " + engine.queue.count.ToString()
                |> engine.output

                //s.Close()
            with
            | ex ->
                ex.ToString() |> engine.output
                "Accepting thread" |> engine.output
    }

let cycleRcv engine = 
    async{
        while true do
            try
                let items = engine.queue.array()

                items
                |> Array.iter(fun conn -> 
                    recvIncoming engine conn
                    |> Async.Start)
                engine.queue.clear()
            with
            | ex ->
                ex.ToString() |> engine.output
                "Rcving thread" |> engine.output
    }

let cycleWs engine =
    async{
        while true do
            try
                engine.keeps.Values
                |> Seq.toArray
                |> Array.Parallel.iter(fun conn -> 
                    "Conn [" + conn.id.ToString() + "] Receving ..."
                    |> engine.output
    
                    let stream,incoming = recv conn

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
                        rep
                        |> wsEncode
                        |> stream.Write
                    | None -> ())
            with
            | ex ->
                ex.ToString() |> engine.output
                "Ws thread" |> engine.output
    }

let startEngine engine = 

    "Listening at: " + engine.port.ToString()
    |> engine.output

    engine.listener.Start()

    cycleAccept engine 
    |> Async.Start
    
    cycleRcv engine
    |> Async.Start

    cycleWs engine
    |> Async.Start
    

