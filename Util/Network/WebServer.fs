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
open Util.Bin
open Util.Text
open Util.Perf
open Util.Http
open Util.HttpServer
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
state: ConnState }

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
connsRcv: ConcurrentDictionary<int64,Conn>
connsKeep: ConcurrentDictionary<int64,Conn> }
with override this.ToString() = 
        [|  "ID " + this.connId.Value.ToString() 
            "ConnRcv " + this.connsRcv.Count.ToString() |]
        |> String.concat crlf

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

    engine.connsRcv.Remove(conn.id,ref conn)
    |> ignore


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
        connsRcv = new ConcurrentDictionary<int64,Conn>()
        connsKeep = new ConcurrentDictionary<int64,Conn>() }

let processConn engine conn = 
    let bb,bin = conn.buffer.bb,conn.buffer.bin
    let stream = conn.client.GetStream() :> Stream

    let incoming = 
        let mutable keep = true
        while keep do

            let count = stream.Read(bin, 0, bin.Length)

            bb.append(bin,count)
            if count < bin.Length then
                keep <- false

        bb.bytes()

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

        drop engine stream conn

    | HttpRequestWithWS.WebSocketUpgrade upgrade -> 

        stream.Write(upgrade,0,upgrade.Length)
        stream.Flush()

    | _ -> 
        drop engine stream conn


let startEngine engine = 

    "Listening at: " + engine.port.ToString()
    |> engine.output

    (fun _ ->
        engine.listener.Start()
        while true do 
            let client = engine.listener.AcceptTcpClient()
            let id = Interlocked.Increment engine.connId
            engine.connsRcv[id] <- checkoutConn engine id client
            //s.Close()
        ())
    |> Util.Concurrent.asyncProcess

    (fun _ ->
        while true do 
            engine.connsRcv.ToArray()
            |> Array.Parallel.iter(fun item -> 
                processConn engine item.Value))
    |> Util.Concurrent.asyncProcess


