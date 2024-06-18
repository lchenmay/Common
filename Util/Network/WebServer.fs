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
plugino: (HttpRequest -> byte[]) option
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

let startEngine engine = 

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

                let conn = item.Value
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

                match checkHttpUpgrade incoming with
                | _,false -> 

                    ()
                | upgrade,_ ->

                    if upgrade.Length > 0 then
                        stream.Write(upgrade,0,upgrade.Length)
                        stream.Flush()
                    else

                        let outgoing = [||]
                        stream.Write(outgoing,0,outgoing.Length)
                        stream.Flush()
                        stream.Close()
                        conn.client.Close()
                
                        conn.buffer
                        |> engine.buffers.Push

                        engine.connsRcv.Remove(conn.id,ref conn)
                        |> ignore)
        ())
    |> Util.Concurrent.asyncProcess


