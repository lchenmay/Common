﻿module JWebServer.Common

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

type WsMsg = unit
type WsFrame = unit


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
