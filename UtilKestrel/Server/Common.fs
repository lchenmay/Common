module UtilWebServer.Server.Common

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

open UtilWebServer.Common

type WsMsg = unit
type WsFrame = unit


let bufferLength = 16 * 1024

type Buffer = {
bb: BytesBuilder
bin: byte[] }


let bufferAsNull = {
    bb = new BytesBuilder()
    bin = [||] }

let checkoutConn id client = 

    {
        since = DateTime.UtcNow
        id = id
        client = client 
        ns = client.GetStream()
        idleSince = DateTime.UtcNow
        state = ConnState.Idle }

let drop (collectiono:ModDict<int64,Conn> option) conn = 

    match collectiono with
    | Some collection -> collection.Remove conn.id
    | None -> ()

    try
        conn.ns.Close()
    with
    | ex -> ()

    try
        conn.client.Close()
    with
    | ex -> ()
