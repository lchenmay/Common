module UtilWebServer.Server.Monitor

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
open Util.Json
open Util.Perf
open Util.Http
open Util.HttpServer
open Util.WebSocket
open Util.Concurrent

open UtilWebServer.Common
open UtilWebServer.Api

let outputHex output caption hexData = 
    caption |> output

    hexData
    |> hex
    |> output

let apiMonitor runtime = 
    let listener = runtime.listener
    [|  ok
        ("connId", listener.connId.Value.ToString() |> Json.Str)
        ("queue", listener.queue.count.ToString() |> Json.Str)
        ("keeps", listener.keeps.count.ToString() |> Json.Str) |]

