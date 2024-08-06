module JWebServer.Monitor

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

open JWebServer.Common

let engine__monitor engine = 
    [|  ("connId", engine.connId.ToString() |> Json.Str)
        ("queue", engine.queue.count.ToString() |> Json.Str)
        ("keeps", engine.keeps.count.ToString() |> Json.Str) |]
    |> Json.Braket
