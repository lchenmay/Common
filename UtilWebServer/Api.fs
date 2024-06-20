module UtilWebServer.Api

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Runtime
open Util.Text
open Util.Json
open Util.Db
open Util.DbTx
open Util.Orm
open Util.Http
open Util.HttpServer
open Util.Zmq

open UtilWebServer.DbLogger

let ok = "Er",Json.Str "OK"
let er er = "Er",er.ToString() |> Json.Str
let wrapOk name json = 
    [|  ok
        (name,json) |]

let rep404 = [| |]

let echoApiHandler branch req =

    let service = req.path[1]
    let api = req.path[2]

    let json = str__root req.body

    branch service api json
    |> Json.Braket
    |> json__strFinal
    |> str__StandardResponse "application/json"
