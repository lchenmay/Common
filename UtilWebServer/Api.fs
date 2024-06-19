﻿module UtilWebServer.Api

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
open Util.Zmq

open UtilWebServer.DbLogger

let ok = "Er",Json.Str "OK"
let er er = "Er",er.ToString() |> Json.Str
let wrapOk name json = 
    [|  ok
        (name,json) |]

let apiHandler branch json =
    json
    |> tryFindStrByAtt "api"
    |> branch json
    |> Json.Braket
