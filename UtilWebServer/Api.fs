﻿module UtilWebServer.Api

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Cat
open Util.Runtime
open Util.Text
open Util.Json
open Util.Db
open Util.DbTx
open Util.Orm
open Util.Http
open Util.HttpServer
open Util.Zmq

open UtilWebServer.Json

open UtilWebServer.DbLogger

type ApiReturn = (string * Json)[]

type ApiCtx<'Runtime,'Session, 'Error> = { 
since: DateTime
service: string
api: string
ip: string
json: Json
mutable proco: (ApiCtx<'Runtime,'Session, 'Error> -> ApiReturn) option
mutable sessiono: 'Session option  
mutable ero: 'Error option
runtime: 'Runtime    }

let incoming__x runtime service api ip json = 
    {
        since = DateTime.UtcNow
        service = service
        api = api
        ip = ip
        json = json
        proco = None
        sessiono = None
        ero = None
        runtime = runtime }
    |> Suc


let ok = "Er",Json.Str "OK"
let er er = [|  "Er",(er.ToString() |> Json.Str) |]
let wrapOk name json = 
    [|  ok
        (name,json) |]

let wrapOkAry = Json.Ary >> wrapOk "list"

let rep404 = [| |]

let echoApiHandler branch req =

    let service = req.path[1]
    let api = req.path[2]

    let json = str__root req.body

    branch service api json
    |> Json.Braket
    |> json__strFinal
    |> str__StandardResponse "application/json"


let tryLoadFromJsonIdWrapOK 
    v__json
    (n,e) 
    tryLoader 
    x = 
    match 
        tryLoader
        |> tryLoadFromJsonId x.json "id" with
    | Some v -> 
        v
        |> v__json
        |> wrapOk n
    | None -> er e
