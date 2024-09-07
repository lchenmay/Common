module UtilWebServer.Api

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Cat
open Util.Perf
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

    let req = str__root req.body

    let rep = 
        req
        |> branch service api 
        |> Json.Braket

    rep
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

let runApi branching x =
    match
        x
        |> bind branching with
    | Suc x -> 
        use cw = new CodeWrapper("branch.exe")

        match x.proco with
        | Some p ->
            use cw = new CodeWrapper("Api." + x.api)
            p x
        | None -> [| ok |]
    | Fail(e,x) -> er e

let apiCreate bin__p x = 
    let fields = x.json |> json__items
    let o = tryDeserialize bin__p "p" fields
    fields,o

let apiUpdate bin__rcd x = 
    x.json 
    |> json__items
    |> tryDeserialize bin__rcd "rcd"

let apiList item__json = 
    Seq.toArray >> Array.map item__json >> wrapOkAry


let apiMonitorPerf() = 

    stats None
    |> Array.map(fun i -> 
        let k,v,elapse,max,mil,histogram = i

        let total = v.sum |> TimeSpan.FromTicks
        let mean = 
            (float v.sum) / (float v.count) 
            |> int64 
            |> TimeSpan.FromTicks
        let rps =
            let measurespan = (v.last - v.start) |> TimeSpan.FromTicks
            (float v.count) / measurespan.TotalSeconds

        [|  ("key", k |> Json.Str)
            ("count", v.count.ToString() |> Json.Num)
            ("totalMil", total.TotalMilliseconds.ToString("0.000") |> Json.Num)
            ("avgMil", mean.TotalMilliseconds.ToString("0.000") |> Json.Num)
            ("rps", rps.ToString("0.000") |> Json.Num ) |] 
        |> Json.Braket)
    |> wrapOkAry
