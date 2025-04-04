﻿module UtilWebServer.Api

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Cat
open Util.ADT
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

open UtilWebServer.Common
open UtilWebServer.Json
open UtilWebServer.Db
open UtilWebServer.DbLogger

type ApiReturn = (string * Json)[]

type ApiCtx<'Runtime,'Session,'Error> = { 
req: HttpRequest
since: DateTime
service: string
api: string
ip: string
json: Json
mutable proco: (ApiCtx<'Runtime,'Session, 'Error> -> ApiReturn) option
mutable sessiono: 'Session option  
mutable ero: 'Error option
runtime: 'Runtime    }

let incoming__x 
    runtime req
    service api ip json = 
    {
        req = req
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

    let sReq = req.body |> System.Text.Encoding.UTF8.GetString 
    let jsonReq = sReq |> str__root 

    let jsonRep = 
        jsonReq
        |> branch service api 
        |> Json.Braket

    let sRep = jsonRep |> json__strFinal
    sRep
    |> str__StandardResponse "application/json"

let tryLoadFromJsonIdWrapOK 
    v__json
    (n,e) 
    tryLoader 
    x = 
    match 
        tryFindNumByAtt "id" x.json
        |> parse_int64
        |> tryLoader with
    | Some v -> 
        v
        |> v__json
        |> wrapOk n
    | None -> er e

let createUpdateDeleteActTx
    loc conn metadata dbLoggero e tryLoader v__rcd json__p 
    (p__UpdateTx, onUpdateSuc)
    (p__ActTx, onActSuc)
    (p__CreateTx, onCreateSuc)
    x =

    let pretx = None |> opctx__pretx

    match 
        tryFindNumByAtt "id" x.json
        |> parse_int64
        |> tryLoader with
    | Some v -> 
        let rcd = v__rcd v
        let act = tryFindStrByAtt "act" x.json
        if act = "" then
            match tryFindByAtt "p" x.json with
            | Some(n,json) ->
                match json |> json__p with
                | Some pIncoming -> 
                    let p,tag,txs = p__UpdateTx pretx v rcd pIncoming
                    txs |> pretx.sqls.AddRange
                    if pretx |> loggedPipeline dbLoggero loc conn then
                        rcd.p <- p
                        onUpdateSuc v rcd tag
                    else
                        er e
                | None -> er e
            | None -> er e
        else
            p__ActTx act v rcd
    | None ->
        match tryFindByAtt "p" x.json with
        | Some(n,json) ->
            match json |> json__p with
            | Some pIncoming -> 
                let rcd,tag,txs = p__CreateTx pretx pIncoming 
                txs |> pretx.sqls.AddRange
                if pretx |> loggedPipeline dbLoggero loc conn then
                    onCreateSuc rcd tag
                else
                    er e
            | None -> er e
        | None -> er e

let createUpdateDeleteAct
    loc conn metadata dbLoggero e tryLoader hacto v__rcd json__p 
    pModifier postRemoveo preCreateo postCreateo
    x =
    match 
        tryFindNumByAtt "id" x.json
        |> parse_int64
        |> tryLoader with
    | Some v -> 
        let rcd = v__rcd v
        let act = tryFindStrByAtt "act" x.json
        if act = "" then
            match tryFindByAtt "p" x.json with
            | Some(n,json) ->
                match json |> json__p with
                | Some pIncoming -> 
                    if updateRcd loc conn metadata dbLoggero
                        (fun p -> pModifier p pIncoming) rcd then
                        [|  ok
                            ("rcd",rcd |> metadata.rcd__json) |]
                    else
                        er e
                | None -> er e
            | None -> er e
        else if act = "remove" then
            if  "WHERE ID=" + rcd.ID.ToString() |> delete loc conn metadata dbLoggero then
                handlero postRemoveo v
                [|  ok |]
            else
                er e
        else 
            match hacto with
            | Some h -> h act v
            | None -> er e
    | None ->
        match tryFindByAtt "p" x.json with
        | Some(n,json) ->
            match json |> json__p with
            | Some pIncoming -> 
                let p = 
                    match preCreateo with
                    | Some h -> 
                        let p = metadata.empty__p()
                        h p pIncoming
                        p
                    | None -> pIncoming
                match p__createRcd p metadata dbLoggero loc conn with
                | Some rcd -> 
                    handlero postCreateo rcd
                    [|  ok
                        ("rcd",rcd |> metadata.rcd__json) |]
                | None -> er e
            | None -> er e
        | None -> er e


let nullParam = 
    [|  ("Er",Json.Str "") |]

let runApi branching x =

    match
        x
        |> bind branching with
    | Suc x -> 
        use cw = new CodeWrapper("branch.exe")

        match x.json with
        | Json.Null -> nullParam
        | _ -> 
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

let apiPing x =
    [|  ok
        ("timestamp",Json.Num (DateTime.UtcNow.Ticks.ToString()))   |]

let apiMonitorPerf x = 

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
