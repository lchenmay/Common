module UtilKestrel.Api

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

open UtilKestrel.Types
open UtilKestrel.Ctx
open UtilKestrel.Json
open UtilKestrel.Db
open UtilKestrel.DbLogger
open UtilKestrel.Paging

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
    (x:EchoCtx<'Runtime,'Session,'Error>) = 
    match 
        tryFindNumByAtt "id" x.Json
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
    (x:EchoCtx<'Runtime,'Session,'Error>) =

    let json = x.Json

    let pretx = None |> opctx__pretx

    match 
        tryFindNumByAtt "id" json
        |> parse_int64
        |> tryLoader with
    | Some v -> 
        let rcd = v__rcd v
        let act = tryFindStrByAtt "act" json
        if act = "" then
            match tryFindByAtt "p" json with
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
        match tryFindByAtt "p" json with
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
    (x:EchoCtx<'Runtime,'Session,'Error>) =

    let json = x.Json

    match 
        tryFindNumByAtt "id" json
        |> parse_int64
        |> tryLoader with
    | Some v -> 
        let rcd = v__rcd v
        let act = tryFindStrByAtt "act" json
        if act = "" then
            match tryFindByAtt "p" json with
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
        match tryFindByAtt "p" json with
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

let runApi branching 
    (x:EchoCtx<'Runtime,'Session,'Error>) =

    match
        x
        |> Suc
        |> bind branching with
    | Suc s -> 
        use cw = new CodeWrapper("branch.exe")

        match x.Struct.proco with
        | Some p ->
            use cw = new CodeWrapper("Api." + x.Struct.api)
            p x
        | None -> [| ok |]
    | Fail(e,x) -> er e

let apiCreate bin__p json = 
    let fields = json |> json__items
    let o = tryDeserialize bin__p "p" fields
    fields,o

let apiUpdate bin__rcd = 
    json__items >> tryDeserialize bin__rcd "rcd"

let apiList item__json = 
    Seq.toArray >> Array.map item__json >> wrapOkAry

let apiPing x =
    [|  ok
        ("timestamp",Json.Num (DateTime.UtcNow.Ticks.ToString()))   |]


type InternalEr = 
| Ok of (string * Json)[]
| InvalideParameter
| Internal

let saveApi 
    output
    rcd__existsg
    onSucCreateo
    onSucUpdateeo
    metadata 
    json = 
    
    json 
    |> tryFindByAtt "rcd"
    |> processOptionWithNone 
        (fun (s,json) -> 
            match
                json
                |> metadata.json__rcdo with
            | Some rcd ->

                if rcd__existsg rcd then
                    match 
                        rcd
                        |> metadata.rcd_update output with
                    | Some rcd -> 
                        match onSucUpdateeo with
                        | Some h -> h rcd
                        | None -> ()

                        [| ("data",metadata.rcd__json rcd) |]
                        |> InternalEr.Ok
                    | None -> InternalEr.Internal
                else
                    match 
                        rcd.p
                        |> metadata.p_create output with
                    | Some rcd -> 
                        match onSucCreateo with
                        | Some h -> h rcd
                        | None -> ()

                        [| ("data",metadata.rcd__json rcd) |]
                        |> InternalEr.Ok
                    | None -> InternalEr.Internal
            | None -> InternalEr.InvalideParameter)
        (fun _ -> InternalEr.InvalideParameter)

type ApiDbCtx<'Data,'p> = {
marshall:MarshallTypes<'Data>
metadata:MetadataTypes<'p>
__items: unit -> 'Data[]
listFilter: 'Data -> bool
searching: string -> 'Data -> bool
rcd__existing: Rcd<'p> -> bool
onSucCreateo: (Rcd<'p> -> unit) option
onSucUpdateo: (Rcd<'p> -> unit) option
continueo: (string -> Json -> InternalEr) option }

let empty__ApiDbCtx
    (marshall,metadata)
    __items
    listFilter
    searching
    rcd_existing
    onSucCreateo
    onSucUpdateo
    continueo = {
        marshall = marshall
        metadata = metadata
        __items = __items
        listFilter = listFilter
        searching = searching
        rcd__existing = rcd_existing
        onSucCreateo = onSucCreateo
        onSucUpdateo = onSucUpdateo
        continueo = continueo }

let apiBuilder
    output 
    (adx:ApiDbCtx<'Data,'p>)
    json: InternalEr = 

    let act = json |> tryFindStrByAtt "act"

    match act with
    | "ls-all" ->  
        adx.__items()
        |> paging adx.marshall.data__json json 
        |> InternalEr.Ok

    | "ls" ->  
        adx.__items()
        |> Array.filter adx.listFilter
        |> paging adx.marshall.data__json json 
        |> InternalEr.Ok

    | "search" ->
        let term = (json |> tryFindStrByAtt "term").ToLower()
        let filter = adx.searching term

        let data = 
            adx.__items()
            |> Array.filter filter
            |> Array.map adx.marshall.data__json
            |> Json.Ary

        [| ("data",data) |]
        |> InternalEr.Ok

    | "save" -> 
        saveApi 
            output 
            adx.rcd__existing
            adx.onSucCreateo
            adx.onSucUpdateo
            adx.metadata
            json
    | _ -> 
        match adx.continueo with
        | Some switcher -> switcher act json
        | None -> InternalEr.InvalideParameter


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

        k,v,total,mean,rps)
    |> Array.sortByDescending(fun (k,v,total,mean,rps) -> total)
    |> Array.map(fun (k,v,total,mean,rps) ->
        [|  ("key", k |> Json.Str)
            ("count", v.count.ToString() |> Json.Num)
            ("totalMil", total.TotalMilliseconds.ToString("0.000") |> Json.Num)
            ("avgMil", mean.TotalMilliseconds.ToString("0.000") |> Json.Num)
            ("rps", rps.ToString("0.000") |> Json.Num ) |] 
        |> Json.Braket)
    |> wrapOkAry
