module UtilKestrel.Paging

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

type Paging = {
mutable npp: Int32
mutable page: Int32
mutable total: Int32
mutable pages: Int32 }

let Paging_empty() = {
    npp = 30
    page = 0
    total = 0
    pages = 0 }

let Paging__json (v:Paging) =

    [|  ("npp",int32__json v.npp)
        ("page",int32__json v.page)
        ("total",int32__json v.total)
        ("pages",int32__json v.pages)
         |]
    |> Json.Braket

let Paging__jsonTbw (w:TextBlockWriter) (v:Paging) =
    json__str w (Paging__json v)

let Paging__jsonStr (v:Paging) =
    (Paging__json v) |> json__strFinal

/// Serialize a complete collection for explicit `ls-all` requests.
/// This path is intentionally separate from `paging`, whose per-page safety cap remains 200.
let allItems item__json (ary:'a[]) =
    let info = {
        npp = Math.Max(ary.Length, 1)
        page = 0
        total = ary.Length
        pages = if ary.Length = 0 then 0 else 1 }

    [|  ("data", ary |> Array.map item__json |> Json.Ary)
        ("paging", info |> Paging__json) |]


let json__Pagingo (json:Json):Paging option =
    let fields = json |> json__items

    let mutable passOptions = true

    let nppo =
        match json__tryFindByName json "npp" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__int32o with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let pageo =
        match json__tryFindByName json "page" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__int32o with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let totalo =
        match json__tryFindByName json "total" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__int32o with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    let pageso =
        match json__tryFindByName json "pages" with
        | None ->
            passOptions <- false
            None
        | Some v -> 
            match v |> json__int32o with
            | Some res -> Some res
            | None ->
                passOptions <- false
                None

    if passOptions then
        ({
            npp = nppo.Value
            page = pageo.Value
            total = totalo.Value
            pages = pageso.Value }:Paging) |> Some
    else
        None

let paging item__json json (ary:'a[]) = 

    let paging =
        match json |> tryFindByAtt "paging" with
        | Some (s,v) -> 
            match json__Pagingo v with
            | Some vv -> vv
            | None -> Paging_empty()
        | None -> Paging_empty()

    if paging.npp <= 0 then
        paging.npp <- 30
    elif paging.npp > 200 then
        paging.npp <- 200

    paging.total <- ary.Length
       
    paging.pages <- float(paging.total) / float(paging.npp) |> Math.Ceiling |> int
    if paging.page < 0 then
        paging.page <- 0
    elif paging.pages = 0 then
        paging.page <- 0
    elif paging.page >= paging.pages then
        paging.page <- paging.pages - 1

    let json = 
        let index = paging.page * paging.npp
        let count = Math.Min(paging.npp, ary.Length - index)
        if count <= 0 then [||]
        else Array.sub ary index count
        |> Array.map item__json
        |> Json.Ary

    [|  ("data",json)
        ("paging",paging |> Paging__json) |]

