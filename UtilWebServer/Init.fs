module UtilWebServer.Init

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Runtime
open Util.Db
open Util.DbTx
open Util.Orm
open Util.Zmq

open UtilWebServer.DbLogger

let loadAll output conn metadata h = 
    match 
        ""
        |> Util.Orm.loadall conn
            (metadata.table,metadata.fieldorders,metadata.db__rcd) with
    | Some items ->
        items
        |> Array.iter h
    | None -> 
        halt output ("BizLogics.Init.loadAll [" + metadata.table + "]") ""
