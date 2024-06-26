module UtilWebServer.Init

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Cat
open Util.Runtime
open Util.Db
open Util.DbTx
open Util.Orm
open Util.Zmq

open UtilWebServer.DbLogger
open UtilWebServer.Common

let loadAll output conn metadata h = 

    "Loading all records from [" + metadata.table + "]" 
    |> output

    match 
        ""
        |> Util.Orm.loadall conn
            (metadata.table,metadata.fieldorders,metadata.db__rcd) with
    | Some items ->
        items
        |> Array.iter h
    | None -> 
        halt output ("BizLogics.Init.loadAll [" + metadata.table + "]") ""

let updateDbStructure runtime conn = 

    "Updating database structure ... " |> runtime.output
    match
        Environment.CurrentDirectory + "/OrmTypes.sql"
        |> File.ReadAllText
        |> txOne conn runtime.output with
    | Suc x -> 
        "Done" |> runtime.output
    | Fail(e,x) -> 
        "Failed" |> runtime.output
