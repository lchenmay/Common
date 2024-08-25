module UtilWebServer.Init

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Cat
open Util.Perf
open Util.Runtime
open Util.Db
open Util.DbQuery
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

let loadAllBulk<'p> output conn metadata = 

    use cw = new CodeWrapper("BizLogics.Social.loadAllBulk")

    "Loading bulk [" + metadata.table + "] ..."
    |> output

    let mutable count = 0

    let res = List<Rcd<'p>>()

    let h (line,x) =  
        count <- count + 1
        if count % 100000 = 0 then
            count.ToString()
            |> output
        line |> metadata.db__rcd |> res.Add
        true

    match
        [|  "SELECT " + metadata.fieldorders
            " FROM " + metadata.table
            " order by ID asc" |]
        |> String.Concat
        |> str__sql
        |> multiline_handle(conn,h) with
    | Suc x -> ()
    | Fail(exn,ctx) -> 
        Util.Runtime.halt output ("UtilWebServer.Init.loadAllBulk [" + metadata.table + "]") ""

    res.ToArray()


let updateDbStructure runtime conn = 

    "Updating database structure ... " |> runtime.output
    match

        let file = 
            match runtime.host.database with
            | Rdbms.SqlServer -> "/sqlSQLServer.sql"
            | Rdbms.PostgreSql -> "/sqlPostgreSQL.sql"

        Environment.CurrentDirectory() + file
        |> File.ReadAllText
        |> txOne conn runtime.output with
    | Suc x -> 
        "Done" |> runtime.output
    | Fail(e,x) -> 
        "Failed" |> runtime.output
