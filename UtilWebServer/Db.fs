module UtilWebServer.Db

open System
open System.Text
open System.Collections.Generic
open System.Threading

open Util.Cat
open Util.Text
open Util.Db
open Util.DbQuery
open Util.DbTx
open Util.Orm
open Util.Zmq

open UtilWebServer.DbLogger

let loggedPipeline (loc:string) conn pretx = 
    if pretx.sqls.Count > 0 then
        match 
            pretx
            |> pipeline conn with
        | Suc ctx -> true
        | Fail(dte,ctx) -> 
            match dbLoggero with
            | Some logger -> logger (loc,dte,ctx)
            | None -> ()
            false
    else
        true

let populateCreateTx 
    pretx 
    metadata 
    p = 

    let tid = Interlocked.Increment metadata.id

    let rcd = 
        ((tid,pretx.dt,pretx.dt,tid),p)
        |> metadata.wrapper

    (tid,pretx.dt,pretx.dt,tid,p)
    |> build_create_sql metadata
    |> pretx.sqls.Add

    rcd