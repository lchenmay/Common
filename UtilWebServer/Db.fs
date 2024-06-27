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
            | Some logger -> 
                (loc,dte) 
                |> empty__DbLog 
                |> logger
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

let create param__p param metadata loc conn = 

    let pretx = None |> opctx__pretx

    let rcd = 
        param
        |> param__p
        |> populateCreateTx pretx metadata

    if pretx |> loggedPipeline loc conn then
        Some rcd
    else
        None

let p__createRcd p = create (fun _ -> p) ()
