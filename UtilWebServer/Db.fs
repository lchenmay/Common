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

let populateUpdateTx
    pretx 
    metadata 
    rcd = 
    (rcd.ID,pretx.dt,rcd.p)
    |> build_update_sql metadata
    |> pretx.sqls.Add

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

let update loc conn metadata (id,p) = 

    let pretx = None |> opctx__pretx

    (id,DateTime.UtcNow,p)
    |> build_update_sql metadata
    |> pretx.sqls.Add
        
    pretx |> loggedPipeline loc conn

let updateRcd loc conn metadata changer rcd = 
    
    let current = metadata.clone rcd.p
    changer rcd.p

    if update loc conn metadata (rcd.ID,rcd.p) then
        true
    else
        rcd.p <- current
        false

