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

let loggedPipeline dbLoggero (loc:string) conn pretx = 
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

let p__create metadata pretx p = 
    let tid = Interlocked.Increment metadata.id
    let rcd = 
        ((tid,pretx.dt,pretx.dt,tid),p)
        |> metadata.wrapper
    let tx = 
        (tid,pretx.dt,pretx.dt,tid,p)
        |> build_create_sql metadata
    rcd,tx    

let id__CreateTx tid pretx metadata p = 

    let rcd = 
        ((tid,pretx.dt,pretx.dt,tid),p)
        |> metadata.wrapper

    (tid,pretx.dt,pretx.dt,tid,p)
    |> build_create_sql metadata
    |> pretx.sqls.Add

    rcd

let populateCreateTx pretx metadata = 
    id__CreateTx (Interlocked.Increment metadata.id) pretx metadata

let populateUpdateTx
    pretx 
    metadata 
    rcd = 
    (rcd.ID,pretx.dt,rcd.p)
    |> build_update_sql metadata
    |> pretx.sqls.Add

let create param__p param metadata dbLoggero loc conn = 

    let pretx = None |> opctx__pretx

    let rcd = 
        param
        |> param__p
        |> populateCreateTx pretx metadata

    if pretx |> loggedPipeline dbLoggero loc conn then
        Some rcd
    else
        None

let p__createRcd p = create (fun _ -> p) ()

let update loc conn metadata dbLoggero (id,p) = 

    let pretx = None |> opctx__pretx

    (id,DateTime.UtcNow,p)
    |> build_update_sql metadata
    |> pretx.sqls.Add
        
    pretx |> loggedPipeline dbLoggero loc conn

let updateRcd loc conn metadata dbLoggero changer rcd = 
    
    let current = metadata.clone rcd.p
    if changer rcd.p then
        if update loc conn metadata dbLoggero (rcd.ID,rcd.p) then
            true
        else
            rcd.p <- current
            false
    else
        false

let delete loc conn metadata dbLoggero where = 

    let pretx = None |> opctx__pretx

    {
        text = "DELETE FROM [" + metadata.table + "] " + where
        ps = [||] }
    |> pretx.sqls.Add
        
    pretx |> loggedPipeline dbLoggero loc conn

