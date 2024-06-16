module UtilWebServer.Db

open System
open System.Text
open System.Collections.Generic

open Util.Text
open Util.Db
open Util.DbQuery
open Util.DbTx
open Util.Orm
open Util.Zmq

type TxInject<'b,'p> = {   
creatoro:(PreTx<'b option> -> Rcd<'p> -> unit) option
updatero:(PreTx<'b option> -> Rcd<'p> -> unit) option
removero:(PreTx<'b option> -> Rcd<'p> -> unit) option
state:(Rcd<'p> -> int32*string*int64*int32) option }

//let rcdUpdateTx
//    pretx
//    (x:X,loc,rcd__json,jsono:(TextBlockWriter -> Rcd<'p> -> unit) option,metadata)
//    (idTryload:int64 -> Rcd<'p> option,
//     onSuccess:(Rcd<'p> -> unit)option,
//     validateCodeo,
//     pAssign,
//     txInjecto:TxInject<'b,'p> option) =

//    let mutable error = Error.OK

//    let rcdo,p,oldStatuso =
//        if x.fields.ContainsKey("id") && (checkfield(x.fields)("id"))<>"" && (checkfield(x.fields)("id"))<>"0" then
//            match
//                "id"
//                |> checkfield(x.fields)
//                |> parse_int64
//                |> idTryload with
//            | Some(rcd) ->
//                let oso =
//                    match txInjecto with
//                    | Some txi ->
//                        match txi.state with
//                        | Some state -> state rcd |> Some
//                        | None -> None
//                    | None -> None

//                Some(rcd), rcd.p, oso
//            | None ->
//                error <- Error.InvalidParameters
//                None,metadata.empty__p(), None
//        else None,metadata.empty__p(), None
            
//    match validateCodeo with
//    | Some(field,length,getter,p__code) ->

//        let code = 
//            if(length > 0) then
//                checkfieldz x.fields field length
//            else
//                checkfield x.fields field

//        let id = checkfield x.fields "id" |> parse_int64

//        match 
//            getter(code) with
//        | Some(id,rcd) -> 
//            if(id <> rcd.ID && equalIgnoreCase(p__code(rcd.p),code)) then
//                error <- Error.CodeExists
//        | None -> ()
//    | None -> ()

//    match error with
//    | Error.OK ->
//        let transits =
//            if(checkfield(x.fields)("act") <> "remove") then
//                pAssign(x.fields,p)
//                let id = checkfield x.fields "id" |> parse_int64
//                if id > 0L then
//                    match oldStatuso with
//                    | Some (oldState,_,fk,tp) ->
//                        let newState,stateName,fk,tp = txInjecto.Value.state.Value rcdo.Value
//                        let eco = apictx__eco x

//                        getTransit x metadata.table stateName oldState newState
//                        |> Seq.toArray
//                        |> Seq.iter(
//                            fun t ->
//                                let ledger =
//                                    if x.runtime.authCenter.wcs.ContainsKey t.p.Workflow then
//                                        let wc1 = x.runtime.authCenter.wcs.[t.p.Workflow]
//                                        wc1.workflow.p.Ledger
//                                    else 0L

//                                Oa.LogWorkflowForward pretx t.p.Workflow t.ID eco.Value.eu ledger id (sprintf "transit#%d, may trigger other transits on fk:%d" t.ID fk) t.ID

//                                triggerTransitLinks x pretx t fk tp
//                        )
//                    | _ -> ()
            
//        let rcd =
//            match rcdo with
//            | Some(rcd) -> 

//                if(checkfield(x.fields)("act") = "remove") then

//                    ("DELETE FROM [" + metadata.table + "] WHERE ID=" + rcd.ID.ToString())                        
//                    |> str__sql
//                    |> pretx.sqls.Add

//                    match txInjecto with
//                    | Some txi ->
//                        match txi.removero with
//                        | Some remover -> remover pretx rcd
//                        | None -> ()
//                    | None -> ()

//                    (fun ctx -> 
//                        rcd.p <- p
//                        if(onSuccess.IsSome) then
//                            rcd |> onSuccess.Value)
//                    |> pretx.sucs.Add

//                else
//                    match txInjecto with
//                    | Some txi ->
//                        match txi.updatero with
//                        | Some updater -> updater pretx rcd
//                        | None -> ()
//                    | None -> ()
//                    (rcd.ID,pretx.dt,p)
//                    |> build_update_sql(metadata)
//                    |> pretx.sqls.Add

//                    (fun ctx -> 
//                        rcd.p <- p
//                        if(onSuccess.IsSome) then
//                            rcd |> onSuccess.Value)
//                    |> pretx.sucs.Add
//                rcd

//            | None -> 
                    
//                let tid = System.Threading.Interlocked.Increment(metadata.id)

//                let rcd = ((tid,pretx.dt,pretx.dt,tid),p) |> metadata.wrapper

//                match txInjecto with
//                | Some txi ->
//                    match txi.creatoro with
//                    | Some creator -> creator pretx rcd
//                    | None -> ()
//                | None -> ()

//                (tid,pretx.dt,pretx.dt,tid,p)
//                |> build_create_sql(metadata)
//                |> pretx.sqls.Add

//                if(onSuccess.IsSome) then
//                    (fun ctx -> rcd |> onSuccess.Value)
//                    |> pretx.sucs.Add
            
//                rcd
//        error, Some (rcd)
//    | _ -> error, None

//let rcdUpdate
//    (x,loc,rcd__json,jsono:(TextBlockWriter -> Rcd<'p> -> unit) option,metadata)
//    (idTryload:int64 -> Rcd<'p> option,
//     onSuccess:(Rcd<'p> -> unit)option,
//     validateCodeo,
//     pAssign,
//     txInjecto:TxInject<'b,'p> option) =

//    let pretx = None |> opctx__pretx
//    let error, rcdo =
//        rcdUpdateTx
//            pretx
//            (x,loc,rcd__json,jsono, metadata)
//            (idTryload,onSuccess,
//            validateCodeo,
//            pAssign,
//            txInjecto)

//    if error = Error.OK then
//        match
//            pretx 
//            |> logged_pipeline loc with
//        | Error.OK ->
//            if rcdo.IsSome then
//                rcdo.Value |> rcd__json x.w
//                x.w.backspace()
//                x.w.backspace()
//                ",\"Error\":\"OK\"}" |> x.w.newline
//                //Console.WriteLine "rcdUpdate new OK"
//                Error.OK
//            else
//                "{\"Error\":\"OK\"" |> x.w.newline
//                ",\"msg\":\"rcd is none\"}" |> x.w.newline
//                Error.OK
//        | _ -> Error.Internal

//    else error

