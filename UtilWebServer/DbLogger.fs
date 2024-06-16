module UtilWebServer.DbLogger

open LanguagePrimitives

open System
open System.Collections.Generic
open System.Data.SqlClient
open System.Text

open Util.Text

open Util.Orm

open Util.Cat
open Util.Db
open Util.DbQuery
open Util.DbTx

let empty__DbLog (loc,sql,exn) = {
    location = loc |> checkEscape
    content = exn.ToString() |> checkEscape
    sql = sql |> checkEscape }

// Set during init, provided a DB log table or a file logger
let mutable dbLoggero:(string * DbTxError * Ctx -> unit) option = None

let dbLogger = fun (loc,dte,ctx) -> 

    let exns =
        if(dte.exno.IsSome) then
            dte.exno.Value.ToString()
        else
            ""
    let sqls =
        if(dte.sqlo.IsSome) then
            dte.sqlo.Value |> sql__string
        else
            ""

    ()


