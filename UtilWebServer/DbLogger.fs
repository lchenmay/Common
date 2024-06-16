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

let empty__DbLog (loc,dte) = {
    location = loc |> checkEscape
    content = 
        match dte.exno with
        | Some exn -> exn.ToString() |> checkEscape
        | None -> ""
    sql = 
        match dte.sqlo with
        | Some sql -> sql.text |> checkEscape
        | None -> "" }

// Set during init, provided a DB log table or a file logger
let mutable dbLoggero:(DbLog -> unit) option = None

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


