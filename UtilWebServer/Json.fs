module UtilWebServer.Json

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Runtime
open Util.Text
open Util.Json
open Util.Db
open Util.DbTx
open Util.Orm
open Util.Zmq

open UtilWebServer.DbLogger

let tryLoadFromJsonId json name tryLoader = 
    let idStr = tryFindStrByAtt name json
    if idStr.Length > 0 then
        match try_parse_int64 idStr with
        | Some id ->
            tryLoader id
        | None -> None
    else
        None
