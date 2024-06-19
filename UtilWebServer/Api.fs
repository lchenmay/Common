module UtilWebServer.Api

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

let apiHandler branch json api = 

    let mutable ero = ref None

    branch api json ero
    |> Array.map(fun (k,v) -> k,(Json.Str v))
    |> Json.Braket
