module UtilWebServer.Cache

open System
open System.Text
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Cat
open Util.Runtime
open Util.Text
open Util.Json
open Util.Db
open Util.DbTx
open Util.Orm
open Util.Http
open Util.HttpServer
open Util.Zmq

open UtilWebServer.DbLogger
open UtilWebServer.Common

type CachedWithExpiry = {
mutable timestamp: DateTime
mutable json: Json }

let empty__CachedWithExpiry() = {
    timestamp = DateTime.MinValue
    json = Json.Null }

let checkCache (loader: unit -> 'a[]) item__json n cwe =

    if (DateTime.UtcNow.Subtract cwe.timestamp).TotalMinutes > 1.0 then
        cwe.json <-
            let data = loader()
            if data.Length > n then
                Array.sub data 0 n
            else
                data
            |> Array.map item__json
            |> Json.Ary
        cwe.timestamp <- DateTime.UtcNow
