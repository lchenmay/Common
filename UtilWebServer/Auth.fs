module UtilWebServer.Auth

open System
open System.Text
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent

open Util.Cat
open Util.Perf
open Util.Crypto
open Util.DbQuery
open Util.DbTx
open Util.Text
open Util.Json
open Util.Http
open Util.Orm
open Util.Db
open Util.Zmq

type Session<'SessionRole,'Data> = { 
since: DateTime
mutable expiry: DateTime
mutable identity: 'SessionRole
mutable data: 'Data
session: string }

let keepSession = 7.0

let createSession 
    (creator: string -> Session<'SessionRole,'Data>)
    (sessions: ConcurrentDictionary<string,Session<'SessionRole,'Data>>) = 

    use cw = new CodeWrapper("UtilWebServer.Auth.createSessionKey")

    let key = 
        Guid.NewGuid().ToByteArray()
        |> bin__sha256

    let session = creator key

    sessions[key] <- session

let checkExpire
    (sessions: ConcurrentDictionary<string,Session<'SessionRole,'Data>>)
    key = 

    if sessions.ContainsKey key then
        let s = sessions[key]
        if DateTime.UtcNow.Ticks >= s.expiry.Ticks then
            sessions.Remove key
            |> ignore

