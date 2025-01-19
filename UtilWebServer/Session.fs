module UtilWebServer.Session

open System
open System.Text
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent

open Util.Cat
open Util.ADT
open Util.Perf
open Util.CollectionModDict
open Util.Crypto
open Util.DbQuery
open Util.DbTx
open Util.Text
open Util.Json
open Util.Http
open Util.Orm
open Util.Db
open Util.Zmq

open UtilWebServer.Db
open UtilWebServer.Common
open UtilWebServer.Open
open UtilWebServer.Api

let keepSession = 7.0

let user__session
    (sessions: ModDictStr<SessionTemplate<'User,'SessionData>>)
    user = 

    let key = 
        Guid.NewGuid().ToByteArray()
        |> bin__sha256

    let session = { 
        since = DateTime.UtcNow
        expiry = DateTime.MaxValue
        identity = Some user
        datao = None
        session = key }

    sessions[key] <- session

    session

let validateRuntimeSession 
    (sessions: ModDictStr<SessionTemplate<'User,'SessionData>>)
    x = 

    x.sessiono <- None

    let s = tryFindStrByAtt "session" x.json
    if s.Length > 0 && sessions.ContainsKey s then
        let session = sessions[s]
        if DateTime.UtcNow.Ticks >= session.expiry.Ticks then
            sessions.Remove s |> ignore
        else
            x.sessiono <- Some session

let checkRuntimeSession 
    erUnauthorized 
    (sessions: ModDictStr<SessionTemplate<'User,'SessionData>>)
    x = 

    validateRuntimeSession sessions x

    match x.sessiono with
    | Some session -> Suc x
    | None -> 
        x.ero <- Some erUnauthorized
        Fail(erUnauthorized,x)

let checkSessionUsero 
    erUnauthorized 
    (sessions: ModDictStr<SessionTemplate<'User,'SessionData>>)
    x = 

    match
        checkRuntimeSession
            erUnauthorized
            sessions
            x with
    | Suc x -> 
        let session = x.sessiono.Value
        Some session,session.identity
    | _ -> None,None
