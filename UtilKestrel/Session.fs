module UtilKestrel.Session

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

open UtilKestrel.Types
open UtilKestrel.Ctx
open UtilKestrel.Common
open UtilKestrel.Open
open UtilKestrel.Api

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
    (x:EchoCtx<'Runtime,SessionTemplate<'User,'SessionData>,'Error>) = 

    x.Struct.sessiono <- None

    let s = tryFindStrByAtt "session" x.Json
    if s.Length > 0 && sessions.ContainsKey s then
        let session = sessions[s]
        if DateTime.UtcNow.Ticks >= session.expiry.Ticks then
            sessions.Remove s |> ignore
        else
            x.Struct.sessiono <- Some session

let checkRuntimeSession 
    erUnauthorized 
    (sessions: ModDictStr<SessionTemplate<'User,'SessionData>>)
    (x:EchoCtx<'Runtime,SessionTemplate<'User,'SessionData>,'Error>) = 

    validateRuntimeSession sessions x

    match x.Struct.sessiono with
    | Some session -> Suc x
    | None -> 
        x.Struct.ero <- Some erUnauthorized
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
        let session = x.Struct.sessiono.Value
        Some session,session.identity
    | _ -> None,None
