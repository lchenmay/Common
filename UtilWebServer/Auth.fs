module UtilWebServer.Auth

open System
open System.Text
open System.IO
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent

open Util.Cat
open Util.ADT
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

open UtilWebServer.Db
open UtilWebServer.Common
open UtilWebServer.Open
open UtilWebServer.Api

let keepSession = 7.0

let createSession 
    (creator: string -> SessionTemplate<'User,'Data>)
    (sessions: SessionsTemplate<'User,'Data>) = 

    use cw = new CodeWrapper("UtilWebServer.Auth.createSessionKey")

    let key = 
        Guid.NewGuid().ToByteArray()
        |> bin__sha256

    let session = creator key

    sessions[key] <- session

let checkSession 
    erUnauthorized 
    (sessions: SessionsTemplate<'User,'Data>)
    x = 

    x.sessiono <- 
        let s = tryFindStrByAtt "session" x.json
        if s.Length = 0 then
            None
        else
            if sessions.ContainsKey s then
                let session = sessions[s]
                if DateTime.UtcNow.Ticks >= session.expiry.Ticks then
                    sessions.Remove s
                    |> ignore

                    None
                else
                    Some session
            else
                None

    match x.sessiono with
    | Some session -> Suc x
    | None -> 
        x.ero <- Some erUnauthorized
        Fail(erUnauthorized,x)

type AuthParams<'p,'complex> = {
getSocialAuthBiz: 'p -> int64
setSocialAuthBiz: 'p -> int64 -> unit
getSocialAuthId: 'p -> string
setSocialAuthId: 'p -> string -> unit
metadata: MetadataTypes<'p>
p__complex: Rcd<'p> -> 'complex
complex__ids: 'complex -> int64 * string
conn: string }

let tryFindExisting 
    ap
    (ecs: ConcurrentDictionary<int64,'complex>)
    bizId id = 
    ecs.Values
    |> Seq.tryFind(fun ec -> 
        let a,b = ec |> ap.complex__ids
        a = bizId && b = id)

let tryCreateUser 
    ap 
    (ecs: ConcurrentDictionary<int64,'complex>)
    bizId id = 

    let pretx = None |> opctx__pretx
    
    let rcd = 

        let p = ap.metadata.empty__p()

        ap.setSocialAuthBiz p bizId
        ap.setSocialAuthId p id

        p
        |> populateCreateTx pretx ap.metadata

    if  pretx 
        |> loggedPipeline 
            "UtilWebServer.Auth.tryCreateUser" ap.conn then
        Some rcd
    else
        None
    |> optionProcessSome
        (fun rcd -> 
            let ec = rcd |> ap.p__complex
            ecs[rcd.ID] <- ec
            ec)


let socialAuth 
    (erInternal,erInvalideParameter) 
    discord
    checkoutEu
    v__json
    x =

    let json = x.json

    match tryFindStrByAtt "biz" json with
    | "DISCORD" ->
        match
            Discord.requestAccessToken
                discord
                (tryFindStrByAtt "redirectUrl" json)
                (tryFindStrByAtt "code" json)
            |> Discord.requestUserInfo with
        | Some (uid,usernameWithdiscriminator, avatar, json) -> 

            match 
                uid.ToString()
                |> checkoutEu "DISCORD" with
            | Some v -> 
                [|  ok
                    ("ec", v |> v__json)   |]

            | None -> er erInternal

        | None -> er erInvalideParameter

    | _ -> er erInvalideParameter
