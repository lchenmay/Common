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
open UtilWebServer.Open
open UtilWebServer.Api

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


type AuthParams<'p,'complex> = {
getSocialAuthBiz: 'p -> int64
setSocialAuthBiz: 'p -> int64 -> unit
getSocialAuthId: 'p -> string
setSocialAuthId: 'p -> string -> unit
empty__p: unit -> 'p 
metadata: MetadataTypes<'p>
p__complex: Rcd<'p> -> 'complex
complex__ids: 'complex -> int64 * string
loc: string
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

        let p = ap.empty__p()

        ap.setSocialAuthBiz p bizId
        ap.setSocialAuthId p id

        p
        |> populateCreateTx pretx ap.metadata

    if pretx |> loggedPipeline ap.loc ap.conn then
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
    json =

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
