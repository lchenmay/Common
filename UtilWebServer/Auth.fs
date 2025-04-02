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
open Util.CollectionModDict
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
open UtilWebServer.Session

type AuthParams<'p,'complex> = {
getSocialAuthBiz: 'p -> int64
setSocialAuthBiz: 'p -> int64 -> unit
getSocialAuthId: 'p -> string
setSocialAuthId: 'p -> string -> unit
getSocialAuthCaption: 'p -> string
setSocialAuthCaption: 'p -> string -> unit
getSocialAuthAvatar: 'p -> string
setSocialAuthAvatar: 'p -> string -> unit
metadata: MetadataTypes<'p>
p__complex: Rcd<'p> -> 'complex
complex__ids: 'complex -> int64 * string
conn: string }

let tryFindExisting 
    ap
    (userxs: ModDictInt64<'complex>)
    bizId id = 
    userxs.Values
    |> Seq.tryFind(fun ec -> 
        let a,b = ec |> ap.complex__ids
        a = bizId && b = id)

let tryCreateUser 
    dbLoggero
    ap 
    (userxs: ModDictInt64<'complex>)
    bizId
    (id,caption,avatar) = 

    let pretx = None |> opctx__pretx
    
    let rcd = 

        let p = ap.metadata.empty__p()

        ap.setSocialAuthBiz p bizId
        ap.setSocialAuthId p id
        ap.setSocialAuthCaption p caption
        ap.setSocialAuthAvatar p avatar

        p
        |> populateCreateTx pretx ap.metadata

    if  pretx 
        |> loggedPipeline dbLoggero
            "UtilWebServer.Auth.tryCreateUser" ap.conn then
        Some rcd
    else
        None
    |> oPipelineSome
        (fun rcd -> 
            let ec = rcd |> ap.p__complex
            userxs[rcd.ID] <- ec
            ec)

let checkDiscordUser runtime checkoutEu (uid, username, avatar) = 
    match 
        (uid.ToString(),username,avatar)
        |> checkoutEu runtime "DISCORD" with
    | Some user -> 
        user 
        |> user__session runtime.sessions
        |> Some
    | None -> None


let socialAuth 
    (erInternal,erInvalideParameter,erUnauthorized) 
    discord
    runtime
    checkoutEu
    v__json
    x =

    match 
        checkSessionUsero 
            erUnauthorized 
            runtime.sessions
            x with
    | None,_ -> 

        let json = x.json
        let biz = tryFindStrByAtt "biz" json
        let code = tryFindStrByAtt "code" json
        let redirectUrl = tryFindStrByAtt "redirecturl" json

        match biz with
        | "DISCORD" ->
            let (token, js) = Discord.requestAccessToken discord redirectUrl code
            match
                token |> Discord.requestUserInfo with
            | Some (uid,usernameWithdiscriminator, avatar, json) -> 

                match 
                    (uid.ToString(),usernameWithdiscriminator,avatar)
                    |> checkoutEu "DISCORD" with
                | Some user -> 

                    let session = 
                        user 
                        |> user__session runtime.sessions

                    [|  ok
                        ("session", session.session |> Json.Str)
                        ("ec", user |> v__json)   |]

                | None -> er erInternal

            | None -> er erInvalideParameter

        | _ -> er erInvalideParameter

    | Some session,ido -> 
        [|  ok
            ("session", session.session |> Json.Str)  |]

