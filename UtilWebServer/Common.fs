module UtilWebServer.Common

open System
open System.Collections.Concurrent
open System.Text
open System.IO
open System.Diagnostics

open Util.Cat
open Util.Http
open Util.Zmq

let since = DateTime.UtcNow

let output:string -> unit = 

    let assbly = System.Reflection.Assembly.GetCallingAssembly()
    let dir = Directory.GetCurrentDirectory()
    if dir.EndsWith "WebService" then
        Debug.WriteLine
    else if dir.EndsWith "WebApp" then
        fun s ->
            let elapse = DateTime.UtcNow.Subtract since
            [|
                elapse.Days.ToString("000") + "."
                elapse.Hours.ToString("00") + ":"
                elapse.Minutes.ToString("00") + ":"
                elapse.Seconds.ToString("00") + "."
                elapse.Milliseconds.ToString("000") + "> "
                s |]
            |> String.Concat
            |> Debug.WriteLine
    else
        Console.OutputEncoding <- Encoding.Unicode
        Console.WriteLine

type Host = {
mutable zmq: bool
mutable port: int
mutable conn: string
defaultHtml: string

mutable openDiscordAppId: string
mutable openDiscordPubKey: string
mutable openDiscordSecret: string

mutable fsDir: string }

let port__zweb port = create__ZWeb 2 port LogLevel.All false [||]

type SessionTemplate<'User,'Data> = { 
since: DateTime
mutable expiry: DateTime
mutable identity: 'User option
mutable datao: 'Data option
session: string }

type SessionsTemplate<'User,'Data> = ConcurrentDictionary<string,SessionTemplate<'User,'Data>>

type RuntimeTemplate<'User,'SessionData,'Data> = {
host: Host
data: 'Data
users: ConcurrentDictionary<int64,'User>
sessions: ConcurrentDictionary<string,SessionTemplate<'User,'SessionData>>
output: string -> unit }

type ReqRep = { req: HttpRequest; mutable rep: byte[] option }
type CWQP = CtxWrapper<ReqRep,unit>

