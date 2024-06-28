module UtilWebServer.Common

open System
open System.Collections.Concurrent
open System.Text
open System.IO
open System.Diagnostics

open Util.Cat
open Util.Console
open Util.Http
open Util.Zmq

let since = DateTime.UtcNow

let output:string -> unit = 

    let assbly = System.Reflection.Assembly.GetCallingAssembly()
    let dir = Directory.GetCurrentDirectory()
    if dir.EndsWith "WebService" then
        prompt since >> Debug.WriteLine
    else if dir.EndsWith "WebApp" then
        prompt since >> Debug.WriteLine
    else
        Console.OutputEncoding <- Encoding.Unicode
        prompt since >> Console.WriteLine

type Host = {
mutable zmq: bool
mutable port: int
mutable conn: string
defaultHtml: string
mutable url: string

mutable updateDatabase: bool

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

