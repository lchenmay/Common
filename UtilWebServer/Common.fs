module UtilWebServer.Common

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Collections.Concurrent
open System.Text
open System.IO
open System.Diagnostics

open Util.Cat
open Util.CollectionModDict
open Util.Console
open Util.Json
open Util.Http

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


type ReqRep = { req: HttpRequest; mutable rep: byte[] option }
type CWQP = CtxWrapper<ReqRep,unit>

type Host<'Data> = {
mutable data: 'Data
mutable port: int
mutable conn: string
mutable url: string

mutable updateDatabase: bool

mutable DiscordAppId: string
mutable DiscordPubKey: string
mutable DiscordSecret: string

mutable VsDirSolution: string
mutable req__vueDeployDir: HttpRequest -> string
mutable fsDir: string }

type ConnState = 
| Idle
| Rcv
| Snd
| Keep

type Conn = {
since: DateTime
id: int64 
client: TcpClient
ns: NetworkStream
mutable idleSince: DateTime
mutable state: ConnState }

type Listener = {
port: int
socket: TcpListener
mutable echo: HttpRequest -> byte[] option
mutable h404o: (unit -> byte[]) option
mutable wsHandler: Json -> Json option
mutable fileService: HttpRequest -> byte[] option
connId: ref<int64>
queue: ModDict<int64,Conn>
keeps: ModDict<int64,Conn>
mutable output: (string -> unit) }


type SessionTemplate<'User,'Data> = { 
since: DateTime
mutable expiry: DateTime
mutable identity: 'User option
mutable datao: 'Data option
session: string }

type SessionsTemplate<'User,'Data> = ConcurrentDictionary<string,SessionTemplate<'User,'Data>>

type RuntimeTemplate<'User,'SessionData,'RuntimeData,'HostData> = {
host: Host<'HostData>
data: 'RuntimeData
mutable langs: string[]
users: ModDictInt64<'User>
sessions: ModDictStr<SessionTemplate<'User,'SessionData>>
output: string -> unit
projectCode: string
listener: Listener }
