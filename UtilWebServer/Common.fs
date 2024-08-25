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

type Host<'Data> = {
mutable data: 'Data
mutable zmq: bool
mutable port: int
mutable conn: string
defaultHtml: string
mutable url: string

mutable database: Util.Db.Rdbms
mutable updateDatabase: bool

mutable openDiscordAppId: string
mutable openDiscordPubKey: string
mutable openDiscordSecret: string

mutable fsDir: string }

let port__zweb port = create__ZWeb 2 port LogLevel.All false [||]

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
users: ConcurrentDictionary<int64,'User>
sessions: ConcurrentDictionary<string,SessionTemplate<'User,'SessionData>>
output: string -> unit
mutable echo: HttpRequest -> byte[] option
mutable h404o: (unit -> byte[]) option
mutable wsHandler: Json -> Json option
mutable listener: TcpListener
connId: ref<int64>
queue: ModDict<int64,Conn>
keeps: ModDict<int64,Conn> }

type ReqRep = { req: HttpRequest; mutable rep: byte[] option }
type CWQP = CtxWrapper<ReqRep,unit>

let empty__Runtime<'User,'SessionData,'HostData,'RuntimeData>
    (host:Host<'HostData>)
    (data:'RuntimeData) =

    {
        host = host
        data = data 
        users = new ConcurrentDictionary<int64,'User>()
        sessions = new ConcurrentDictionary<string,SessionTemplate<'User,'SessionData>>()
        output = output
        echo = (fun _ -> None)
        h404o = None
        wsHandler = (fun _ -> None)
        listener = new TcpListener(IPAddress.Any, 80)
        connId = ref 0L
        queue = createMDInt64<Conn> 8
        keeps = createMDInt64<Conn> 8 }
