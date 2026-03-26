module UtilKestrel.Types

open System
open System.IO
open System.Collections.Concurrent
open System.Text
open System.Text.Encodings
open System.Threading.Tasks
open System.Net.WebSockets

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.FileProviders

open Util.Cat
open Util.Json
open Util.CollectionModDict
open Util.Perf
open Util.Json
open Util.Http

type ApiReturn = (string * Json)[]

type Host<'Data> = {
mutable data: 'Data
mutable port: int
mutable rdbms: Util.Db.Rdbms
mutable conn: string
mutable url: string
mutable cert: string
mutable certpwd: string

mutable updateDatabase: bool

mutable VsDirSolution: string
mutable req__vueDeployDir: string
mutable fsDir: string }

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
mutable output: string -> unit
projectCode: string }

