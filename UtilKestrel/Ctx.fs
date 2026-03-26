module UtilKestrel.Ctx

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

open UtilKestrel.Types

type EchoCtxStruct<'Runtime,'Session,'Error> = {
scheme: string
api: string
httpx: HttpContext
mutable proco: (EchoCtx<'Runtime,'Session,'Error> -> ApiReturn) option
mutable rep: byte[]
mutable contentType: string
mutable sessiono: 'Session option  
mutable ero: 'Error option
runtime: 'Runtime }
and EchoCtx<'Runtime,'Session,'Error>
    (runtime,httpx,scheme,api) = 

    let s:EchoCtxStruct<'Runtime,'Session,'Error> = {
        scheme = scheme
        api = api
        httpx = httpx
        proco = None
        rep = [||]
        contentType = ""
        sessiono = None
        ero = None
        runtime = runtime }

    let read () = task {
        use cw = new CodeWrapper("branch.exe")

        if httpx.Request.ContentLength.HasValue then
            let length = int httpx.Request.ContentLength.Value
            let buffer = Array.zeroCreate<byte> length
            // 确保填满 buffer
            do! httpx.Request.Body.ReadExactlyAsync(Memory<byte>(buffer))
            return buffer
        else
            use ms = new MemoryStream()
            do! httpx.Request.Body.CopyToAsync(ms)
            return ms.ToArray()
    }
    
    let bodyRead =
        lazy ((read ()).GetAwaiter().GetResult() )

    member val Struct = s with get

    member this.Path = 
        s.httpx.Request.Path.ToString()

    member this.PathSegments = 
        this.Path.Split [| '/' |]

    member this.ReqBodyBin = bodyRead.Value

    member this.Json
        with get() = 
            this.ReqBodyBin
            |> Encoding.UTF8.GetString
            |> Util.Json.str__root
