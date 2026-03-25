module UtilWebServer.Kestrel

open System
open System.IO
open System.Text
open System.Text.Encodings
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.FileProviders
open System.Net.WebSockets

open UtilWebServer.Api

type KestrelCtx = {
scheme: string
api: string
httpx: HttpContext
req: byte[]
mutable proco: (KestrelCtx -> ApiReturn) option
mutable rep: byte[]
mutable contentType: string }

let runServer 
    (devRoot, fsRoot, vueDistPath)
    (cert,certpwd)
    (echo)
    (port80, port443)
    output
    (args: string[]) =

    let builder = WebApplication.CreateBuilder(args)

    // 1. 高性能 Kestrel 配置
    builder.WebHost.ConfigureKestrel(fun options ->
        options.Limits.MaxRequestBodySize <- Nullable(10L * 1024L * 1024L * 1024L) 
        options.Limits.MinRequestBodyDataRate <- null 

        options.ListenAnyIP(port80)
        options.ListenAnyIP(port443, fun listenOptions ->
            if File.Exists cert then
                listenOptions.UseHttps(cert,certpwd) |> ignore
                "SSL Certificate loaded from: " + cert |> output
            else
                "Warning: SSL certificate not found at " + cert + ". HTTPS may not work." |> output
        )

    ) |> ignore

    let app = builder.Build()

    // 启用 WebSocket 支持
    app.UseWebSockets() |> ignore

    // --- 路由与功能实现区 ---

    let read (context:HttpContext) = 

        if context.Request.ContentLength.HasValue then
            let length = int context.Request.ContentLength.Value
            let buffer = Array.zeroCreate<byte> length
            let! _ = context.Request.Body.ReadAsync(buffer, 0, length)
            task { return buffer }
        else
            task {
                use ms = new MemoryStream()
                do! context.Request.Body.CopyToAsync(ms)
                return ms.ToArray()
            }

    let req__kestrelx (scheme,api,httpx,req) = 
        {   scheme = scheme
            api = api
            httpx = httpx
            req = req
            rep = [| |]
            proco = None
            contentType = "" }

    // 1.2 GET 型 API 分发
    app.MapGet("/api/{scheme}/{api}",
        Func<string, string, HttpContext, Task>(fun scheme api httpx -> task {
            let! reqBodyBin = read httpx
            let kestrelx = 
                (scheme,api,httpx,reqBodyBin)
                |> req__kestrelx

            echo kestrelx
        
            if kestrelx.contentType.Length > 0 then
                httpx.Response.ContentType <- kestrelx.contentType
            else
                httpx.Response.ContentType <- "application/json; charset=utf-8"
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(kestrelx.rep))
    })) |> ignore


    // 1.2 POST 型 API 分发
    app.MapPost("/api/{scheme}/{api}",
        Func<string, string, HttpContext, Task>(fun scheme api httpx -> task {
            let! reqBodyBin = read httpx
            let kestrelx = 
                (scheme,api,httpx,reqBodyBin)
                |> req__kestrelx

            echo kestrelx
        
            if kestrelx.contentType.Length > 0 then
                httpx.Response.ContentType <- kestrelx.contentType
            else
                httpx.Response.ContentType <- "application/json; charset=utf-8"
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(kestrelx.rep))
    })) |> ignore

    // 2. 文件服务：/file/{id} 映射到动态计算的 fsRoot
    app.MapGet("/file/{id}", Func<string, HttpContext, Task<IResult>>(fun id context -> task {
        let fullPath = Path.Combine(fsRoot, id) 
        if File.Exists(fullPath) then
            // 支持大文件断点续传
            return Results.File(fullPath, enableRangeProcessing = true)
        else 
            return Results.NotFound()
    })) |> ignore

    // 3. Vue 静态文件托管
    if Directory.Exists(vueDistPath) then
        let fileServerOptions = StaticFileOptions()
        fileServerOptions.FileProvider <- new PhysicalFileProvider(vueDistPath)
        app.UseStaticFiles(fileServerOptions) |> ignore

        // 4. 兜底处理：SPA 路由支持 (过滤非 GET 请求以修复 CONNECT 异常)
        app.MapFallback(Func<HttpContext, Task>(fun context -> task {
            if HttpMethods.IsGet(context.Request.Method) then
                let indexPath = Path.Combine(vueDistPath, "index.html")
                if File.Exists(indexPath) then
                    context.Response.ContentType <- "text/html"
                    do! context.Response.SendFileAsync(indexPath)
                else
                    context.Response.StatusCode <- 404
            else
                // 对于非 GET 请求（如 CONNECT），只返回状态码而不写入 Body
                context.Response.StatusCode <- 200
        })) |> ignore

    app.Run()