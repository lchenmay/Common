module UtilKestrel.Server

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

open UtilKestrel.Ctx

let runServer 
    runtime
    (devRoot, fsRoot, vueDistPath)
    (cert,certpwd)
    (apiEngine)
    (port80, port443)
    output
    (args: string[]) =

    let showHttpX (httpx:HttpContext) = 
        " <= " + httpx.Request.Method + " " + httpx.Request.Path.Value
        |> output

        httpx.Request.Headers
        |> Seq.iter(fun h ->
            h.Value
            |> Seq.iter(fun v ->
                h.Key + ": " + v |> output))

    let builder = WebApplication.CreateBuilder(args)

    // CORS
    builder.Services.AddCors(fun options ->
        options.AddDefaultPolicy(fun policy ->
            policy.AllowAnyOrigin()      // 允许所有来源（开发+正式）
                  .AllowAnyHeader()      // 允许 Authorization Header
                  .AllowAnyMethod() |> ignore
        )) |> ignore

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

    // 必须第一步
    app.UseCors() |> ignore 

    // 强力拦截：在进入任何路由前，打印并清理非正常请求
    app.Use(fun (httpx: HttpContext) (next: RequestDelegate) ->

        if httpx.Request.Method = "CONNECT" then
            httpx.Response.StatusCode <- 405 // 直接拒绝 CONNECT
            Task.CompletedTask
        elif HttpMethods.IsOptions httpx.Request.Method then
            // 显式处理 CORS 预检请求，直接返回 204
            httpx.Response.StatusCode <- 204
            Task.CompletedTask
        else 
            showHttpX httpx
            next.Invoke(httpx)) |> ignore

    // Vue 静态文件托管
    if Directory.Exists(vueDistPath) then
        let fileServerOptions = StaticFileOptions()
        fileServerOptions.FileProvider <- new PhysicalFileProvider(vueDistPath)
        app.UseStaticFiles(fileServerOptions) |> ignore

    // 启用 WebSocket 支持
    app.UseWebSockets() |> ignore

    // --- 路由与功能实现区 ---

    let runApiEngine (runtime,httpx,scheme,api) = 
        let x = EchoCtx(runtime,httpx,scheme,api)
            
        apiEngine x

        if x.Struct.contentType.Length > 0 then
            httpx.Response.ContentType <- x.Struct.contentType
        else
            httpx.Response.ContentType <- "application/json; charset=utf-8"

        httpx.Response.Headers.["Content-Security-Policy"] <- ""
        
        x

    // 1.2 GET 型 API 分发
    app.MapGet("/api/{scheme}/{api}",
        Func<string, string, HttpContext, Task>(fun scheme api httpx -> task {
            let x = runApiEngine (runtime,httpx,scheme,api)
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(x.Struct.rep))
    })) |> ignore

    // 1.2 POST 型 API 分发
    app.MapPost("/api/{scheme}/{api}",
        Func<string, string, HttpContext, Task>(fun scheme api httpx -> task {
            let x = runApiEngine (runtime,httpx,scheme,api)
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(x.Struct.rep))
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

    app.MapFallback(Func<HttpContext, Task>(fun context -> task {

        "FALLBACK" |> output
        showHttpX context

        if HttpMethods.IsGet(context.Request.Method) && 
           not (context.Request.Path.Value.StartsWith("/api", StringComparison.OrdinalIgnoreCase)) then
            let indexPath = Path.Combine(vueDistPath, "index.html")
            if File.Exists(indexPath) then
                context.Response.ContentType <- "text/html"
                do! context.Response.SendFileAsync(indexPath)
        else
            context.Response.StatusCode <- 404
    })) |> ignore

    app.Run()