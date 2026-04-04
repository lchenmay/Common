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

open Util.Json

open UtilKestrel.Ctx

let runServer 
    runtime
    vueDistPath
    (incomingFile,fileid__localpath)
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

    // --- 保持原有功能，新增大文件表单配置 ---
    builder.Services.Configure<Microsoft.AspNetCore.Http.Features.FormOptions>
        (fun (options:Microsoft.AspNetCore.Http.Features.FormOptions) ->
            options.MultipartBodyLengthLimit <- 10L * 1024L * 1024L * 1024L // 10GB
            options.ValueLengthLimit <- Int32.MaxValue
            options.MemoryBufferThreshold <- 1024 * 1024) |> ignore

    // CORS 保持不变
    builder.Services.AddCors(fun options ->
        options.AddDefaultPolicy(fun policy ->
            policy.AllowAnyOrigin()      
                  .AllowAnyHeader()      
                  .AllowAnyMethod() |> ignore
        )) |> ignore

    // 1. 高性能 Kestrel 配置 (保持你的 10GB 限制)
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

    // 强力拦截：保持原有逻辑
    app.Use(fun (httpx: HttpContext) (next: RequestDelegate) ->
        if httpx.Request.Method = "CONNECT" then
            httpx.Response.StatusCode <- 405 
            Task.CompletedTask
        elif HttpMethods.IsOptions httpx.Request.Method then
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
    
    // 新增：处理 /api/public/upload 路由 (在通用分发前拦截)
    app.MapPost("/api/public/upload", Func<HttpContext, Task>(fun httpx -> task {
        try
            if not httpx.Request.HasFormContentType then
                httpx.Response.StatusCode <- 415
                return ()

            let files = 
                httpx.Request.Form.Files
                |> Seq.toArray
            
            if files.Length <> 1 then
                return ()

            let rep = incomingFile files[0] |> Async.RunSynchronously

            let bin = 
                rep
                |> Util.Json.json__strFinal
                |> System.Text.Encoding.UTF8.GetBytes

            httpx.Response.ContentType <- "application/json; charset=utf-8"
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(bin))

        with ex ->
            "[Upload Error] " + ex.Message |> output
            httpx.Response.StatusCode <- 500
            do! httpx.Response.WriteAsJsonAsync({| Er = ex.Message; Size = 0L |})
    })) |> ignore

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

    // 2. 文件服务：/file/{id} 
    app.MapGet("/file/{id}", Func<string, HttpContext, Task<IResult>>(fun id context -> task {
        let fullPath = fileid__localpath id
        if File.Exists fullPath then
            return Results.File(fullPath, enableRangeProcessing = true)
        else 
            return Results.NotFound()
    })) |> ignore

    app.MapGet("/thumbnail/{id}", Func<string, HttpContext, Task<IResult>>(fun id context -> task {
        let fullPath = fileid__localpath id
        if File.Exists fullPath then
            return Results.File(fullPath, enableRangeProcessing = true)
        else 
            return Results.NotFound()
    })) |> ignore

    // 保持原有的 FALLBACK 逻辑
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