module UtilKestrel.Server

open System
open System.IO
open System.Text
open System.Text.Encodings
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Logging
open System.Net.WebSockets

open Util.Bash

open UtilKestrel.Ctx

let runServer 
    runtime
    vueDistPath
    (incomingFile,fileid__bin,id__thumbnail)
    (cert,certpwd)
    (apiEngine,wsEngineo)
    (port80, port443)
    output
    (args: string[]) =

    let showHttpX (httpx:HttpContext) = 
        //" <= " + httpx.Request.Method + " " + httpx.Request.Path.Value
        //|> output

        httpx.Request.Headers
        |> Seq.iter(fun h ->
            h.Value
            |> Seq.iter(fun v ->
                h.Key + ": " + v |> output))

    let builder = WebApplication.CreateBuilder(args)


    builder.Logging.AddFilter("Microsoft.AspNetCore.Hosting.Diagnostics", LogLevel.Warning) |> ignore
    builder.Logging.AddFilter("Microsoft.AspNetCore.Routing.EndpointMiddleware", LogLevel.Warning) |> ignore
    builder.Logging.AddFilter("Microsoft.AspNetCore.StaticFiles.StaticFileMiddleware", LogLevel.Warning) |> ignore
    builder.Logging.AddFilter("Microsoft.AspNetCore", LogLevel.Error) |> ignore
    builder.Logging.ClearProviders() |> ignore

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
                "SSL Certificate loaded from: " + cert |> green |> output
            else
                "Warning: SSL certificate not found at " + cert + ". HTTPS may not work." |> red |> output
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
            //showHttpX httpx
            next.Invoke(httpx)) |> ignore

    // Vue 静态文件托管
    if Directory.Exists(vueDistPath) then
        let fileServerOptions = StaticFileOptions()
        fileServerOptions.FileProvider <- new PhysicalFileProvider(vueDistPath)
        app.UseStaticFiles(fileServerOptions) |> ignore

    // 启用 WebSocket 支持
    app.UseWebSockets() |> ignore
    app.Use(fun (context: HttpContext) (next: Func<Task>) ->
        if context.Request.Path = PathString("/ws") then
            if context.WebSockets.IsWebSocketRequest then

                context.Request.GetDisplayUrl()
                |> green |> output
                
                // 必须将 Async 转换为 Task，并强制转换为 Task 类型
                (async {
                    let! webSocket = context.WebSockets.AcceptWebSocketAsync() |> Async.AwaitTask
                    match wsEngineo with
                    | Some e -> 
                        do! e webSocket
                    | None -> return ()
                } |> Async.StartAsTask) :> Task
            else
                context.Response.StatusCode <- StatusCodes.Status400BadRequest
                Task.CompletedTask
        else
            next.Invoke()
    ) |> ignore

    // --- 路由与功能实现区 ---
    
    app.MapGet("/thumbnail/{id}", 
        Func<string, HttpContext, Task>(fun id httpx -> task {

            let bin:byte[] = id__thumbnail id

            "/thumbnail/" + id + " " + bin.Length.ToString() + " bytes" 
            |> green |> output

            //httpx.Response.Headers.["Cache-Control"] <- "public, max-age=86400"
            httpx.Response.ContentType <- "image/jpeg"

            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory bin)
    })) |> ignore

    // 文件服务：/file/{id} 
    app.MapGet("/file/{id}", 
        Func<string, HttpContext, Task>(fun id httpx -> task {
            let (bin:byte[]),mime = fileid__bin id

            "/file/" + id + " " + mime + " " + bin.Length.ToString() + " bytes" 
            |> green |> output

            //httpx.Response.Headers.["Cache-Control"] <- "public, max-age=86400"
            httpx.Response.ContentType <- mime

            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory bin)
    })) |> ignore

    // 新增：处理 /api/public/upload 路由 (在通用分发前拦截)
    app.MapPost("/api/public/upload", Func<HttpContext, Task>(fun httpx -> task {
        try
            "/api/public/upload/" 
            |> green |> output

            if not httpx.Request.HasFormContentType then
                httpx.Response.StatusCode <- 415
                return ()

            let files = 
                httpx.Request.Form.Files
                |> Seq.toArray
            
            if files.Length <> 1 then
                return ()
            
            let file = files[0]
            
            file.FileName + " " + file.Length.ToString() + " bytes"
            |> green |> output

            let rep = incomingFile httpx file |> Async.RunSynchronously

            let bin = 
                rep
                |> Util.Json.json__strFinal
                |> System.Text.Encoding.UTF8.GetBytes

            httpx.Response.ContentType <- "application/json; charset=utf-8"
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(bin))

        with ex ->
            "[Upload Error] " + ex.Message
            |> red |> output

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
            $"/api/{scheme}/{api}/" 
            |> green |> output
            let x = runApiEngine (runtime,httpx,scheme,api)
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(x.Struct.rep))
    })) |> ignore

    // 1.2 POST 型 API 分发
    app.MapPost("/api/{scheme}/{api}",
        Func<string, string, HttpContext, Task>(fun scheme api httpx -> task {
            $"/api/{scheme}/{api}/" 
            |> green |> output
            let x = runApiEngine (runtime,httpx,scheme,api)
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory x.Struct.rep)
    })) |> ignore

    app.Use(fun (context: HttpContext) (next: Func<Task>) ->
        if context.Request.Path.StartsWithSegments("/.git") || 
           context.Request.Path.StartsWithSegments("/.env") then

            "Anti scanning: " + context.Request.GetDisplayUrl() 
            |> red |> output

            // 显式将 StatusCode 设为 403
            context.Response.StatusCode <- StatusCodes.Status403Forbidden
            // 必须返回一个 Task 以符合委托签名
            Task.CompletedTask
        else
            // 继续执行管道中的下一个中间件
            next.Invoke()
    ) |> ignore


    // 保持原有的 FALLBACK 逻辑
    app.MapFallback(Func<HttpContext, Task>(fun httpx -> task {

        "FALLBACK " + httpx.Request.GetDisplayUrl() 
        |> red |> output

        //showHttpX context

        if HttpMethods.IsGet(httpx.Request.Method) && 
           not (httpx.Request.Path.Value.StartsWith("/api", StringComparison.OrdinalIgnoreCase)) then
            let indexPath = Path.Combine(vueDistPath, "index.html")
            if File.Exists(indexPath) then
                httpx.Response.ContentType <- "text/html"
                do! httpx.Response.SendFileAsync(indexPath)
        else
            httpx.Response.StatusCode <- 404
    })) |> ignore

    app.Run()