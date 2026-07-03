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

open Util.Linux.Bash
open UtilOpen

open UtilKestrel.Types
open UtilKestrel.Ctx

let runServer 
    (runtime:RuntimeTemplate<'User,'SessionData,'RuntimeData,'HostData>)
    (incomingFile,fileid__bin,id__thumbnail)
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

    // 1. 高性能 Kestrel 配置 (保持你的 10GB 限制)
    builder.WebHost.ConfigureKestrel(fun options ->
        options.Limits.MaxRequestBodySize <- Nullable(10L * 1024L * 1024L * 1024L) 
        options.Limits.MinRequestBodyDataRate <- null
        options.Limits.KeepAliveTimeout <- TimeSpan.FromMinutes(5.0)
        options.Limits.RequestHeadersTimeout <- TimeSpan.FromMinutes(5.0)
        // 禁用 response buffering，强制使用 Content-Length 而非 chunked
        options.AllowSynchronousIO <- true

        options.ConfigureEndpointDefaults(fun ep ->
            // 强制 HTTP/1.1，避免 HTTP/2 Content-Length 帧与 body 不匹配问题
            ep.Protocols <- Microsoft.AspNetCore.Server.Kestrel.Core.HttpProtocols.Http1
        ) |> ignore

        options.ListenAnyIP(port80)
        options.ListenAnyIP(port443, fun listenOptions ->
            if File.Exists runtime.host.cert then
                listenOptions.UseHttps(runtime.host.cert,runtime.host.certpwd) |> ignore
                "SSL Certificate loaded from: " + runtime.host.cert |> green |> output
            else
                "Warning: SSL certificate not found at " + runtime.host.cert + ". HTTPS may not work." |> red |> output
        )
    ) |> ignore

    let app = builder.Build()

    // --- CORS 手动中间件（替代 UseCors，确保跨域头绝对生效）---
    // UseCors() + AllowAnyOrigin() 在 net10.0 上不追加 Access-Control-Allow-Origin
    // 改用显式中间件直接设响应头，所有请求（含 OPTIONS preflight）均覆盖
    app.Use(fun (context: HttpContext) (next: Func<Task>) ->
        task {
            context.Response.Headers["Access-Control-Allow-Origin"] <- "*"
            if HttpMethods.IsOptions(context.Request.Method) then
                context.Response.Headers["Access-Control-Allow-Methods"] <- "GET, POST, PUT, DELETE, OPTIONS"
                context.Response.Headers["Access-Control-Allow-Headers"] <- "*"
                context.Response.StatusCode <- 204
            elif context.Request.Method = "CONNECT" then
                context.Response.StatusCode <- 405 
            else
                return! next.Invoke()
        } :> Task
    ) |> ignore


    // 扫描拦截器
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

    // Vue 静态文件托管
    let vueDistPath = runtime.host.disk + "Dev/" + runtime.projectCode + "/vscode/dist"
    if Directory.Exists vueDistPath then
        let fileServerOptions = StaticFileOptions()
        fileServerOptions.FileProvider <- new PhysicalFileProvider(vueDistPath)
        fileServerOptions.OnPrepareResponse <- fun ctx ->
            // 禁止 Cloudflare CDN 缓存静态资源（Vite hash 文件名自带版本控制，不需要 CDN 缓存）
            ctx.Context.Response.Headers["Cache-Control"] <- "no-cache, no-store, must-revalidate"
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
    
    app.MapPost("/api/{scheme}/upload", Func<string, HttpContext, Task>(fun scheme httpx -> task {
        try
            $"/api/{scheme}/upload/" |> green |> output

            if not httpx.Request.HasFormContentType then
                httpx.Response.StatusCode <- 415
                return ()
            else

                // session 鉴权（仅 eu 需要）
                if scheme = "eu" then
                    let form = httpx.Request.Form
                    let session = form.["session"].ToString().Replace("\"", "")
                    match runtime.sessions.TryGet session with
                    | Some s when DateTime.UtcNow.Ticks <= s.expiry.Ticks -> ()
                    | _ ->
                        "[Upload] Unauthorized" |> red |> output
                        httpx.Response.StatusCode <- 401
                        httpx.Response.ContentType <- "application/json; charset=utf-8"
                        do! httpx.Response.WriteAsJsonAsync({| Er = "Unauthorized" |})
                        return ()

                let files =
                    httpx.Request.Form.Files
                    |> Seq.toArray

                if files.Length <> 1 then
                    httpx.Response.StatusCode <- 400
                    return ()

                let file = files.[0]

                file.FileName + " " + file.Length.ToString() + " bytes"
                |> green |> output

                let! rep = incomingFile httpx file |> Async.StartImmediateAsTask

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
        // 显式设置 Content-Length 避免浏览器 Content-Length 不匹配错误
        httpx.Response.ContentLength <- int64 x.Struct.rep.Length
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


    // 保持原有的 FALLBACK 逻辑
    app.MapFallback(Func<HttpContext, Task>(fun httpx -> task {

        "FALLBACK " + httpx.Request.GetDisplayUrl() 
        |> red |> output

        //showHttpX context

        if HttpMethods.IsGet(httpx.Request.Method) && 
           not (httpx.Request.Path.Value.StartsWith("/api", StringComparison.OrdinalIgnoreCase)) then
            let indexPath = Path.Combine(vueDistPath, "index.html")
            if File.Exists(indexPath) then
                let fileInfo = FileInfo(indexPath)
                httpx.Response.ContentType <- "text/html"
                httpx.Response.ContentLength <- fileInfo.Length
                do! httpx.Response.SendFileAsync(indexPath)
        else
            httpx.Response.StatusCode <- 404
    })) |> ignore


    "UtilKestrel.Server: app.Run(), comming up ..."
    |> green |> output

    app.Run()