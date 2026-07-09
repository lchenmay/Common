module UtilKestrel.Server

open System
open System.IO
open System.IO.Compression
open System.Text
open System.Text.Encodings
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.ResponseCompression
open Microsoft.AspNetCore.StaticFiles
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.FileProviders
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.WebUtilities
open System.Net.WebSockets

open Util.Perf
open Util.Linux.Bash
open Util.Bin

open UtilOpen

open UtilKestrel.Types
open UtilKestrel.Ctx

let private cacheVueDistResponse (ctx: StaticFileResponseContext) =
    let requestPath = ctx.Context.Request.Path.Value
    let response = ctx.Context.Response

    if isNull requestPath || requestPath.EndsWith(".html", StringComparison.OrdinalIgnoreCase) then
        response.Headers["Cache-Control"] <- "no-cache, no-store, must-revalidate"
        response.Headers["Pragma"] <- "no-cache"
        response.Headers["Expires"] <- "0"
    elif requestPath.StartsWith("/js/", StringComparison.OrdinalIgnoreCase)
      || requestPath.StartsWith("/as/", StringComparison.OrdinalIgnoreCase)
      || requestPath.Equals("/favicon.png", StringComparison.OrdinalIgnoreCase) then
        response.Headers["Cache-Control"] <- "public, max-age=31536000, immutable"
    else
        response.Headers["Cache-Control"] <- "public, max-age=86400"

let private setPrivateBinaryCache (httpx:HttpContext) (tag:string) =
    let etag = "\"" + tag.Replace("\"", "") + "\""
    httpx.Response.Headers["Cache-Control"] <- "private, max-age=86400"
    httpx.Response.Headers["ETag"] <- etag
    httpx.Request.Headers["If-None-Match"].ToString() = etag

let private activeSessionExists (runtime:RuntimeTemplate<'User,'SessionData,'RuntimeData,'HostData>) session =
    if String.IsNullOrWhiteSpace session then false
    else
        match runtime.sessions.TryGet session with
        | Some s when DateTime.UtcNow.Ticks <= s.expiry.Ticks -> true
        | Some _ ->
            runtime.sessions.Remove session |> ignore
            false
        | None -> false

let private tryReadJsonStringProperty (name:string) (body:string) =
    try
        use doc = System.Text.Json.JsonDocument.Parse(body)
        let mutable prop = Unchecked.defaultof<System.Text.Json.JsonElement>
        if doc.RootElement.TryGetProperty(name, &prop) then prop.ToString()
        else ""
    with _ -> ""

let private jsonString (value:string) =
    System.Text.Json.JsonSerializer.Serialize(value)

let private jsonBytes (text:string) =
    Encoding.UTF8.GetBytes text

let runServer 
    (runtime:RuntimeTemplate<'User,'SessionData,'RuntimeData,'HostData>)
    (incomingFileWithPath, fileid__bin, _fileid__binSecret, fileid__url)
    (apiEngine,wsEngineo)
    (port80, port443)
    output
    (args: string[]) =

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

    builder.Services.AddResponseCompression(fun options ->
        options.EnableForHttps <- true
        options.Providers.Add<BrotliCompressionProvider>()
        options.Providers.Add<GzipCompressionProvider>()
        options.MimeTypes <-
            Seq.append
                ResponseCompressionDefaults.MimeTypes
                [ "application/javascript"
                  "text/javascript"
                  "application/x-javascript"
                  "application/json"
                  "application/manifest+json"
                  "application/wasm"
                  "image/svg+xml" ]
    ) |> ignore

    builder.Services.Configure<BrotliCompressionProviderOptions>(fun (options:BrotliCompressionProviderOptions) ->
        options.Level <- CompressionLevel.Fastest
    ) |> ignore

    builder.Services.Configure<GzipCompressionProviderOptions>(fun (options:GzipCompressionProviderOptions) ->
        options.Level <- CompressionLevel.Fastest
    ) |> ignore

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

    app.UseResponseCompression() |> ignore

    // --- CORS 手动中间件（替代 UseCors，确保跨域头绝对生效）---
    // UseCors() + AllowAnyOrigin() 在 net10.0 上不追加 Access-Control-Allow-Origin
    // 改用显式中间件直接设响应头，所有请求（含 OPTIONS preflight）均覆盖
    app.Use(fun (context: HttpContext) (next: Func<Task>) ->
        task {
            context.Response.Headers["Access-Control-Allow-Origin"] <- "*"
            context.Response.Headers["Access-Control-Expose-Headers"] <- "ETag"
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


    // 请求 body 可回退缓冲（防止被多处读取后抛 BadHttpRequestException）
    app.Use(fun (context: HttpContext) (next: Func<Task>) ->
        task {
            context.Request.EnableBuffering()
            do! next.Invoke()
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
        fileServerOptions.OnPrepareResponse <- cacheVueDistResponse
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

            if setPrivateBinaryCache httpx ("thumbnail-" + id) then
                httpx.Response.StatusCode <- StatusCodes.Status304NotModified
            else
                let (bin:byte[]),(mime:string) =
                  use cw = new CodeWrapper("id__thumbnail")
                  fileid__bin id

                "/thumbnail/" + id + " " + bin.Length.ToString() + " bytes" 
                |> green |> output

                if bin.Length > 0 && mime.StartsWith("image/", StringComparison.OrdinalIgnoreCase) then
                    httpx.Response.ContentType <- mime
                    do! httpx.Response.Body.WriteAsync(ReadOnlyMemory bin)
                else
                    httpx.Response.StatusCode <- StatusCodes.Status404NotFound
    })) |> ignore

    // 文件服务：/file/{id} 
    app.MapGet("/file/{id}/{secret}", 
        Func<string, string, HttpContext, Task>(fun id secret httpx -> task {
            let expectedUrl = fileid__url id
            let validSecret =
                not (String.IsNullOrWhiteSpace expectedUrl) &&
                expectedUrl.EndsWith("/" + secret, StringComparison.Ordinal)

            if not validSecret then
                httpx.Response.StatusCode <- StatusCodes.Status404NotFound
            elif setPrivateBinaryCache httpx ("file-" + id + "-" + secret) then
                httpx.Response.StatusCode <- StatusCodes.Status304NotModified
            else
                let (bin:byte[]),(mime:string) =
                    use cw = new CodeWrapper("fileid__bin.encryptedUrl")
                    fileid__bin id

                "/file/" + id + "/*** " + mime + " " + bin.Length.ToString() + " bytes" 
                |> green |> output

                if bin.Length > 0 && not (String.IsNullOrWhiteSpace mime) then
                    httpx.Response.ContentType <- mime
                    do! httpx.Response.Body.WriteAsync(ReadOnlyMemory bin)
                else
                    httpx.Response.StatusCode <- StatusCodes.Status404NotFound
    })) |> ignore

    app.MapGet("/file/{id}", 
        Func<string, HttpContext, Task>(fun id httpx -> task {
            "/file/" + id + " rejected: missing secret" |> red |> output
            httpx.Response.StatusCode <- StatusCodes.Status404NotFound
    })) |> ignore

    app.MapPost("/api/{scheme}/fileUrl", Func<string, HttpContext, Task>(fun scheme httpx -> task {
        try
            let! body =
                use reader = new StreamReader(httpx.Request.Body, Encoding.UTF8)
                reader.ReadToEndAsync()

            let session = tryReadJsonStringProperty "session" body
            if not (activeSessionExists runtime session) then
                httpx.Response.StatusCode <- StatusCodes.Status401Unauthorized
                httpx.Response.ContentType <- "application/json; charset=utf-8"
                do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(jsonBytes """{"Er":"Unauthorized"}"""))
            else
                let fileId = tryReadJsonStringProperty "fileId" body
                let url = fileid__url fileId
                httpx.Response.ContentType <- "application/json; charset=utf-8"
                if String.IsNullOrWhiteSpace url then
                    httpx.Response.StatusCode <- StatusCodes.Status404NotFound
                    do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(jsonBytes """{"Er":"NotFound"}"""))
                else
                    let body = """{"Er":"OK","url":""" + jsonString url + "}"
                    do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(jsonBytes body))
        with ex ->
            "[FileUrl Error] " + ex.Message
            |> red |> output
            httpx.Response.StatusCode <- StatusCodes.Status500InternalServerError
            httpx.Response.ContentType <- "application/json; charset=utf-8"
            let body = """{"Er":""" + jsonString ex.Message + "}"
            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(jsonBytes body))
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

                if files.Length = 0 then
                    httpx.Response.StatusCode <- 400
                    return ()

                let form = httpx.Request.Form
                let relativePath = 
                    if form.ContainsKey "relativePath" then 
                        form.["relativePath"].ToString().Replace("\"", "") 
                    else ""
                let parentFolderId =
                    if form.ContainsKey "folderId" then
                        try form.["folderId"].ToString().Replace("\"", "") |> int64
                        with _ -> 0L
                    else 0L

                let! reps = 
                    files 
                    |> Array.map (fun file ->
                        file.FileName + " " + file.Length.ToString() + " bytes"
                        |> green |> output
                        incomingFileWithPath httpx file relativePath parentFolderId)
                    |> fun tasks -> Async.Parallel(tasks, maxDegreeOfParallelism = 3) 
                    |> Async.StartImmediateAsTask

                let ary =
                    reps
                    |> Array.map (fun rep ->
                        rep |> Util.Json.json__strFinal)
                    |> String.concat ","
                    |> fun s -> "[" + s + "]"

                let bin =
                    ary
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
      Func<string, string, HttpContext, Task>(
        fun scheme api httpx -> 
          task {
            $"/api/{scheme}/{api}/" 
            |> green |> output
            let x = runApiEngine (runtime,httpx,scheme,api)
            do! 
              x.Struct.rep
              |> ReadOnlyMemory
              |> httpx.Response.Body.WriteAsync})) |> ignore

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
                httpx.Response.Headers["Cache-Control"] <- "no-cache, no-store, must-revalidate"
                httpx.Response.Headers["Pragma"] <- "no-cache"
                httpx.Response.Headers["Expires"] <- "0"
                httpx.Response.ContentLength <- fileInfo.Length
                do! httpx.Response.SendFileAsync(indexPath)
        else
            httpx.Response.StatusCode <- 404
    })) |> ignore


    "UtilKestrel.Server: app.Run(), comming up ..."
    |> green |> output

    app.Run()
