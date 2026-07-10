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

let private wantsFileSecret (httpx:HttpContext) =
    let q = httpx.Request.Query
    q.ContainsKey("secret") && q["secret"].ToString() = "1"

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

let private readRequestBodySafe (httpx:HttpContext) = task {
    try
        if httpx.Request.Body.CanSeek then
            httpx.Request.Body.Position <- 0L
        use reader = new StreamReader(httpx.Request.Body, Encoding.UTF8, false, 1024, true)
        return! reader.ReadToEndAsync()
    with
    | :? ObjectDisposedException -> return ""
}

// ========== 分片上传辅助模块 ==========

let private maxChunkUploadSize = 10L * 1024L * 1024L * 1024L // 10GB
let private defaultChunkSize = 4 * 1024 * 1024 // 4MB

type private ChunkedUploadMeta = {
    uploadId: string
    session: string
    fileName: string
    fileSize: int64
    contentType: string
    totalChunks: int
    chunkSize: int
    receivedChunks: System.Collections.Generic.HashSet<int>
    relativePath: string
    parentFolderId: int64
    createdAt: DateTime
    tempDir: string
}

let private chunkedUploads = 
    System.Collections.Concurrent.ConcurrentDictionary<string, ChunkedUploadMeta>()

let private tryReadJsonInt64Property (body:string) (name:string) =
    try
        use doc = System.Text.Json.JsonDocument.Parse(body)
        let mutable prop = Unchecked.defaultof<System.Text.Json.JsonElement>
        if doc.RootElement.TryGetProperty(name, &prop) then
            match prop.ValueKind with
            | System.Text.Json.JsonValueKind.Number -> prop.GetInt64()
            | System.Text.Json.JsonValueKind.String ->
                match Int64.TryParse(prop.GetString()) with
                | true, value -> value
                | false, _ -> 0L
            | _ -> 0L
        else 0L
    with _ -> 0L

let private tryReadJsonIntProperty (body:string) (name:string) =
    try
        use doc = System.Text.Json.JsonDocument.Parse(body)
        let mutable prop = Unchecked.defaultof<System.Text.Json.JsonElement>
        if doc.RootElement.TryGetProperty(name, &prop) then
            match prop.ValueKind with
            | System.Text.Json.JsonValueKind.Number -> prop.GetInt32()
            | System.Text.Json.JsonValueKind.String ->
                match Int32.TryParse(prop.GetString()) with
                | true, value -> value
                | false, _ -> 0
            | _ -> 0
        else 0
    with _ -> 0

let private verifySession 
    (runtime:RuntimeTemplate<_,_,_,_>) 
    (scheme:string) 
    (session:string) =
    if scheme <> "eu" then Ok ""
    elif activeSessionExists runtime session then Ok session
    else Error 401

let private chunksBaseDir (runtime:RuntimeTemplate<_,_,_,_>) =
    runtime.host.disk + "FsRoot/" + runtime.projectCode + "/_chunks"

let private writeChunkMeta (meta:ChunkedUploadMeta) =
    let chunksJson = 
        meta.receivedChunks 
        |> Seq.map string |> String.concat ","
        |> fun s -> "[" + s + "]"
    let json = 
        System.String.Format(
            """{{"uploadId":"{0}","session":"{1}","fileName":"{2}","fileSize":{3},"contentType":"{4}","totalChunks":{5},"chunkSize":{6},"receivedChunks":{7},"relativePath":"{8}","parentFolderId":{9},"createdAt":"{10}"}}""",
            meta.uploadId, meta.session, meta.fileName, meta.fileSize, meta.contentType,
            meta.totalChunks, meta.chunkSize, chunksJson,
            meta.relativePath, meta.parentFolderId,
            meta.createdAt.ToString("o"))
    File.WriteAllText(meta.tempDir + "/meta.json", json)

let private readChunkMeta (dir:string) =
    let metaPath = dir + "/meta.json"
    if File.Exists metaPath then
        try
            let json = File.ReadAllText(metaPath)
            use doc = System.Text.Json.JsonDocument.Parse(json)
            let root = doc.RootElement
            let mutable prop = Unchecked.defaultof<System.Text.Json.JsonElement>
            Some {
                uploadId = if root.TryGetProperty("uploadId", &prop) then prop.GetString() else ""
                session = if root.TryGetProperty("session", &prop) then prop.GetString() else ""
                fileName = if root.TryGetProperty("fileName", &prop) then prop.GetString() else ""
                fileSize = if root.TryGetProperty("fileSize", &prop) then prop.GetInt64() else 0L
                contentType = if root.TryGetProperty("contentType", &prop) then prop.GetString() else ""
                totalChunks = if root.TryGetProperty("totalChunks", &prop) then prop.GetInt32() else 0
                chunkSize = if root.TryGetProperty("chunkSize", &prop) then prop.GetInt32() else 0
                receivedChunks = 
                    if root.TryGetProperty("receivedChunks", &prop) then
                        let chunks = System.Collections.Generic.HashSet<int>()
                        for item in prop.EnumerateArray() do chunks.Add(item.GetInt32()) |> ignore
                        chunks
                    else System.Collections.Generic.HashSet<int>()
                relativePath = if root.TryGetProperty("relativePath", &prop) then prop.GetString() else ""
                parentFolderId = if root.TryGetProperty("parentFolderId", &prop) then prop.GetInt64() else 0L
                createdAt = if root.TryGetProperty("createdAt", &prop) then prop.GetDateTime() else DateTime.UtcNow
                tempDir = dir
            }
        with _ -> None
    else None

let private cleanupStaleChunks (runtime:RuntimeTemplate<_,_,_,_>) =
    let baseDir = chunksBaseDir runtime
    if Directory.Exists baseDir then
        let threshold = DateTime.UtcNow.AddHours(-2.0)
        try
            for dir in Directory.GetDirectories(baseDir) do
                try
                    let dirInfo = DirectoryInfo(dir)
                    if dirInfo.LastWriteTimeUtc < threshold then
                        Directory.Delete(dir, true)
                        ("[ChunkGC] removed stale: " + Path.GetFileName(dir)) 
                        |> yellow 
                        |> runtime.output
                with _ -> ()
        with _ -> ()

let private ensureChunksDir (runtime:RuntimeTemplate<_,_,_,_>) uploadId =
    let dir = chunksBaseDir runtime + "/" + uploadId
    if not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> ignore
    dir

let private writeChunkFileHelperAsync (dir:string) (chunkIndex:int) (formFile:IFormFile) = task {
    let chunkPath = dir + "/chunk_" + chunkIndex.ToString("D5")
    use fs = new FileStream(chunkPath, FileMode.Create, FileAccess.Write, FileShare.None, 64 * 1024, true)
    do! formFile.CopyToAsync(fs)
    do! fs.FlushAsync()
    return FileInfo(chunkPath).Length
}

let private mergeChunks (meta:ChunkedUploadMeta) =
    let mergedPath = meta.tempDir + "/merged.dat"
    use merged = new FileStream(mergedPath, FileMode.Create)
    let buffer = Array.zeroCreate<byte> (64 * 1024)
    for i in 0 .. meta.totalChunks - 1 do
        let chunkPath = meta.tempDir + "/chunk_" + i.ToString("D5")
        if not (File.Exists chunkPath) then
            failwithf "Missing chunk %d" i
        use chunk = new FileStream(chunkPath, FileMode.Open, FileAccess.Read)
        let mutable bytesRead = chunk.Read(buffer, 0, buffer.Length)
        while bytesRead > 0 do
            merged.Write(buffer, 0, bytesRead)
            bytesRead <- chunk.Read(buffer, 0, buffer.Length)
    merged.Flush()
    let totalSize = FileInfo(mergedPath).Length
    if totalSize <> meta.fileSize then
        failwithf "Size mismatch: expected %d, got %d" meta.fileSize totalSize
    mergedPath

let private resolveAndVerifyMeta
    (runtime:RuntimeTemplate<_,_,_,_>)
    (uploadId:string)
    (session:string)
    (scheme:string) =
    match chunkedUploads.TryGetValue uploadId with
    | true, meta ->
        if scheme = "eu" && meta.session <> session then
            None
        else Some meta
    | false, _ ->
        let dir = chunksBaseDir runtime + "/" + uploadId
        match readChunkMeta dir with
        | Some meta ->
            if scheme = "eu" && meta.session <> session then
                None
            else
                chunkedUploads.[uploadId] <- meta
                ("[ChunkRecover] restored " + uploadId) 
                |> yellow 
                |> runtime.output
                Some meta
        | None -> None

type FileHandler = 
  { fileid__bin: string -> byte[] * string
    fileid__binSecret: string -> byte[] * string * Numerics.BigInteger
    fileid__url: string -> string
    formfile__relativePath__parentFolderId__AsyncJson: IFormFile -> string -> int64 -> Async<Util.Json.Json>
    chunkfile__parentFolderId__AsyncJson: (string -> string -> string -> int64 -> int64 -> string -> Async<Util.Json.Json>) option }

let runServer 
    (runtime:RuntimeTemplate<'User,'SessionData,'RuntimeData,'HostData>)
    fileHandler
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
            context.Response.Headers["Access-Control-Expose-Headers"] <- "ETag, X-Aiarwa-File-Secret"
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
                  fileHandler.fileid__bin id

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
            let expectedUrl = fileHandler.fileid__url id
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
                    fileHandler.fileid__bin id

                "/file/" + id + "/*** " + mime + " " + bin.Length.ToString() + " bytes" 
                |> green |> output

                if bin.Length > 0 && not (String.IsNullOrWhiteSpace mime) then
                    httpx.Response.ContentType <- mime
                    do! httpx.Response.Body.WriteAsync(ReadOnlyMemory bin)
                else
                    httpx.Response.StatusCode <- StatusCodes.Status404NotFound
    })) |> ignore

    app.MapPost("/api/{scheme}/fileUrl", Func<string, HttpContext, Task>(fun scheme httpx -> task {
        try
            let! body = readRequestBodySafe httpx

            let session = tryReadJsonStringProperty "session" body
            if not (activeSessionExists runtime session) then
                httpx.Response.StatusCode <- StatusCodes.Status401Unauthorized
                httpx.Response.ContentType <- "application/json; charset=utf-8"
                do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(jsonBytes """{"Er":"Unauthorized"}"""))
            else
                let fileId = tryReadJsonStringProperty "fileId" body
                let url = fileHandler.fileid__url fileId
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
                        fileHandler.formfile__relativePath__parentFolderId__AsyncJson file relativePath parentFolderId)
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

    // ========== 分片上传端点 ==========

    // --- uploadChunk/init ---
    app.MapPost("/api/{scheme}/uploadChunk/init", 
        Func<string, HttpContext, Task>(fun scheme httpx -> task {
        try
            if fileHandler.chunkfile__parentFolderId__AsyncJson.IsNone then
                httpx.Response.StatusCode <- 501
                do! httpx.Response.WriteAsJsonAsync({| Er = "Chunked upload not supported" |})
                return ()

            let! body = readRequestBodySafe httpx

            let session = tryReadJsonStringProperty "session" body |> fun s -> s.Replace("\"", "")
            match verifySession runtime scheme session with
            | Error code ->
                httpx.Response.StatusCode <- code
                do! httpx.Response.WriteAsJsonAsync({| Er = "Unauthorized" |})
                return ()
            | Ok sessionVerified ->

            let fileName = tryReadJsonStringProperty "fileName" body |> fun s -> s.Replace("\"", "")
            let fileSize = tryReadJsonInt64Property body "fileSize"
            let contentType = tryReadJsonStringProperty "contentType" body |> fun s -> s.Replace("\"", "")
            let chunkSize = 
                let cs = tryReadJsonIntProperty body "chunkSize"
                if cs > 0 then cs else defaultChunkSize

            if String.IsNullOrWhiteSpace fileName || fileSize <= 0L then
                httpx.Response.StatusCode <- 400
                do! httpx.Response.WriteAsJsonAsync({| Er = "Missing fileName or invalid fileSize" |})
                return ()
            if fileSize > maxChunkUploadSize then
                httpx.Response.StatusCode <- 413
                do! httpx.Response.WriteAsJsonAsync({| Er = sprintf "File too large: %d bytes (max %d)" fileSize maxChunkUploadSize |})
                return ()

            cleanupStaleChunks runtime

            let uploadId = Guid.NewGuid().ToString("N")
            let totalChunks = int (Math.Ceiling(float fileSize / float chunkSize))
            let dir = ensureChunksDir runtime uploadId

            let meta = {
                uploadId = uploadId
                session = sessionVerified
                fileName = fileName
                fileSize = fileSize
                contentType = contentType
                totalChunks = totalChunks
                chunkSize = chunkSize
                receivedChunks = System.Collections.Generic.HashSet<int>()
                relativePath = ""
                parentFolderId = 0L
                createdAt = DateTime.UtcNow
                tempDir = dir
            }
            chunkedUploads.[uploadId] <- meta
            writeChunkMeta meta

            ("[ChunkInit] " + uploadId + " " + fileName + " " + 
             fileSize.ToString() + " bytes, " + totalChunks.ToString() + " chunks")
            |> green |> output

            httpx.Response.ContentType <- "application/json; charset=utf-8"
            do! httpx.Response.WriteAsJsonAsync({| Er = "OK"; uploadId = uploadId; totalChunks = totalChunks; chunkSize = chunkSize |})

        with ex ->
            ("[ChunkInit Error] " + ex.Message) |> red |> output
            httpx.Response.StatusCode <- 500
            do! httpx.Response.WriteAsJsonAsync({| Er = ex.Message |})
    })) |> ignore

    // --- uploadChunk (multipart) ---
    app.MapPost("/api/{scheme}/uploadChunk", 
        Func<string, HttpContext, Task>(fun scheme httpx -> task {
        try
            if fileHandler.chunkfile__parentFolderId__AsyncJson.IsNone then
                httpx.Response.StatusCode <- 501
                return ()

            if not httpx.Request.HasFormContentType then
                httpx.Response.StatusCode <- 415
                return ()
            else
                let! form = httpx.Request.ReadFormAsync()

                let uploadId = 
                    if form.ContainsKey "uploadId" then form.["uploadId"].ToString().Replace("\"", "")
                    else ""
                let session = 
                    if form.ContainsKey "session" then form.["session"].ToString().Replace("\"", "")
                    else ""

                match verifySession runtime scheme session with
                | Error code ->
                    httpx.Response.StatusCode <- code
                    do! httpx.Response.WriteAsJsonAsync({| Er = "Unauthorized" |})
                    return ()
                | Ok sessionVerified ->

                let chunkIndex =
                    if form.ContainsKey "chunkIndex" then
                        try int (form.["chunkIndex"].ToString().Replace("\"", ""))
                        with _ -> -1
                    else -1

                if String.IsNullOrWhiteSpace uploadId || chunkIndex < 0 then
                    httpx.Response.StatusCode <- 400
                    do! httpx.Response.WriteAsJsonAsync({| Er = "Missing uploadId or chunkIndex" |})
                    return ()

                match resolveAndVerifyMeta runtime uploadId sessionVerified scheme with
                | None ->
                    httpx.Response.StatusCode <- 404
                    do! httpx.Response.WriteAsJsonAsync({| Er = "Upload not found" |})
                    return ()
                | Some meta ->
                    if chunkIndex >= meta.totalChunks then
                        httpx.Response.StatusCode <- 400
                        do! httpx.Response.WriteAsJsonAsync({| Er = "chunkIndex out of range" |})
                        return ()

                    let files = form.Files |> Seq.toArray
                    if files.Length = 0 then
                        httpx.Response.StatusCode <- 400
                        do! httpx.Response.WriteAsJsonAsync({| Er = "No file in form" |})
                        return ()

                    let chunkFile = files.[0]
                    let! chunkActualSize = writeChunkFileHelperAsync meta.tempDir chunkIndex chunkFile

                    meta.receivedChunks.Add(chunkIndex) |> ignore
                    chunkedUploads.[uploadId] <- meta
                    writeChunkMeta meta

                    ("[Chunk] " + uploadId + " #" + chunkIndex.ToString() + 
                     " " + chunkActualSize.ToString() + " bytes (" + 
                     meta.receivedChunks.Count.ToString() + "/" + meta.totalChunks.ToString() + ")")
                    |> green |> output

                    httpx.Response.ContentType <- "application/json; charset=utf-8"
                    do! httpx.Response.WriteAsJsonAsync({| Er = "OK"; chunkIndex = chunkIndex; received = true |})

        with ex ->
            ("[Chunk Error] " + ex.Message) |> red |> output
            httpx.Response.StatusCode <- 500
            do! httpx.Response.WriteAsJsonAsync({| Er = ex.Message |})
    })) |> ignore

    // --- uploadChunk/complete ---
    app.MapPost("/api/{scheme}/uploadChunk/complete", 
        Func<string, HttpContext, Task>(fun scheme httpx -> task {
        try
            if fileHandler.chunkfile__parentFolderId__AsyncJson.IsNone then
                httpx.Response.StatusCode <- 501
                return ()

            let! body = readRequestBodySafe httpx

            let session = tryReadJsonStringProperty "session" body |> fun s -> s.Replace("\"", "")
            match verifySession runtime scheme session with
            | Error code ->
                httpx.Response.StatusCode <- code
                do! httpx.Response.WriteAsJsonAsync({| Er = "Unauthorized" |})
                return ()
            | Ok sessionVerified ->

            let uploadId = tryReadJsonStringProperty "uploadId" body |> fun s -> s.Replace("\"", "")
            let relativePath = tryReadJsonStringProperty "relativePath" body |> fun s -> s.Replace("\"", "")
            let parentFolderId = tryReadJsonInt64Property body "parentFolderId"

            if String.IsNullOrWhiteSpace uploadId then
                httpx.Response.StatusCode <- 400
                do! httpx.Response.WriteAsJsonAsync({| Er = "Missing uploadId" |})
                return ()

            match resolveAndVerifyMeta runtime uploadId sessionVerified scheme with
            | None ->
                httpx.Response.StatusCode <- 404
                do! httpx.Response.WriteAsJsonAsync({| Er = "Upload not found" |})
                return ()
            | Some meta ->
                if meta.receivedChunks.Count <> meta.totalChunks then
                    let missing = 
                        [0 .. meta.totalChunks - 1] 
                        |> List.filter (fun i -> not (meta.receivedChunks.Contains i))
                    httpx.Response.StatusCode <- 409
                    do! httpx.Response.WriteAsJsonAsync(
                        {| Er = "Incomplete"
                           received = meta.receivedChunks.Count
                           total = meta.totalChunks
                           missing = missing |})
                    return ()

                ("[ChunkComplete] merging " + uploadId + " (" + meta.fileName + ")")
                |> green |> output

                let mergedPath = mergeChunks meta
                let completeFn = fileHandler.chunkfile__parentFolderId__AsyncJson.Value
                let! rep = completeFn mergedPath meta.fileName meta.contentType meta.fileSize parentFolderId relativePath

                try Directory.Delete(meta.tempDir, true)
                with _ -> ()
                chunkedUploads.TryRemove(uploadId) |> ignore

                ("[ChunkComplete] done " + uploadId) |> green |> output

                let ary = rep |> Util.Json.json__strFinal
                httpx.Response.ContentType <- "application/json; charset=utf-8"
                do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(jsonBytes ary))

        with ex ->
            ("[ChunkComplete Error] " + ex.Message) |> red |> output
            httpx.Response.StatusCode <- 500
            do! httpx.Response.WriteAsJsonAsync({| Er = ex.Message |})
    })) |> ignore

    // --- uploadChunk/status ---
    app.MapGet("/api/{scheme}/uploadChunk/status", 
        Func<string, HttpContext, Task>(fun scheme httpx -> task {
        try
            let uploadId = 
                if httpx.Request.Query.ContainsKey "uploadId" then
                    httpx.Request.Query.["uploadId"].ToString()
                else ""
            let session = 
                if httpx.Request.Query.ContainsKey "session" then
                    httpx.Request.Query.["session"].ToString()
                else ""

            match verifySession runtime scheme session with
            | Error code ->
                httpx.Response.StatusCode <- code
                do! httpx.Response.WriteAsJsonAsync({| Er = "Unauthorized" |})
                return ()
            | Ok sessionVerified ->

            if String.IsNullOrWhiteSpace uploadId then
                httpx.Response.StatusCode <- 400
                do! httpx.Response.WriteAsJsonAsync({| Er = "Missing uploadId" |})
                return ()

            match resolveAndVerifyMeta runtime uploadId sessionVerified scheme with
            | None ->
                httpx.Response.StatusCode <- 404
                do! httpx.Response.WriteAsJsonAsync({| Er = "Upload not found" |})
                return ()
            | Some meta ->
                let received = meta.receivedChunks |> Seq.toArray |> Array.sort
                let missing = 
                    [0 .. meta.totalChunks - 1] 
                    |> List.filter (fun i -> not (meta.receivedChunks.Contains i))
                httpx.Response.ContentType <- "application/json; charset=utf-8"
                do! httpx.Response.WriteAsJsonAsync(
                    {| Er = "OK"
                       uploadId = uploadId
                       fileName = meta.fileName
                       totalChunks = meta.totalChunks
                       receivedChunks = received
                       missingChunks = missing
                       chunkSize = meta.chunkSize |})

        with ex ->
            ("[ChunkStatus Error] " + ex.Message) |> red |> output
            httpx.Response.StatusCode <- 500
            do! httpx.Response.WriteAsJsonAsync({| Er = ex.Message |})
    })) |> ignore

    // --- uploadChunk/abort ---
    app.MapPost("/api/{scheme}/uploadChunk/abort", 
        Func<string, HttpContext, Task>(fun scheme httpx -> task {
        try
            let! body = readRequestBodySafe httpx

            let session = tryReadJsonStringProperty "session" body |> fun s -> s.Replace("\"", "")
            match verifySession runtime scheme session with
            | Error code ->
                httpx.Response.StatusCode <- code
                do! httpx.Response.WriteAsJsonAsync({| Er = "Unauthorized" |})
                return ()
            | Ok sessionVerified ->

            let uploadId = tryReadJsonStringProperty "uploadId" body |> fun s -> s.Replace("\"", "")
            if String.IsNullOrWhiteSpace uploadId then
                httpx.Response.StatusCode <- 400
                do! httpx.Response.WriteAsJsonAsync({| Er = "Missing uploadId" |})
                return ()

            match resolveAndVerifyMeta runtime uploadId sessionVerified scheme with
            | None ->
                httpx.Response.StatusCode <- 404
                do! httpx.Response.WriteAsJsonAsync({| Er = "Upload not found" |})
                return ()
            | Some meta ->
                try Directory.Delete(meta.tempDir, true)
                with _ -> ()
                chunkedUploads.TryRemove(uploadId) |> ignore
                ("[ChunkAbort] " + uploadId) |> yellow |> output
                httpx.Response.ContentType <- "application/json; charset=utf-8"
                do! httpx.Response.WriteAsJsonAsync({| Er = "OK" |})

        with ex ->
            ("[ChunkAbort Error] " + ex.Message) |> red |> output
            httpx.Response.StatusCode <- 500
            do! httpx.Response.WriteAsJsonAsync({| Er = ex.Message |})
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
