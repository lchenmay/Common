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

    // 处理 OPTIONS 预检请求的兜底
    app.Use(fun (context: HttpContext) (next: RequestDelegate) ->
        if context.Request.Method = "OPTIONS" then
            context.Response.StatusCode <- 204
            Task.CompletedTask
        else 
            next.Invoke(context)) |> ignore    

    // 启用 WebSocket 支持
    app.UseWebSockets() |> ignore

    // --- 路由与功能实现区 ---

    let getClerkIdentity (httpx: HttpContext) =
        let authHeader = httpx.Request.Headers.["Authorization"].ToString()
        if authHeader.StartsWith("Bearer ", StringComparison.OrdinalIgnoreCase) then
            let token = authHeader.Substring(7).Trim()
            // 调用我们之前定义的 Auth.validateClerkToken
            //UtilKestrel.Auth.validateClerkToken token 
            Some token
        else
            None

    // 1.2 GET 型 API 分发
    app.MapGet("/api/{scheme}/{api}",
        Func<string, string, HttpContext, Task>(fun scheme api httpx -> task {
            let x = EchoCtx(runtime,httpx,scheme,api)
            
            apiEngine x

            if x.Struct.contentType.Length > 0 then
                httpx.Response.ContentType <- x.Struct.contentType
            else
                httpx.Response.ContentType <- "application/json; charset=utf-8"

            httpx.Response.Headers.["Content-Security-Policy"] <- ""

            do! httpx.Response.Body.WriteAsync(ReadOnlyMemory(x.Struct.rep))
    })) |> ignore

    // 1.2 POST 型 API 分发
    app.MapPost("/api/{scheme}/{api}",
        Func<string, string, HttpContext, Task>(fun scheme api httpx -> task {
            
            printfn "[Debug] 收到请求: %s/%s, Content-Length: %A" scheme api httpx.Request.ContentLength

            let clerkUserId = getClerkIdentity httpx
            let x = EchoCtx(runtime,httpx,scheme,api)
            match clerkUserId with
            | Some uid -> 
                // 假设你根据 ClerkID 查找或创建本地用户
                // x.Struct.identity <- Some localUser 
                printfn "已截获有效 Clerk 请求，用户 ID: %s" uid
            | None -> 
                // 如果没有 Token，可以记录日志或保持匿名状态
                printfn "匿名请求或无效 Token: %s/%s" scheme api

            apiEngine x

            if x.Struct.contentType.Length > 0 then
                httpx.Response.ContentType <- x.Struct.contentType
            else
                httpx.Response.ContentType <- "application/json; charset=utf-8"

            httpx.Response.Headers.["Content-Security-Policy"] <- ""

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