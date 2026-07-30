module UtilOpen.Google

open System
open System.Net.Http
open System.Text

open Util.Text
open Util.Json
open Util.HttpClient

/// HttpContent that streams a [offset, offset+sliceLen) slice of a file, reporting
/// absolute (offset+written) progress via callback. A per-write CancellationToken
/// (writeTimeout) guarantees a stalled TCP send surfaces as a failure instead of
/// hanging the whole upload forever (P0 fix).
type ProgressHttpContent(filePath: string, mime: string, onProgress: int64 * int64 -> unit,
                         offset: int64, sliceLen: int64, writeTimeout: TimeSpan) as this =
    inherit HttpContent()
    do this.Headers.ContentType <- System.Net.Http.Headers.MediaTypeHeaderValue.Parse(mime)
    override this.SerializeToStreamAsync(stream: System.IO.Stream, _context: System.Net.TransportContext) =
        task {
            let total = (System.IO.FileInfo filePath).Length
            use fs = new System.IO.FileStream(filePath, System.IO.FileMode.Open, System.IO.FileAccess.Read,
                                               System.IO.FileShare.ReadWrite, 81920, true)
            fs.Seek(offset, System.IO.SeekOrigin.Begin) |> ignore
            // 写超时：底层网络写卡死时，必须在 writeTimeout 内抛 OperationCanceledException，
            // 进而让 SendAsync 失败，而不是无限阻塞在 TCP 重传上。
            use wtCts = new System.Threading.CancellationTokenSource(writeTimeout)
            let buf = Array.zeroCreate<byte> 65536
            let mutable remaining = sliceLen
            let mutable doneBytes = offset
            while remaining > 0L do
                let toRead = min (int64 buf.Length) remaining |> int
                let! n = fs.ReadAsync(buf, 0, toRead, wtCts.Token)
                if n = 0 then
                    remaining <- 0L   // 文件比预期短，提前结束切片
                else
                    do! stream.WriteAsync(buf, 0, n, wtCts.Token)
                    remaining <- remaining - int64 n
                    doneBytes <- doneBytes + int64 n
                    onProgress(doneBytes, total)
            do! stream.FlushAsync(wtCts.Token)
            onProgress(doneBytes, total)
        } :> System.Threading.Tasks.Task
    override this.TryComputeLength(length: byref<int64>) =
        length <- sliceLen
        true


let requestAccessToken(client_id:string[],client_sceret,redirect_url) code = 

        let hc = empty__HttpClient()

        let postdata = 
            [|  "client_id=" + client_id.[0];
                "&client_secret=" + client_sceret;
                "&grant_type=authorization_code";
                "&code=" + code;
                "&redirect_uri=" + redirect_url |]
            |> linesConcat

        let fields = 
            let res = hc.post("https://oauth2.googleapis.com/token",postdata).html
            res |> jsonstr__items

        let id_token = Util.Json.checkfield(fields)("id_token")
        //let access_token = Util.Json.checkfield(fields)("access_token")
        //https://www.googleapis.com/oauth2/v3/certs
        //access_token

        //Util.JWT.validate(id_token, "https://accounts.google.com", client_id))
        id_token    

//let requestUserInfo access_token = 

    //let verified,uid, email, avatar, fn, ln, name, lang, jwts, nickname,iss,aud,email_verified = 
    //    validate(id_token, "https://accounts.google.com", client_ids)
    //if verified = "verified" then
    //    uid, email, avatar, fn, ln, name, lang, jwts,uid
    //else
    //    "","","","","","","","",""

let translate apiKey src dst txt = 

    let hc = empty__HttpClient()
    
    let url = 
        [|  "https://translation.googleapis.com/language/translate/v2?key="
            apiKey |]
        |> String.Concat

    let postdata = 
        [|  "target=" + dst
            "&q=" + Util.Json.encode(txt) |]
        |> String.Concat

    let html = hc.post(url,postdata).html

    let mutable res = ""

    let mutable json = html |> Util.Json.str__root
    match tryFindByPath [| "data";"translations" |] json with
    | Some (n,j) -> 
        match j with
        | Json.Ary items -> 
            if items.Length = 1 then
                res <- 
                    tryFindStrByAtt "translatedText" items[0]
                    |> Util.Json.decode
        | _ -> ()
    | None -> ()

    (*
{
  "data": {
    "translations": [
      {
        "translatedText": "Chinese translation test",
        "detectedSourceLanguage": "zh-CN"
      }
    ]
  }
}
    
    *)

    res


let client = 
    let client = new System.Net.Http.HttpClient()
    client.Timeout <- TimeSpan.FromSeconds 300.0
    client

// 定义符合 Gemini API 格式的类型结构
type Part = { text: string }
type Content = { parts: Part list }
type GeminiRequest = { contents: Content list }

let GeminiListModels output apiKey =
    let url = $"https://generativelanguage.googleapis.com/v1beta/models?key={apiKey}"
    use client = new System.Net.Http.HttpClient()
    try
        let response = client.GetStringAsync(url).Result
        "你可用的模型列表: " + response |> output
    with ex -> 
        "无法获取列表: " + ex.Message.ToString() |> output


let loadTextFromRep responseBody = 
    let root = responseBody |> Util.Json.str__root
    match tryFindByPath [| "candidates" |] root with
    | Some (_, Json.Ary items) when items.Length > 0 ->
        match tryFindByPath [| "content"; "parts" |] items[0] with
        | Some (_, Json.Ary parts) when parts.Length > 0 ->
            tryFindStrByAtt "text" parts[0]
        | _ -> ""
    | _ -> ""

let loadErFromRep responseBody = 
    let root = responseBody |> Util.Json.str__root
    match tryFindByPath [| "error"; "message" |] root with
    | Some (s,j) ->
        match j with
        | Json.Str txt -> txt
        | _ -> ""
    | _ -> ""


// 在调用 Gemini 之前先跑一下这个：
// listModels "你的API_KEY" |> Async.RunSynchronously |> ignore

let GeminiChat 
    output apiKey model 
    msg = 

    let content = 
        let requestObj = { contents = [ { parts = [ { text = msg } ] } ] }
        let jsonPayload = System.Text.Json.JsonSerializer.Serialize(requestObj)
        new StringContent(jsonPayload, Encoding.UTF8, "application/json")

    async {
        try
            let url = $"https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent?key={apiKey}"
            output "正在连接 Gemini API..."
            
            // 2. 将 .NET Task 转换为 F# Async
            let! response = client.PostAsync(url, content) |> Async.AwaitTask
            let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            
            if response.IsSuccessStatusCode then
                output "✅ 连接成功！"
                output $"Gemini 应答: {responseBody}"
            else
                // 注意：logger 如果是简单 string -> unit，不支持 printf 占位符，需用插值字符串
                output $"❌ 连接失败。状态码: {response.StatusCode}"
                output $"错误详情: {responseBody}"

            return loadTextFromRep responseBody
        with
        | ex -> 
            output $"⚠️ 发生异常: {ex.Message}"
            return ""
    }

// 扩展原有类型以支持多模态数据
type InlineData = { mime_type: string; data: string }
type PartMulti = { text: string option; inline_data: InlineData option }
type ContentMulti = { parts: PartMulti list }
type GeminiMultiRequest = { contents: ContentMulti list }

/// 使用 Gemini File API 可恢复上传协议，将单个文件【真分块】上传到 Google。
/// 不把文件读进内存，从根本上规避大文件导致的 OutOfMemoryException。
/// 真分块：每块独立 POST 请求，末段被网络掐断只影响单块；块内进度停滞超 600s（10 分钟）
/// 由看门狗强制关闭连接暴露失败，失败块通过 query 查询已收字节数续传；本地放弃前再做一次
/// GET upload URL 直接查服务端文件元数据（复活机制），修复"本地慢但服务端已收完"误判失败。
/// 返回 (uri, name, state)。任何失败返回 ("", "", "")。
let GeminiUploadFile output apiKey (path: string) (mime: string) (onProgress: int64 * int64 -> unit) =
    async {
        try
            let len = (System.IO.FileInfo path).Length
            let startUrl = $"https://generativelanguage.googleapis.com/upload/v1beta/files?key={apiKey}"
            use startClient = new System.Net.Http.HttpClient()
            startClient.Timeout <- System.TimeSpan.FromSeconds 120.0
            let startReq = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Post, startUrl)
            startReq.Headers.Add("X-Goog-Upload-Protocol", "resumable")
            startReq.Headers.Add("X-Goog-Upload-Command", "start")
            startReq.Headers.Add("X-Goog-Upload-Header-Content-Length", len.ToString())
            startReq.Headers.Add("X-Goog-Upload-Header-Content-Type", mime)
            startReq.Content <- new StringContent(
                $"{{\"file\":{{\"display_name\":\"{System.IO.Path.GetFileName path}\",\"mimeType\":\"{mime}\"}}}}",
                System.Text.Encoding.UTF8, "application/json")
            let! startResp = startClient.SendAsync startReq |> Async.AwaitTask
            if not startResp.IsSuccessStatusCode then
                let! eb = startResp.Content.ReadAsStringAsync() |> Async.AwaitTask
                output $"⚠️ GeminiUploadFile 申请上传会话失败: {startResp.StatusCode} {eb}"
                return ("", "", "")
            else
                let hasUrl, values = startResp.Headers.TryGetValues("x-goog-upload-url")
                if not hasUrl || values |> Seq.isEmpty then
                    output "⚠️ GeminiUploadFile 未返回上传 URL"
                    return ("", "", "")
                else
                    let u = values |> Seq.head
                    // 查询 resumable 会话已确认接收的字节数，用于断点续传
                    let queryReceived (uploadUrl: string) : int64 =
                        try
                            use qc = new System.Net.Http.HttpClient()
                            qc.Timeout <- System.TimeSpan.FromSeconds 30.0
                            let qreq = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Post, uploadUrl)
                            qreq.Headers.Add("X-Goog-Upload-Command", "query")
                            let resp = qc.SendAsync(qreq).Result
                            let hdr = resp.Headers.TryGetValues("X-Goog-Upload-Size-Received")
                            if fst hdr then
                                match (snd hdr |> Seq.tryHead |> Option.defaultValue "0") |> System.Int64.TryParse with
                                | true, v -> v
                                | _ -> 0L
                            else 0L
                        with _ -> 0L
                    use upClient = new System.Net.Http.HttpClient()
                    // 注意：HttpClient.Timeout 不计入"请求体发送阶段"，卡在写请求体时它不触发；
                    // 故不依赖它，改用块内进度看门狗 + CancelPendingRequests 强制断连（真正打断 socket 写阻塞）。
                    upClient.Timeout <- System.TimeSpan.FromSeconds 1200.0
                    // 真分块上传：每块 4 MiB 独立请求、独立超时、失败可续传
                    let chunkSize = 4L * 1024L * 1024L
                    let maxAttempts = 3
                    // 块内进度停滞超过该时长即判定网络卡死，强制取消并关闭连接。
                    // 设为 600s（10 分钟）以容忍本地直连 Google 的真实慢网络；死网络的真正兜底是放弃前的"复活"查询。
                    let blockStallTimeout = System.TimeSpan.FromSeconds 600.0
                    let mutable offset = 0L
                    let mutable finalBody = ""
                    let mutable gaveUp = false
                    while offset < len && not gaveUp do
                        let isLast = offset + chunkSize >= len
                        let sliceLen = min chunkSize (len - offset)
                        let mutable attempt = 0
                        let mutable ok = false
                        let mutable body = ""
                        let mutable st = ""
                        while attempt < maxAttempts && not ok do
                            attempt <- attempt + 1
                            // 块级取消令牌 + 进度看门狗：停滞超时即取消并强制关闭连接
                            let blockCts = new System.Threading.CancellationTokenSource()
                            let lastTick = ref System.DateTime.UtcNow
                            let wd = new System.Threading.Timer(
                                (fun _ ->
                                    if (System.DateTime.UtcNow - !lastTick) > blockStallTimeout then
                                        try blockCts.Cancel() with _ -> ()
                                        try upClient.CancelPendingRequests() with _ -> ()),
                                null, 1000, 1000)
                            try
                                use upReq = new System.Net.Http.HttpRequestMessage(System.Net.Http.HttpMethod.Post, u)
                                upReq.Headers.Add("X-Goog-Upload-Command",
                                                  if isLast then "upload, finalize" else "upload")
                                upReq.Headers.Add("X-Goog-Upload-Offset", offset.ToString())
                                // 每次写进度都刷新 lastTick，让看门狗感知"仍在推进"
                                let onProg (doneB, totalB) =
                                    lastTick := System.DateTime.UtcNow
                                    onProgress(doneB, totalB)
                                upReq.Content <- new ProgressHttpContent(
                                    path, mime, onProg, offset, sliceLen,
                                    System.TimeSpan.FromSeconds 30.0)
                                let! upResp = upClient.SendAsync(upReq, blockCts.Token) |> Async.AwaitTask
                                let! b = upResp.Content.ReadAsStringAsync() |> Async.AwaitTask
                                let stOpt = upResp.Headers.TryGetValues("X-Goog-Upload-Status")
                                let status = if fst stOpt
                                             then (snd stOpt |> Seq.tryHead |> Option.defaultValue "")
                                             else ""
                                if upResp.IsSuccessStatusCode then
                                    ok <- true; body <- b; st <- status
                                else
                                    output $"分块上传失败 (offset={offset}, 第{attempt}次): {upResp.StatusCode} {b}"
                                    let received = queryReceived u
                                    if received > offset then offset <- received
                            with ex ->
                                output $"分块上传异常 (offset={offset}, 第{attempt}次): {ex.Message}"
                                let received = queryReceived u
                                if received > offset then offset <- received
                            wd.Dispose()
                            blockCts.Dispose()
                        if not ok then
                            output $"GeminiUploadFile 分块上传在 offset={offset} 处放弃（已重试 {maxAttempts} 次）"
                            gaveUp <- true
                        else
                            offset <- offset + sliceLen
                            if isLast || st = "final" then finalBody <- body
                    // 放弃前"复活"：本地超时但 Google 服务端可能已接收完 finalize，
                    // GET upload URL 直接拿文件元数据（修复"网络慢但已传完"误判失败）
                    let mutable resurrectedUri = ""
                    let mutable resurrectedName = ""
                    let mutable resurrectedState = ""
                    if gaveUp then
                        try
                            use gc = new System.Net.Http.HttpClient()
                            gc.Timeout <- System.TimeSpan.FromSeconds 30.0
                            let gResp = gc.GetAsync(u).Result
                            if gResp.IsSuccessStatusCode then
                                let gBody = gResp.Content.ReadAsStringAsync().Result
                                let root = gBody |> Util.Json.str__root
                                resurrectedUri   <- tryFindByPath [| "file"; "uri"   |] root |> function Some (_, Json.Str s) -> s | _ -> ""
                                resurrectedName  <- tryFindByPath [| "file"; "name"  |] root |> function Some (_, Json.Str s) -> s | _ -> ""
                                resurrectedState <- tryFindByPath [| "file"; "state" |] root |> function Some (_, Json.Str s) -> s | _ -> ""
                                if resurrectedUri <> "" && resurrectedName <> "" then
                                    output $"复活成功：服务端已接收文件 {System.IO.Path.GetFileName path} ({resurrectedState})"
                        with _ -> ()
                    if resurrectedUri <> "" && resurrectedName <> "" then
                        return (resurrectedUri, resurrectedName, resurrectedState)
                    elif gaveUp then return ("", "", "")
                    else
                        let root = finalBody |> Util.Json.str__root
                        let uri   = tryFindByPath [| "file"; "uri"   |] root |> function Some (_, Json.Str s) -> s | _ -> ""
                        let name  = tryFindByPath [| "file"; "name"  |] root |> function Some (_, Json.Str s) -> s | _ -> ""
                        let state = tryFindByPath [| "file"; "state" |] root |> function Some (_, Json.Str s) -> s | _ -> ""
                        output $"已上传文件 {System.IO.Path.GetFileName path} -> {state}"
                        return (uri, name, state)
        with ex ->
            output $"⚠️ GeminiUploadFile 异常: {ex.Message}"
            return ("", "", "")
    }

/// 大文件上传后 Gemini 可能先返回 state=PROCESSING，需轮询直到 ACTIVE 才能在
/// generateContent 中引用。超时（120s）或失败则返回原 uri（尽力而为）。
let waitFileActive output apiKey (name: string) (uri: string) (onWait: int64 -> unit) =
    async {
        if System.String.IsNullOrEmpty name then return uri
        else
            let url = $"https://generativelanguage.googleapis.com/v1beta/{name}?key={apiKey}"
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let mutable active = false
            while not active && sw.Elapsed.TotalSeconds < 120.0 do
                do! Async.Sleep 2000
                onWait (int64 sw.Elapsed.TotalSeconds)
                try
                    use c = new System.Net.Http.HttpClient()
                    c.Timeout <- System.TimeSpan.FromSeconds 30.0
                    let! body = c.GetStringAsync(url) |> Async.AwaitTask
                    let root = body |> Util.Json.str__root
                    let state = tryFindByPath [| "state" |] root |> function Some (_, Json.Str s) -> s | _ -> ""
                    if state = "ACTIVE" then active <- true
                with _ -> ()
            if not active then output $"⚠️ waitFileActive 超时: {name} 未在 120s 内变为 ACTIVE"
            return uri
    }

/// Gemini 多模态调用函数：支持文本 + 多个媒体文件 (图片/PDF) 混合输入。
/// 采用 Gemini File API：文件先流式上传（不进内存），generateContent 仅引用
/// file_data.uri，从而彻底消除内联 base64 / 全量 JSON 拼装带来的 OOM 风险，
/// 可稳定处理 1~100MB 级别的大文件。
/// output: 日志输出函数 (string -> unit)
/// model: 模型名称，如 "gemini-2.5-flash"
/// msg: 提示词文本
let GeminiMultimodal 
    output apiKey model 
    msg (files: string[])
    (onFileProgress: int * string * int64 * int64 -> unit)
    (onPhase: string -> unit) = 
    async {
        try
            // 1. 逐文件流式上传到 Gemini File API（并行），仅保留上传成功的
            let! uploaded =
                files
                |> Array.mapi (fun idx path ->
                    async {
                        let mime = path |> Util.FileSys.filename__mime output
                        let fileName = System.IO.Path.GetFileName path
                        let! (uri, name, _) = 
                            GeminiUploadFile output apiKey path mime 
                                (fun (doneBytes, totalBytes) ->
                                    onFileProgress(idx, fileName, doneBytes, totalBytes))
                        let fileLabel = "file_" + (idx + 1).ToString() + "_of_" + files.Length.ToString()
                        if name <> "" then
                            onPhase ("waiting_" + fileLabel)
                        let! finalUri = waitFileActive output apiKey name uri (fun waitedSecs ->
                            onPhase ("waiting_" + fileLabel + "_" + waitedSecs.ToString() + "s"))
                        return (mime, finalUri)
                    })
                |> Async.Parallel

            let valid = uploaded |> Array.filter (fun (_, uri) -> uri <> "")

            if valid.Length = 0 then
                output "⚠️ 所有文件上传到 Gemini 失败，跳过 AI 分析。"
                return ("所有文件上传到 Gemini 失败，请稍后重试或手动填写。", "")
            else
                output $"已成功上传 {valid.Length}/{files.Length} 个文件，正在请求 {model} 分析..."
                onPhase "analyzing"

                // 用 file_data uri 引用（无内联 base64，内存占用恒定）
                let parts =
                    [|  box {| text = msg |} |]
                    |> Array.append
                        (valid |> Array.map (fun (mime, uri) ->
                            box {| file_data = {| mime_type = mime; file_uri = uri |} |}))

                // 构造完整的请求对象（仅含 uri 引用，不内联文件内容）
                let serializableObj = {|
                    contents = [|
                        {| parts = parts |}
                    |]
                |}
                
                let jsonPayload = System.Text.Json.JsonSerializer.Serialize(serializableObj)
                let content = new StringContent(jsonPayload, System.Text.Encoding.UTF8, "application/json")
                
                // 构建 URL 并发送
                let url = $"https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent?key={apiKey}"
                
                let! response = client.PostAsync(url, content) |> Async.AwaitTask
                let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                
                if response.IsSuccessStatusCode then
                    output "✅ 多文件分析成功。"
                    return ("", loadTextFromRep responseBody)
                else
                    let code = response.StatusCode |> int
                    let msg = loadErFromRep responseBody
                    output $"❌ 接口返回错误。状态码: {code}"
                    output $"详情: {msg}"
                    
                    return (code.ToString() + ": " + msg, "")
            
        with | ex -> 
            output $"⚠️ GeminiMultimodal 发生异常: {ex.Message}"
            return (ex.Message, "")
    }