module UtilKestrel.Open.Google

open System
open System.Net.Http
open System.Text

open Util.Text
open Util.Json
open Util.HttpClient


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


let client = new System.Net.Http.HttpClient()

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

            let root = responseBody |> Util.Json.str__root
            match tryFindByPath [| "candidates" |] root with
            | Some (_, Json.Ary items) when items.Length > 0 ->
                match tryFindByPath [| "content"; "parts" |] items[0] with
                | Some (_, Json.Ary parts) when parts.Length > 0 ->
                    return tryFindStrByAtt "text" parts[0]
                | _ -> return ""
            | _ -> return ""
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

/// Gemini 多模态调用函数：支持文本 + (图片/PDF) 混合输入
/// filePath: 本地文件路径
/// mimeType: 例如 "image/jpeg" 或 "application/pdf"
let GeminiMultimodal 
    output apiKey model 
    msg filePath mimeType = 
    async {
        try
            // 1. 读取并转换媒体文件为 Base64
            let base64Data = System.Convert.ToBase64String(System.IO.File.ReadAllBytes(filePath))
            
            // 2. 构造多模态请求对象
            let requestObj = { 
                contents = [ 
                    { 
                        parts = [ 
                            { text = Some msg; inline_data = None }
                            { text = None; inline_data = Some { mime_type = mimeType; data = base64Data } }
                        ] 
                    } 
                ] 
            }
            
            // 3. 序列化 (使用 System.Text.Json)
            // 注意：F# 序列化 Option 类型需要特殊处理或简单的匿名对象
            // 这里为了确保兼容性，建议使用匿名对象进行序列化，避免 Option 标签干扰 JSON 结构
            let serializableObj = {|
                contents = [|
                    {| 
                        parts = [|
                            box {| text = msg |}
                            box {| inline_data = {| mime_type = mimeType; data = base64Data |} |}
                        |] 
                    |}
                |]
            |}
            
            let jsonPayload = System.Text.Json.JsonSerializer.Serialize(serializableObj)
            let content = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
            
            // 4. 定义 URL (使用你已测通的 2.5-flash)
            let url = $"https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent?key={apiKey}"
            
            output "正在连接 Gemini API 并上传媒体数据..."
            
            // 5. 发送请求
            let! response = client.PostAsync(url, content) |> Async.AwaitTask
            let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            
            if response.IsSuccessStatusCode then
                output "✅ 媒体分析成功！"
                // 这里可以根据需要决定是否记录 responseBody
            else
                output $"❌ 媒体分析失败。状态码: {response.StatusCode}"
                output $"错误详情: {responseBody}"
                
            return responseBody
            
        with | ex -> 
            output $"⚠️ 媒体处理发生异常: {ex.Message}"
            return ""
    }