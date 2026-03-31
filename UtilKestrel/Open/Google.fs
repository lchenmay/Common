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

let Gemini (logger: string -> unit) apiKey msg = 

    let content = 
        let requestObj = { contents = [ { parts = [ { text = msg } ] } ] }
        let jsonPayload = System.Text.Json.JsonSerializer.Serialize(requestObj)
        new StringContent(jsonPayload, Encoding.UTF8, "application/json")

    async {
        try
            let url = $"https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key={apiKey}"

            logger "正在连接 Gemini API..."
            
            // 2. 将 .NET Task 转换为 F# Async
            let! response = client.PostAsync(url, content) |> Async.AwaitTask
            let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            
            if response.IsSuccessStatusCode then
                logger "✅ 连接成功！"
                logger $"Gemini 应答: {responseBody}"
            else
                // 注意：logger 如果是简单 string -> unit，不支持 printf 占位符，需用插值字符串
                logger $"❌ 连接失败。状态码: {response.StatusCode}"
                logger $"错误详情: {responseBody}"

            return responseBody
        with
        | ex -> 
            logger $"⚠️ 发生异常: {ex.Message}"
            return ""
    }