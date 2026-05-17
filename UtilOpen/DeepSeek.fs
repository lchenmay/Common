module UtilOpen.DeekSeek

open System
open System.Net.Http
open System.Text

open Util.Text
open Util.Json

// 确保使用 System.Net.Http.HttpClient，避免与 Util.HttpClient 冲突
let client = 
    let c = new System.Net.Http.HttpClient()
    c.Timeout <- TimeSpan.FromSeconds 300.0
    c

let private loadTextFromResponse (responseBody: string) =
    let root = responseBody |> str__root
    match tryFindByPath [| "choices"; "0"; "message"; "content" |] root with
    | Some (_, Json.Str txt) -> txt
    | _ -> ""

let private loadErrorFromResponse (responseBody: string) =
    let root = responseBody |> str__root
    match tryFindByPath [| "error"; "message" |] root with
    | Some (_, Json.Str msg) -> msg
    | _ -> ""

let DeepSeekChat (output: string -> unit) apiKey model (msg:string) =
    async {
        try
            let url = "https://api.deepseek.com/v1/chat/completions"
            let payload = sprintf "{\"model\":\"%s\",\"messages\":[{\"role\":\"user\",\"content\":\"%s\"}],\"stream\":false}" model (msg.Replace("\"", "\\\""))
            let content = new StringContent(payload, Encoding.UTF8, "application/json")
            
            client.DefaultRequestHeaders.Remove("Authorization") |> ignore
            client.DefaultRequestHeaders.Add("Authorization", $"Bearer {apiKey}")

            let! response = client.PostAsync(url, content) |> Async.AwaitTask
            let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask

            if response.IsSuccessStatusCode then
                let reply = loadTextFromResponse responseBody
                output "✅ DeepSeek 调用成功"
                return reply
            else
                let errMsg = loadErrorFromResponse responseBody
                output $"❌ DeepSeek 调用失败。状态码: {int response.StatusCode}"
                output $"错误详情: {errMsg}"
                return ""
        with ex ->
            output $"⚠️ DeepSeek API 异常: {ex.Message}"
            return ""
    }