module UtilOpen.Resend

open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

open Util.Json
open Util.Text

// ---- 配置 ----

type ResendCfg = {
    apiKey: string
    from: string  // 发件人，如 "iDeal <noreply@whatsyourideal.com>"
}

let empty__ResendCfg() = {
    apiKey = ""
    from = "noreply@whatsyourideal.com"
}

let client =
    let c = new System.Net.Http.HttpClient()
    c.Timeout <- TimeSpan.FromSeconds 30.0
    c

// ---- 内部类型 ----

[<CLIMutable>]
type private ResendPayload = {
    from: string
    ``to``: string array
    subject: string
    html: string
}

let private jo = JsonSerializerOptions()
jo.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
jo.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull

// ---- 核心 API ----

/// Resend 发送一封邮件。返回 (ok, id-or-error)
let send (cfg:ResendCfg) (subject:string) (htmlBody:string) (toEmail:string) =
    async {
        if String.IsNullOrWhiteSpace cfg.apiKey then
            return false, "API Key 未配置"
        else
            try
                let url = "https://api.resend.com/emails"
                let payload = { from = cfg.from; ``to`` = [| toEmail |]; subject = subject; html = htmlBody }
                let json = JsonSerializer.Serialize(payload, jo)
                
                client.DefaultRequestHeaders.Remove("Authorization") |> ignore
                client.DefaultRequestHeaders.Add("Authorization", "Bearer " + cfg.apiKey)
                
                let content = new StringContent(json, Encoding.UTF8, "application/json")
                let! response = client.PostAsync(url, content) |> Async.AwaitTask
                let! body = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                
                if response.IsSuccessStatusCode then
                    let root = body |> str__root
                    match tryFindByPath [|"id"|] root with
                    | Some (_, Json.Str id) -> return true, id
                    | _ -> return true, "OK"
                else
                    let root = body |> str__root
                    let err =
                        match tryFindByPath [|"message"|] root with
                        | Some (_, Json.Str msg) -> msg
                        | _ -> body
                    return false, err
            with ex ->
                return false, ex.Message
    }

/// 发送纯文本邮件
let sendText cfg (subject:string) (textBody:string) (toEmail:string) =
    let html = "<div style=\"white-space:pre-wrap;font-family:Arial,sans-serif\">" + textBody.Replace("\n", "<br>") + "</div>"
    send cfg subject html toEmail
