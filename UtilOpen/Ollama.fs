module UtilOpen.Ollama

open System
open System.Net.Http
open System.Text
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Http

open Util.Json
open Util.Text

open jCQT.AI.LLM


// ---- 配置 ----

type OllamaCfg = {
    url: string
    enabled: bool
}

let empty__OllamaCfg() = {
    url = "http://127.0.0.1:11434"
    enabled = false
}

let client = 
    let c = new System.Net.Http.HttpClient()
    c.Timeout <- TimeSpan.FromSeconds 300.0
    c


// ---- 核心 API ----

/// 列出所有模型
let listModels cfg =
    async {
        if not cfg.enabled then 
            return "[]"
        else
            try
                let! res = client.GetStringAsync(cfg.url + "/api/tags") |> Async.AwaitTask
                return res
            with ex ->
                return "{\"error\":\"" + ex.Message + "\"}"
    }


/// OpenAI 兼容 chat completions（流式）
let chatStream cfg model (messages: string) output =
    async {
        if not cfg.enabled then
            output "data: {\"error\":\"Ollama disabled\"}\n\ndata: [DONE]\n"
            return ()
        try
            let payload = "{\"model\":\"" + model + "\",\"messages\":" + messages + ",\"stream\":true}"
            let req = new HttpRequestMessage(HttpMethod.Post, cfg.url + "/v1/chat/completions")
            req.Content <- new StringContent(payload, Encoding.UTF8, "application/json")
            let! res = client.SendAsync(req, HttpCompletionOption.ResponseHeadersRead) |> Async.AwaitTask
            use! stream = res.Content.ReadAsStreamAsync() |> Async.AwaitTask
            use reader = new StreamReader(stream)
            let mutable line = reader.ReadLine()
            while line <> null do
                if line.StartsWith("data: ") then output (line + "\n")
                line <- reader.ReadLine()
        with ex ->
            output ("data: {\"error\":\"" + ex.Message + "\"}\n\ndata: [DONE]\n")
    }


/// OpenAI 兼容 chat completions（非流式，返回完整 JSON）
let chatSync cfg model (messages: string) =
    async {
        if not cfg.enabled then 
            return "{\"error\":\"Ollama disabled\"}"
        else
            try
                let payload = "{\"model\":\"" + model + "\",\"messages\":" + messages + ",\"stream\":false}"
                let content = new StringContent(payload, Encoding.UTF8, "application/json")
                let! res = client.PostAsync(cfg.url + "/v1/chat/completions", content) |> Async.AwaitTask
                let! body = res.Content.ReadAsStringAsync() |> Async.AwaitTask
                return body
            with ex ->
                return "{\"error\":\"" + ex.Message + "\"}"
    }


/// OpenAI 兼容 embeddings
let embeddings cfg model (input: string) =
    async {
        if not cfg.enabled then 
            return "{\"error\":\"Ollama disabled\"}"
        else
            try
                let payload = "{\"model\":\"" + model + "\",\"input\":\"" + encode input + "\"}"
                let content = new StringContent(payload, Encoding.UTF8, "application/json")
                let! res = client.PostAsync(cfg.url + "/v1/embeddings", content) |> Async.AwaitTask
                let! body = res.Content.ReadAsStringAsync() |> Async.AwaitTask
                return body
            with ex ->
                return "{\"error\":\"" + ex.Message + "\"}"
    }


// ---- API Handler（供 Web 路由调用） ----

/// 处理 ollama API 请求，返回 (string * Json)[] 供 wrapOk/er 包装
let handleApi (json: Json) =
    let cfg = { empty__OllamaCfg() with url = "http://127.0.0.1:11434"; enabled = true }
    let act = json |> tryFindStrByAtt "act"

    match act with
    | "list" ->
        let raw = listModels cfg |> Async.RunSynchronously
        [| "models", Json.Str raw |]

    | "scenario" ->
        let scenarios =
            jCQT.AI.Model.allScenarios()
            |> Seq.map (fun s ->
                let model = jCQT.AI.Model.modelName s
                let label = s.ToString()
                [|  "id", Json.Str label
                    "label", Json.Str label
                    "model", Json.Str model |]
                |> Json.Braket
            )
            |> Array.ofSeq
            |> Json.Ary
        [| "scenarios", scenarios |]

    | "msg" ->
        let model = json |> tryFindStrByAtt "model"
        let message = json |> tryFindStrByAtt "message"
        let sessionId = json |> tryFindStrByAtt "sessionId"

        let mgr = empty__SessionManager()
        mgr.current <- loadSession sessionId
        addMessage mgr { empty__Message() with role = User; content = message }

        let msgs =
            buildMessages mgr
            |> Seq.map (fun m ->
                [|  "role", Json.Str (m.role.ToString().ToLower())
                    "content", Json.Str (encode m.content) |]
                |> Json.Braket
            )
            |> Array.ofSeq
            |> Json.Ary

        let msgsStr = msgs |> json__strFinal
        let raw = chatSync cfg model msgsStr |> Async.RunSynchronously

        addMessage mgr { empty__Message() with role = Assistant; content = raw }
        saveSession mgr.current
        updateProfile mgr

        [| "reply", Json.Str raw |]

    | _ -> [| "error", Json.Str "unknown act" |]
