module UtilKestrel.Open.Monday

open System
open System.Net.Http
open System.Text

// 静态客户端定义
let client = 
    let c = new System.Net.Http.HttpClient()
    c.Timeout <- TimeSpan.FromSeconds 300.0
    c

/// 基础 POST 函数：执行 GraphQL 查询并返回原始字符串
let private postToMonday (apiKey: string) (queryText: string) : string option =
    client.DefaultRequestHeaders.Remove("Authorization") |> ignore
    client.DefaultRequestHeaders.Add("Authorization", apiKey)
    client.DefaultRequestHeaders.Remove("API-Version") |> ignore
    client.DefaultRequestHeaders.Add("API-Version", "2023-10")
    
    // 构造标准的 GraphQL JSON 负载
    let jsonPayload = sprintf "{\"query\": \"%s\"}" (queryText.Replace("\"", "\\\"").Replace("\n", ""))
    let content = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
    
    try
        let resp = client.PostAsync("https://api.monday.com/v2", content).Result
        if resp.IsSuccessStatusCode then
            Some (resp.Content.ReadAsStringAsync().Result)
        else None
    with _ -> None

// =====================================================================
// 获取原始数据的接口 (返回原始 JSON 字符串)
// =====================================================================

/// 获取用户原始数据
let GetUsersRaw apiKey =
    let query = "{ users { id name email is_admin enabled } }"
    postToMonday apiKey query

/// 获取看板(队列)原始数据
let GetQueuesRaw apiKey =
    let query = "{ boards (limit: 50) { id name description } }"
    postToMonday apiKey query

/// 获取特定看板下的事项(账单)原始数据
let GetBillsRaw apiKey boardId =
    let query = sprintf "{ boards (ids: [%s]) { items_page { items { id name column_values { id text value } } } } }" boardId
    postToMonday apiKey query

/// 验证 Token 原始接口
let VerifyMondayTokenRaw apiKey =
    let query = "{ me { id name email } }"
    postToMonday apiKey query