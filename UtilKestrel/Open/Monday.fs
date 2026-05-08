module UtilKestrel.Open.Monday

open System
open System.Net.Http
open System.Text

open Util.Text
open Util.Json
open Util.HttpClient

// 静态客户端定义
let client = 
    let c = new System.Net.Http.HttpClient()
    c.Timeout <- TimeSpan.FromSeconds 300.0
    c

/// 基础 POST 函数：显式标注返回类型以修复 .Content 识别错误
let private postToMonday (apiKey: string) (queryText: string) : HttpResponseMessage =
    client.DefaultRequestHeaders.Remove("Authorization") |> ignore
    client.DefaultRequestHeaders.Add("Authorization", apiKey)
    client.DefaultRequestHeaders.Remove("API-Version") |> ignore
    client.DefaultRequestHeaders.Add("API-Version", "2023-10")
    
    let jsonPayload = sprintf "{\"query\": \"%s\"}" queryText
    let content = new StringContent(jsonPayload, Encoding.UTF8, "application/json")
    
    client.PostAsync("https://api.monday.com/v2", content).Result

// =====================================================================
// 读取接口 (Getters)
// =====================================================================

/// 1. 获取看板列表 (对应 J7 的 Kernel_Pool)
/// 注意：这就是你报错找不到的函数，确保调用处使用 GetPools 或 GetBoards
let GetPools 
    output apiKey =
    let query = "{ boards (limit: 50) { id name } }"
    try
        let resp = postToMonday apiKey query
        let body = resp.Content.ReadAsStringAsync().Result
        "Pools: " + body |> output
    with ex -> "Error: " + ex.Message |> output

/// 为了兼容性，如果你其他地方调用了 GetBoards，可以保留这个别名
let GetBoards 
    output apiKey = 
    GetPools output apiKey

/// 2. 获取用户列表 (对应 J7 的 Ca_EndUser)
let GetUsers 
    output apiKey =
    let query = "{ users { id name email } }"
    try
        let resp = postToMonday apiKey query
        let body = resp.Content.ReadAsStringAsync().Result
        "Users: " + body |> output
    with ex -> "Error: " + ex.Message |> output

/// 3. 获取账单列表 (对应 J7 的 Kernel_UtilBill)
let GetBills 
    output apiKey 
    boardId =
    let query = sprintf "{ boards (ids: [%s]) { items_page { items { id name column_values { id text value } } } } }" boardId
    try
        let resp = postToMonday apiKey query
        let body = resp.Content.ReadAsStringAsync().Result
        "Bills: " + body |> output
    with ex -> "Error: " + ex.Message |> output

// =====================================================================
// 写入与验证接口
// =====================================================================

let VerifyMondayToken 
    output apiKey =
    let query = "{ me { id name email } }"
    try
        let resp = postToMonday apiKey query
        let body = resp.Content.ReadAsStringAsync().Result
        "Success: " + body |> output
    with ex -> "Fail: " + ex.Message |> output

let CreateBillItem 
    output apiKey 
    boardId billName amt =
    let mutation = 
        sprintf "mutation { create_item (board_id: %s, item_name: \\\"%s\\\", column_values: \\\"{\\\\\\\"numbers\\\\\\\": \\\\\\\"%f\\\\\\\"}\\\") { id } }" 
                boardId billName amt
    try
        let resp = postToMonday apiKey mutation
        let body = resp.Content.ReadAsStringAsync().Result
        "Created: " + body |> output
    with ex -> "Error: " + ex.Message |> output