module UtilKestrel.Open.Monday

open System
open System.Net.Http
open System.Text

// 静态客户端定义
let client = 
    let c = new System.Net.Http.HttpClient()
    c.Timeout <- TimeSpan.FromSeconds 300.0
    c


/// 基础 POST 函数：执行 GraphQL 查询，返回原始 JSON 字符串（仅在无错误时返回 Some）
let private postToMonday 
    output apiKey 
    queryText =

    client.DefaultRequestHeaders.Remove("Authorization") |> ignore
    client.DefaultRequestHeaders.Add("Authorization", "" + apiKey) |> ignore
    client.DefaultRequestHeaders.Remove("API-Version") |> ignore
    client.DefaultRequestHeaders.Add("API-Version", "2023-10") |> ignore

    let payload = System.Text.Json.JsonSerializer.Serialize({| query = queryText |})
    let content = new StringContent(payload, Encoding.UTF8, "application/json")

    try
        let resp = client.PostAsync("https://api.monday.com/v2", content).Result
        if resp.IsSuccessStatusCode then
            let json = resp.Content.ReadAsStringAsync().Result
            // 检查 GraphQL 层错误（Monday 通常返回 200 但带 errors 字段）
            if json.Contains("\"errors\"") then
                $"GraphQL 错误响应: {json}" |> output
                None
            else
                Some json
        else
            let errorBody = resp.Content.ReadAsStringAsync().Result
            $"HTTP Error: {(int resp.StatusCode)} - {errorBody}" |> output
            None
    with ex ->
        $"请求异常: {ex.Message}" |> output
        None
        
        
 // =====================================================================
// 获取原始数据的接口 (返回原始 JSON 字符串)
// =====================================================================

/// 获取用户原始数据
let GetUsersRaw 
    output apikey =
    
    "{ users { id name email is_admin enabled } }"
    |> postToMonday output apikey

let GetBoardItemsRaw 
    output apikey 
    id =

    $"
{{
  boards(ids: [{id}]) {{
    items_page(limit: 100) {{
      items {{
        id
        name
        column_values {{
          id
          text
          ... on StatusValue {{ label }}
          ... on NumbersValue {{ value }}
          ... on DateValue {{ date }}
        }}
      }}
    }}
  }}
}}"
    |> postToMonday output apikey

/// 获取当前账户下所有 Boards 的原始 JSON 字符串
let GetBoardsRaw 
    output apikey =

    """
{
  boards(limit: 200) {
    id
    name
    description
    type
    owner {
      id
      name
    }
    updated_at
    workspace_id
    state
    columns {
      id
      title
      type
    }
  }
}
"""
    |> postToMonday output apikey

/// 获取看板(队列)原始数据
let GetQueuesRaw output apikey =
    let query = "{ boards (limit: 50) { id name description } }"
    postToMonday output apikey query

/// 获取特定看板下的事项(账单)原始数据
let GetBillsRaw output apikey boardId =
    let query = sprintf "{ boards (ids: [%s]) { items_page { items { id name column_values { id text value } } } } }" boardId
    postToMonday output apikey query

/// 验证 Token 原始接口
let VerifyMondayTokenRaw output apikey =
    let query = "{ me { id name email } }"
    postToMonday output apikey query