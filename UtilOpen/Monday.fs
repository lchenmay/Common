module UtilOpen.Monday

open System
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Util.Json


/// 基础 POST 函数：执行 GraphQL 查询，返回原始 JSON 字符串（仅在无错误时返回 Some）
let postToMonday 
    output apiKey 
    queryText =

    try
        // 诊断：输出 apiKey 前缀
        let keyPreview = 
            if String.IsNullOrWhiteSpace apiKey then "[空]"
            elif apiKey.Length > 10 then apiKey.Substring(0, 10) + "..."
            else apiKey
        $"[postToMonday] apiKey 前缀: {keyPreview}" |> output

        use client = new HttpClient()
        client.Timeout <- TimeSpan.FromSeconds 300.0

        use req = new HttpRequestMessage(HttpMethod.Post, "https://api.monday.com/v2")

        // 同时尝试 Bearer 和原始值两种格式进行诊断
        if not (String.IsNullOrWhiteSpace apiKey) then
            req.Headers.Authorization <- new AuthenticationHeaderValue("Bearer", apiKey)
        else
            "⚠️ [postToMonday] apiKey 为空！请检查 CONFIG 表 ApiKeyMonday" |> output

        req.Headers.Add("API-Version", "2023-10")

        let payload = System.Text.Json.JsonSerializer.Serialize({| query = queryText |})
        req.Content <- new StringContent(payload, Encoding.UTF8, "application/json")

        let resp = client.Send(req)
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
        $"请求异常: {ex.Message}\n{ex.StackTrace}" |> output
        None
        
        
 // =====================================================================
// 获取原始数据的接口 (返回原始 JSON 字符串)
// =====================================================================

/// 获取用户原始数据
let GetUsersRaw 
    output apikey =
    
    "{ users { id name email is_admin enabled } }"
    |> postToMonday output apikey

/// 获取单个 Board 的 Items（单页，最多 100 条）
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

/// 获取单个 Board 的所有 Items（自动分页，获取全部数据）
let GetAllBoardItemsRaw
    output apikey
    (boardId: int64) =

    let rec fetch cursor (allItems: Json[]) =
        let cursorParam = 
            match cursor with
            | Some c -> $", cursor: \"{c}\""
            | None -> ""
        
        let query = $"
{{
  boards(ids: [{boardId}]) {{
    items_page(limit: 100{cursorParam}) {{
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
      cursor
    }}
  }}
}}"
        
        match postToMonday output apikey query with
        | Some json ->
            // 调试输出：打印原始 JSON 前 500 字符
            $"原始 JSON 前 500 字符: {json.Substring(0, Math.Min(500, json.Length))}" |> output
            
            try
                let root = Util.Json.str__root json
                
                // 检查 GraphQL 错误
                match root with
                | Json.Braket items ->
                    match items |> Array.tryFind (fun (key, _) -> key = "errors") with
                    | Some (_, errorNode) ->
                        output $"GraphQL 错误: {errorNode}"
                        allItems
                    | None ->
                        // 解析 data.boards[0].items_page
                        match items |> Array.tryFind (fun (key, _) -> key = "data") with
                        | Some (_, dataNode) ->
                            match dataNode with
                            | Json.Braket dataItems ->
                                match dataItems |> Array.tryFind (fun (key, _) -> key = "boards") with
                                | Some (_, boardsNode) ->
                                    match boardsNode with
                                    | Json.Ary boardsAry ->
                                        if Array.isEmpty boardsAry then
                                            output $"[分页] boards 数组为空"
                                            allItems
                                        else
                                            let boardNode = boardsAry.[0]
                                            match boardNode with
                                            | Json.Braket boardItems ->
                                                match boardItems |> Array.tryFind (fun (key, _) -> key = "items_page") with
                                                | Some (_, itemsPageNode) ->
                                                    match itemsPageNode with
                                                    | Json.Braket itemsPageItems ->
                                                        let itemsAry = 
                                                            match itemsPageItems |> Array.tryFind (fun (key, _) -> key = "items") with
                                                            | Some (_, Json.Ary items) -> items
                                                            | _ -> [||]
                                                        
                                                        let nextCursor = 
                                                            match itemsPageItems |> Array.tryFind (fun (key, _) -> key = "cursor") with
                                                            | Some (_, Json.Str c) -> c
                                                            | _ -> ""
                                                        
                                                        let newItems = Array.append allItems itemsAry
                                                        
                                                        if nextCursor <> "" then
                                                            $"[分页] 已获取 {itemsAry.Length} 条，继续获取下一页，cursor={nextCursor}" |> output
                                                            fetch (Some nextCursor) newItems
                                                        else
                                                            $"[分页] 完成，共获取 {newItems.Length} 条记录" |> output
                                                            newItems
                                                    | _ -> 
                                                        output $"[分页] items_page 不是对象"
                                                        allItems
                                                | None ->
                                                    output $"[分页] 未找到 items_page"
                                                    allItems
                                            | _ ->
                                                output $"[分页] boardNode 不是对象"
                                                allItems
                                    | _ ->
                                        output $"[分页] boards 不是数组"
                                        allItems
                                | None ->
                                    output $"[分页] 未找到 boards"
                                    allItems
                            | _ ->
                                output $"[分页] data 不是对象"
                                allItems
                        | None ->
                            output $"[分页] 未找到 data"
                            allItems
                | _ ->
                    output $"[分页] root 不是对象"
                    allItems
            with ex ->
                output $"[分页] 解析失败: {ex.Message}"
                allItems
        | None ->
            output $"[分页] 请求失败"
            allItems
    
    fetch None [||]


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

/// 诊断：尝试两种 Authorization 格式并对比结果
let VerifyMondayConnection output apikey =
    let apiUrl = "https://api.monday.com/v2"
    let query = System.Text.Json.JsonSerializer.Serialize({| query = "{ me { id name email account { id name } } }" |})
    
    let tryAuth (label: string) (setAuth: HttpRequestMessage -> unit) =
        try
            use client = new HttpClient(Timeout = TimeSpan.FromSeconds 15.0)
            use req = new HttpRequestMessage(HttpMethod.Post, apiUrl)
            setAuth req
            req.Headers.Add("API-Version", "2023-10")
            req.Content <- new StringContent(query, Encoding.UTF8, "application/json")
            let resp = client.Send(req)
            if resp.IsSuccessStatusCode then
                let body = resp.Content.ReadAsStringAsync().Result
                sprintf "[诊断 %s] ✅ HTTP %d - %s" label (int resp.StatusCode) (if body.Length > 200 then body.Substring(0, 200) + "..." else body)
            else
                let errBody = resp.Content.ReadAsStringAsync().Result
                sprintf "[诊断 %s] ❌ HTTP %d - %s" label (int resp.StatusCode) errBody
        with ex ->
            sprintf "[诊断 %s] ❌ 异常: %s" label ex.Message
    
    output "========== Monday API 连接诊断 =========="
    output (sprintf "API Key 前缀: %s..." (if (apikey : string).Length > 20 then apikey.Substring(0, 20) else apikey))
    
    // 测试 1: Bearer 前缀（推荐格式）
    let r1 = tryAuth "Bearer" (fun req -> req.Headers.Authorization <- new AuthenticationHeaderValue("Bearer", apikey))
    output r1
    
    // 测试 2: 无前缀直接传（旧代码格式）
    let r2 = tryAuth "RAW" (fun req -> req.Headers.TryAddWithoutValidation("Authorization", apikey) |> ignore)
    output r2
    
    output "========== 诊断完成 =========="