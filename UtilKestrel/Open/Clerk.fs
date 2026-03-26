module UtilKestrel.Open.Clerk

open System
open System.Text
open System.IdentityModel.Tokens.Jwt

open Microsoft.AspNetCore.Http

let getClerkIdentity output (httpx: HttpContext) =
    let authHeader = httpx.Request.Headers.["Authorization"].ToString()
    if authHeader.StartsWith("Bearer ", StringComparison.OrdinalIgnoreCase) then
        try
            // 1. 去掉 "Bearer " 前缀
            let token = authHeader.Replace("Bearer ", "")
        
            // 2. 使用 JwtSecurityTokenHandler 解析（无需密钥即可读取内容）
            let handler = JwtSecurityTokenHandler()
            let jwtToken = handler.ReadJwtToken(token)
        
            // 3. 提取 'sub' 字段，这才是 Clerk 的真正 User ID (如 user_2xb...)
            let userId = jwtToken.Subject 
        
            "✅ 成功解析 JWT，Clerk User ID: " + userId
            |> output
                        
            userId
        with
        | ex -> 
            "❌ JWT 解析失败: " + ex.Message
            |> output
            ""                    
    else
        ""

