module UtilKestrel.BashDeployer.Process

open Util.Linux.Bash
open Util.Monitor
open Util.OS
open UtilKestrel.Types

// ==================== 端口验证（代码层门禁） ====================

/// 验证端口分配是否符合 mod 4 规则
/// 返回：空字符串 = 验证通过，非空 = 错误信息
let portVerfiy (debugger: Debugger) (port80: int, port443: int) : string =
    let httpMod = port80 % 4
    let httpsMod = port443 % 4
    
    // 规则 1：同一服务的 HTTP 和 HTTPS 必须同 mod 4
    if httpMod <> httpsMod then
        $"端口分配错误：HTTP {port80} (mod 4 = {httpMod}) 和 HTTPS {port443} (mod 4 = {httpsMod}) 必须同 mod 4"
    else
        match debugger with
        | Debugger.AI ->
            // 规则 2：AI 调试端口 mod 4 必须是 2,2
            if httpMod <> 2 then
                $"AI 调试端口错误：HTTP {port80} (mod 4 = {httpMod}) 必须是 mod 4 = 2"
            else
                ""  // 验证通过
        | _ ->
            // 规则 3：人类调试端口 mod 4 必须是 0,0
            if httpMod <> 0 then
                $"人类调试端口错误：HTTP {port80} (mod 4 = {httpMod}) 必须是 mod 4 = 0"
            else
                ""  // 验证通过

// ==================== 健康检查（非阻塞） ====================

/// 轻量 CF 健康检查：检查 cloudflared 是否在运行，不修配置
let checkCfHealth output credential =
    "\n--- Cloudflare Tunnel 健康检查 ---" |> cyan |> output
    try
        let installed = bash output credential "command -v cloudflared > /dev/null 2>&1 && echo 'INSTALLED' || echo 'NOT_INSTALLED'"
        if installed.Contains("NOT_INSTALLED") then
            "  cloudflared 未安装，跳过" |> cyan |> output
            true
        else
            let active = bash output credential "systemctl is-active cloudflared 2>/dev/null || echo 'inactive'"
            if active.Trim() = "active" then
                "✓ cloudflared 服务正在运行" |> green |> output
                true
            else
                "[DEPLOY-WARN] cloudflared 服务未运行，部署继续（AI 后续处理）" |> yellow |> output
                false
    with ex ->
        $"[DEPLOY-WARN] CF 健康检查异常: {ex.Message}" |> yellow |> output
        false
