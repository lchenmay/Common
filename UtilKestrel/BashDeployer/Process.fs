module UtilKestrel.BashDeployer.Process

open Util.Linux.Bash
open Util.Monitor
open UtilKestrel.Types

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
