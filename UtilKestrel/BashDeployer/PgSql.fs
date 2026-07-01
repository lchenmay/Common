module UtilKestrel.BashDeployer.PgSql

open Util.Linux.Bash
open Util.Monitor
open UtilKestrel.Types

// ==================== 健康检查（非阻塞） ====================

/// 轻量 DB 健康检查：pg_isready 一次，通就过，不通记日志不阻断
let checkDbHealth output credential =
    "\n--- DB 健康检查 ---" |> cyan |> output
    try
        let result = bash output credential "pg_isready -q 2>&1 && echo 'DB_OK' || echo 'DB_UNREACHABLE'"
        if result.Contains("DB_OK") then
            "✓ PostgreSQL 可达" |> green |> output
            true
        else
            "[DEPLOY-WARN] PostgreSQL 不可达，部署继续（AI 后续处理）" |> yellow |> output
            result |> output
            false
    with ex ->
        $"[DEPLOY-WARN] DB 健康检查异常: {ex.Message}" |> yellow |> output
        false
