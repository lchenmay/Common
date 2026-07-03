module UtilKestrel.BashDeployer.Common

open System
open Util.Linux.Bash
open UtilKestrel.Types

// ==================== 部署进度追踪 ====================

/// 写入部署进度 JSON 到远程服务器的 ~/.deploy-stamps/{code}-deploy-progress.json
/// 该文件由 API 端点 deployStatus 读取，供前端/外部监控部署进度
let writeDeployProgress output credential code phase startedAt currentStep gitBefore gitAfter error =
    try
        let updatedAt = DateTime.UtcNow.ToString("o")
        let elapsed =
            if startedAt <> "" then
                try let startTime = DateTime.Parse(startedAt, System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.AssumeUniversal ||| System.Globalization.DateTimeStyles.AdjustToUniversal)
                    let s = (DateTime.UtcNow - startTime).TotalSeconds |> int
                    (if s < 0 then 0 else s).ToString() + "s"
                with _ -> "?"
            else "0s"
        let esc (s: string) = s.Replace("\\", "/").Replace("\"", "'").Replace("\n", " ").Replace("\r", "")
        let json = sprintf """{"phase":"%s","startedAt":"%s","updatedAt":"%s","elapsed":"%s","currentStep":"%s","gitBefore":"%s","gitAfter":"%s","error":"%s"}"""
                        phase startedAt updatedAt elapsed (esc currentStep) (esc gitBefore) (esc gitAfter) (esc error)
        let mkdirCmd = "mkdir -p /root/.deploy-stamps"
        bash output credential mkdirCmd |> ignore
        let escapedJson = json.Replace("\\", "\\\\").Replace("\"", "\\\"")
        let cmd = sprintf "echo \"%s\" > /root/.deploy-stamps/%s-deploy-progress.json" escapedJson code
        bash output credential cmd |> ignore
    with _ -> ()

/// 清空部署进度（部署结束后重置为 idle）
let clearDeployProgress output credential code =
    try
        let cmd = sprintf "rm -f /root/.deploy-stamps/%s-deploy-progress.json" code
        bash output credential cmd |> ignore
    with _ -> ()


// ==================== 网络检查 ====================

/// 判断目标服务器是否在内网（RFC 1918 私有地址段）
/// 192.168.x.x, 10.x.x.x, 172.16-31.x.x
let isPrivateNetwork (server: string) =
    let host =
        // 去除可能的端口号
        match server.LastIndexOf(':') with
        | -1 -> server
        | idx -> server.Substring(0, idx)
    
    // 尝试解析为 IP
    match System.Net.IPAddress.TryParse(host) with
    | true, ip ->
        let bytes = ip.GetAddressBytes()
        // 10.0.0.0/8
        if bytes.[0] = 10uy then true
        // 172.16.0.0/12
        elif bytes.[0] = 172uy && bytes.[1] >= 16uy && bytes.[1] <= 31uy then true
        // 192.168.0.0/16
        elif bytes.[0] = 192uy && bytes.[1] = 168uy then true
        // 127.0.0.0/8 (localhost 也当内网)
        elif bytes.[0] = 127uy then true
        else false
    | false, _ ->
        // 非 IP 地址（域名），无法判断，保守视为外网
        false


// ==================== 仓库 URL ====================

/// 获取仓库 URL（SSH 协议）
let getRepoUrl code =
    match code with
    | "Aiarwa" -> "git@github.com:lchenmay/Aiarwa.git"
    | "Common" -> "git@github.com:lchenmay/Common.git"
    | "JCS" -> "git@github.com:lchenmay/JCS.git"
    | "WYI" -> "git@github.com:R77R77R/WYI.git"
    | _ -> $"git@github.com:siduochen/{code}.git"

/// 获取仓库 URL（HTTPS 协议，配合 gh auth 使用）
let getRepoUrlHttps code =
    match code with
    | "Aiarwa" -> "https://github.com/lchenmay/Aiarwa.git"
    | "Common" -> "https://github.com/lchenmay/Common.git"
    | "JCS" -> "https://github.com/lchenmay/JCS.git"
    | "WYI" -> "https://github.com/R77R77R/WYI.git"
    | _ -> $"https://github.com/siduochen/{code}.git"


// ==================== 版本检查函数 ====================

/// 远程获取仓库的 git hash
/// repoPath 应该是相对路径（如 Dev/WYI），函数会自动添加 ~/ 前缀
let remoteGitHash output credential repoPath =
    try
        let cmd = $"cd ~/{repoPath} && git rev-parse --short=8 HEAD 2>/dev/null || echo '-'"
        let result = bash output credential cmd
        result.Trim()
    with _ -> "-"

/// 远程检查 dist 目录是否有产物（返回文件数）
/// vscodeDir 应该是相对路径（如 Dev/WYI/vscode），函数会自动添加 ~/ 前缀
let remoteDistFileCount output credential vscodeDir =
    try
        let cmd = $"if [ -d ~/{vscodeDir}/dist ]; then ls ~/{vscodeDir}/dist 2>/dev/null | wc -l; else echo '0'; fi"
        let result = bash output credential cmd
        result.Trim()
    with _ -> "0"

/// 远程通过 curl 查询 monitorVersion API 获取运行时版本
let remoteQueryVersion output credential port code =
    try
        let cmd = $"curl -s -X POST http://localhost:{port}/api/admin/monitorVersion -H 'Content-Type: application/json' -d '{{\"act\":\"monitorversion\"}}' 2>/dev/null || echo '{{\"Er\":\"N/A\"}}'"
        let result = bash output credential cmd
        result.Trim()
    with _ -> "N/A"
