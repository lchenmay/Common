module Util.Linux.Bash

open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Text
open System.IO

let cyan (s: string) = $"\u001b[36m{s}\u001b[0m"
let green (s: string) = $"\u001b[32m{s}\u001b[0m"
let red (s: string) = $"\u001b[31m{s}\u001b[0m"
let white (s: string) = $"\u001b[37m{s}\u001b[0m"
let yellow (s: string) = $"\u001b[33m{s}\u001b[0m"
let orange (s: string) = $"\u001b[38;5;208m{s}\u001b[0m"

type Credential = (int option) * string * string

/// 全局 SSH 私钥路径（由调用方设置）
let mutable sshPrivateKeyPath = ""

/// 获取 SSH 私钥路径
let getSshPrivateKeyPath() =
    if not (String.IsNullOrEmpty sshPrivateKeyPath) then
        sshPrivateKeyPath
    else
        match Environment.GetEnvironmentVariable("SSH_PRIVATE_KEY_PATH") with
        | null | "" -> 
            // 默认使用当前目录下的 id_rsa
            let defaultPath = Path.Combine(Directory.GetCurrentDirectory(), "id_rsa")
            if File.Exists(defaultPath) then defaultPath else ""
        | path -> path

/// 获取 SSH 私钥参数
let getSshPrivateKeyArg() =
    match getSshPrivateKeyPath() with
    | "" -> ""
    | path -> $"-i \"{path}\""

/// 执行本地命令（支持自定义超时）
let execWithTimeout output setDir (fileName: string) (args: string) (timeoutMs: int) : string =
    $"{fileName}: {args}" |> cyan |> output
    let psi = ProcessStartInfo(fileName, args)
    if not (String.IsNullOrWhiteSpace(setDir)) then
        psi.WorkingDirectory <- setDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    use proc = new Process(StartInfo = psi)
    let outputBuilder = StringBuilder()
    let errorBuilder = StringBuilder()

    proc.OutputDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            e.Data |> output
            outputBuilder.AppendLine(e.Data) |> ignore)

    proc.ErrorDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            e.Data |> output
            errorBuilder.AppendLine(e.Data) |> ignore)

    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    
    if not (proc.WaitForExit(timeoutMs)) then
        proc.Kill()
        $"命令执行超时（{timeoutMs}ms）" |> red |> output
        ""

    elif proc.ExitCode <> 0 then
        $"ExitCode {proc.ExitCode}" |> red |> output
        errorBuilder.ToString()

    else
        outputBuilder.ToString()

/// 执行本地命令（默认30秒超时）
let exec output setDir (fileName: string) (args: string) : string =
    execWithTimeout output setDir fileName args 30000

/// 生成 SSH 密钥的命令
let email__SshKey email = 
    $"ssh-keygen -t rsa -b 4096 -C {email}"

/// SSH 远程执行（带自定义超时）
let bashWithTimeout output credential (cmd: string) (timeoutMs: int) : string =
    let porto, user, server = credential
    let effectiveCmd = 
        if String.IsNullOrWhiteSpace cmd then 
            "echo 'SSH connection established'"
        else 
            cmd
    
    let privateKeyArg = getSshPrivateKeyArg()
    let args = 
        match porto with
        | Some p -> $"{privateKeyArg} -p {p} {user}@{server} \"{effectiveCmd}\""
        | None -> $"{privateKeyArg} {user}@{server} \"{effectiveCmd}\""
    
    execWithTimeout output "" "ssh" args timeoutMs

/// SSH 远程执行（默认30秒超时）
let bash output credential cmd : string =
    bashWithTimeout output credential cmd 30000

/// SSH 远程执行多个命令
let bashMultiple output credential cmds =
    cmds 
    |> String.concat " && "
    |> bash output credential

let execute output credential cmd =
    $"\n--- Executing: {cmd} ---" |> cyan |> output
    let result = bash output credential cmd
    if not (String.IsNullOrEmpty result) then
        result |> output
    result

/// 检查 SSH 免密登录是否已配置成功（带超时）
let checkSshKeyConfiguredWithTimeout output credential (timeoutMs: int) : bool =
    let porto, user, server = credential
    let portArg = 
        match porto with
        | Some p -> $"-p {p}"
        | None -> ""
    
    let privateKeyArg = getSshPrivateKeyArg()
    let args = $"{privateKeyArg} {portArg} -o BatchMode=yes -o ConnectTimeout=10 {user}@{server} \"echo 'ok'\""
    
    let psi = ProcessStartInfo("ssh", args)
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    use proc = new Process(StartInfo = psi)
    let outputBuilder = StringBuilder()
    let errorBuilder = StringBuilder()

    proc.OutputDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            outputBuilder.AppendLine(e.Data) |> ignore)

    proc.ErrorDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            errorBuilder.AppendLine(e.Data) |> ignore)

    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    
    let exited = proc.WaitForExit(timeoutMs)
    
    if not exited then
        proc.Kill()
        $"SSH 连接超时 ({user}@{server})" |> yellow |> output
        false
    else
        let exitCode = proc.ExitCode
        if exitCode = 0 then
            $"SSH 免密登录已配置 ({user}@{server})" |> green |> output
            true
        else
            let error = errorBuilder.ToString()
            if error.Contains("Permission denied") then
                $"SSH 免密登录未配置 ({user}@{server})，需要先配置密钥" |> yellow |> output
            else
                $"SSH 连接失败 ({user}@{server}): {error.Trim()}" |> yellow |> output
            false

/// 检查 SSH 免密登录是否已配置成功（默认30秒超时）
let checkSshKeyConfigured output credential : bool =
    checkSshKeyConfiguredWithTimeout output credential 30000

/// 获取复制 SSH 公钥的命令（返回命令字符串，不自动执行）
let remoteCopy_SshKeyCommands output credential (file: string) =
    let porto, user, server = credential
    let target = $"{user}@{server}"
    let portArg = 
        match porto with
        | Some port -> $"-p {port}"
        | None -> ""
    
    let pubFile = 
        if file.EndsWith ".pub" then 
            file 
        else 
            file + ".pub"
    
    if not (File.Exists(pubFile)) then
        red $"公钥文件不存在: {pubFile}" |> output
        None
    else
        green $"复制公钥到 {target}" |> output
        
        let privateKeyArg = getSshPrivateKeyArg()
        
        // 命令1：复制公钥
        let copyCmd = $"type \"{pubFile}\" | ssh {privateKeyArg} {portArg} {target} \"mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys\""
        
        // 命令2：设置权限（目录 700，文件 600）
        let permCmd = $"chmod 700 ~/.ssh && chmod 600 ~/.ssh/authorized_keys"
        
        // 命令3：验证连接
        let testCmd = $"echo 'SSH configured successfully'"
        
        Some (pubFile, copyCmd, permCmd, testCmd)

/// 检查服务器是否启用公钥认证
let checkPubkeyAuthentication output credential =
    let porto, user, server = credential
    
    // 直接使用 bash，不用 cmd /c
    let cmd = "grep -E '^PubkeyAuthentication yes' /etc/ssh/sshd_config"
    
    // 给予 60 秒超时（网络慢的情况）
    let result = bashWithTimeout output credential cmd 60000
    
    if result.Contains("PubkeyAuthentication yes") then
        "✓ 服务器已启用公钥认证" |> green |> output
        true
    elif result.Contains("命令执行超时") || result.Contains("Connection timed out") then
        "⚠ SSH 连接超时，跳过公钥认证检查" |> yellow |> output
        // 返回 true 允许流程继续（可能服务器已配置）
        true
    else
        "⚠ 服务器未启用公钥认证" |> yellow |> output
        false


// 构建 psql 命令 - 使用单引号包裹 SQL 避免 SSH 解析问题
let psqlpath__Cmd (psqlPath:string) (sql:string) = 
    let cleanPath = psqlPath.Trim().Replace("\n", "").Replace("\r", "")
    // 使用单引号代替双引号，避免 shell 解析
    // 转义 SQL 中的单引号
    let escapedSql = sql.Replace("'", "'\\''")
    $"sudo -u postgres {cleanPath} -h /var/run/postgresql -p 5432 -c '{escapedSql}'"