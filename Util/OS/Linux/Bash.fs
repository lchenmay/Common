module Util.Linux.Bash

open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Text
open System.IO
open System.Collections.Generic
open System.Net.Sockets

let cyan (s: string) = $"\u001b[36m{s}\u001b[0m"
let green (s: string) = $"\u001b[32m{s}\u001b[0m"
let red (s: string) = $"\u001b[31m{s}\u001b[0m"
let white (s: string) = $"\u001b[37m{s}\u001b[0m"
let yellow (s: string) = $"\u001b[33m{s}\u001b[0m"
let orange (s: string) = $"\u001b[38;5;208m{s}\u001b[0m"

type Credential = (int option) * string * string

/// 全局 SSH 私钥路径（由调用方设置）
let mutable sshPrivateKeyPath = ""

/// 清理命令中的 Windows 行尾符和多余空白
let cleanCommand (cmd: string) =
    cmd.Replace("\r\n", "\n").Replace("\r", "").Trim()

/// 获取 SSH 私钥路径
let getSshPrivateKeyPath() =
    if not (String.IsNullOrEmpty sshPrivateKeyPath) then
        sshPrivateKeyPath
    else
        match Environment.GetEnvironmentVariable("SSH_PRIVATE_KEY_PATH") with
        | null | "" -> 
            let defaultPath = Path.Combine(Directory.GetCurrentDirectory(), "id_rsa")
            if File.Exists(defaultPath) then defaultPath else ""
        | path -> path

/// 获取 SSH 私钥参数
let getSshPrivateKeyArg() =
    match getSshPrivateKeyPath() with
    | "" -> ""
    | path -> $"-i \"{path}\""

/// 确保 SSH ControlPath 目录存在，返回可用的 ControlPath 路径
let private ensureSshControlDir() =
    let dir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".ssh", "controlmasters")
    if not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> ignore
    dir

/// 获取 SSH 连接复用选项（ControlMaster），复用同一条 TCP 连接避免重复握手
/// - ControlMaster=auto: 自动复用已有连接，没有则创建
/// - ControlPath: 使用 %C 哈希作为 socket 名，兼容 Windows 文件名限制
/// - ControlPersist=600: 连接保持 10 分钟，期间新连接可复用
/// 注意：Windows 上 ControlMaster 基于 Unix Domain Socket，OpenSSH for Windows 实现不完整，
/// 会反复产生 "getsockname failed: Not a socket" 错误，因此在 Windows 上直接禁用。
let private isWindows =
    RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

let getSshControlOpts() =
    if isWindows then
        ""
    else
        let controlDir = ensureSshControlDir()
        let controlPath = (Path.Combine(controlDir, "ssh-%C")).Replace('\\', '/')
        $"-o ControlMaster=auto -o ControlPath=\"{controlPath}\" -o ControlPersist=600"

/// 检测是否是 SSH ControlMaster socket 损坏错误
let isSocketError (text: string) =
    not (String.IsNullOrEmpty text) &&
    (text.Contains("getsockname failed") || 
     text.Contains("Not a socket") ||
     text.Contains("Connection closed") ||
     text.Contains("Unknown error"))

/// 清理所有 ControlMaster socket 文件，返回清理数量
/// Windows 上 SSH ControlMaster 可能使用命名管道或特殊文件，File.Delete 可能失败，
/// 因此优先用 ssh -O exit 优雅关闭连接，再用 File.Delete + cmd del 兜底清理残留。
let cleanSshControlSockets (output: string -> unit) =
    try
        let dir = Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
            ".ssh", "controlmasters")
        if Directory.Exists dir then
            // 不加文件名筛选，避免 Windows 上 glib pattern 匹配失败
            let allFiles = Directory.GetFiles(dir)
            let socketFiles = allFiles |> Array.filter (fun f ->
                let name = Path.GetFileName(f)
                name.StartsWith("ssh-"))

            let mutable cleaned = 0

            // 阶段 1：ssh -O exit 优雅关闭 master 连接（释放文件锁）
            for f in socketFiles do
                try
                    let normalizedPath = f.Replace('\\', '/')
                    let exitArgs = $"-O exit -o ControlPath=\"{normalizedPath}\" localhost"
                    let psi = ProcessStartInfo("ssh", exitArgs,
                        RedirectStandardError = true,
                        RedirectStandardOutput = true,
                        UseShellExecute = false,
                        CreateNoWindow = true)
                    use proc = Process.Start(psi)
                    proc.WaitForExit(3000) |> ignore
                with _ -> ()

            System.Threading.Thread.Sleep(200)

            // 阶段 2：File.Delete 删除残留文件
            for f in socketFiles do
                if File.Exists f then
                    try File.Delete f; cleaned <- cleaned + 1
                    with _ ->
                        // 阶段 3：Windows 兜底 — cmd del /f 强制删除
                        try
                            let delPsi = ProcessStartInfo("cmd", $"/c del /q /f \"{f}\"",
                                RedirectStandardError = true,
                                RedirectStandardOutput = true,
                                UseShellExecute = false,
                                CreateNoWindow = true)
                            use proc = Process.Start(delPsi)
                            proc.WaitForExit(3000) |> ignore
                            if not (File.Exists f) then cleaned <- cleaned + 1
                        with _ -> ()

            if cleaned > 0 then
                $"[SSH] 已清理 {cleaned} 个损坏的 ControlMaster socket" |> yellow |> output
            cleaned
        else 0
    with _ -> 0

/// 懒加载：扫描系统查找 Git 安装目录（仅计算一次）
let private gitPathCache = lazy (
    let commonPaths = [|
        @"C:\Program Files\Git\cmd"
        @"C:\Program Files\Git\bin"
        @"C:\Program Files (x86)\Git\cmd"
        @"C:\Program Files (x86)\Git\bin"
    |]
    let gitDir = commonPaths |> Array.tryFind (fun dir ->
        File.Exists(Path.Combine(dir, "git.exe")))
    match gitDir with
    | Some dir -> Some dir
    | None ->
        // 从注册表查找 Git for Windows 安装路径
        try
            use key = Microsoft.Win32.Registry.LocalMachine.OpenSubKey(@"SOFTWARE\GitForWindows")
            if key <> null then
                let installPath = key.GetValue("InstallPath") :?> string
                if not (String.IsNullOrEmpty(installPath)) then
                    let cmdDir = Path.Combine(installPath, "cmd")
                    if Directory.Exists(cmdDir) then Some cmdDir else None
                else None
            else None
        with _ -> None
)

/// 获取增强版 PATH：自动补充 Git 目录，确保子进程能找到 git
let private getAugmentedPath() =
    let currentPath = Environment.GetEnvironmentVariable("PATH")
    let currentPath = if isNull currentPath then "" else currentPath
    match gitPathCache.Force() with
    | Some gitDir ->
        if currentPath.Contains(gitDir) then currentPath
        else gitDir + ";" + currentPath
    | None -> currentPath

/// 执行本地命令（支持自定义超时）- 修复编码和 PowerShell 兼容
let execWithTimeout output setDir (fileName: string) (args: string) (timeoutMs: int) : string =
    $"{fileName}: {args}" |> cyan |> output
    
    let psi = ProcessStartInfo(fileName, args)
    if not (String.IsNullOrWhiteSpace(setDir)) then
        psi.WorkingDirectory <- setDir
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    
    // 设置 UTF-8 编码
    psi.StandardOutputEncoding <- Encoding.UTF8
    psi.StandardErrorEncoding <- Encoding.UTF8

    // 自动注入 Git 路径到子进程 PATH，防止父进程启动时 PATH 不含 Git
    psi.Environment.["PATH"] <- getAugmentedPath()

    use proc = new Process(StartInfo = psi)
    let outputBuilder = StringBuilder()
    let errorBuilder = StringBuilder()

    proc.OutputDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            let data = e.Data
            data |> output
            outputBuilder.AppendLine(data) |> ignore)

    proc.ErrorDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            let data = e.Data
            data |> output
            errorBuilder.AppendLine(data) |> ignore)

    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    
    if not (proc.WaitForExit(timeoutMs)) then
        proc.Kill()
        $"命令执行超时（{timeoutMs}ms）" |> red |> output
        // 即使超时也返回已捕获的输出（命令可能已输出错误信息后才挂起）
        let combined = outputBuilder.ToString() + errorBuilder.ToString()
        if not (String.IsNullOrWhiteSpace combined) then combined
        else ""

    elif proc.ExitCode <> 0 then
        $"ExitCode {proc.ExitCode}" |> red |> output
        // 合并 stdout 和 stderr：当命令使用了 2>&1 时 stderr 重定向到了 stdout，
        // errorBuilder 可能为空，此时 stdout 中包含了实际的错误信息
        let combined = outputBuilder.ToString() + errorBuilder.ToString()
        if not (String.IsNullOrWhiteSpace combined) then combined
        else $"ExitCode {proc.ExitCode}"  // 兜底：确保调用方不会收到空字符串

    else
        outputBuilder.ToString()

/// 执行本地命令（默认30秒超时）
let exec output setDir (fileName: string) (args: string) : string =
    execWithTimeout output setDir fileName args 30000

/// 生成 SSH 密钥的命令
let email__SshKey email = 
    $"ssh-keygen -t rsa -b 4096 -C {email}"

/// SSH 远程执行（带自定义超时）- 自动清理命令，自动检测并修复 ControlMaster socket 损坏
let bashWithTimeout output credential (cmd: string) (timeoutMs: int) : string =
    let porto, user, server = credential
    let effectiveCmd = 
        if String.IsNullOrWhiteSpace cmd then 
            "echo 'SSH connection established'"
        else 
            cleanCommand cmd
    
    let privateKeyArg = getSshPrivateKeyArg()
    let controlOpts = getSshControlOpts()
    let sshOpts = $"-o StrictHostKeyChecking=no -o ConnectTimeout=10 {controlOpts}"
    let sshOptsNoControl = $"-o StrictHostKeyChecking=no -o ConnectTimeout=10"
    
    let buildArgs (includeControl: bool) =
        let opts = if includeControl then sshOpts else sshOptsNoControl
        match porto with
        | Some p -> $"{opts} {privateKeyArg} -p {p} {user}@{server} \"{effectiveCmd}\""
        | None -> $"{opts} {privateKeyArg} {user}@{server} \"{effectiveCmd}\""
    
    let args = buildArgs true
    let result = execWithTimeout output "" "ssh" args timeoutMs
    
    // ── 自动修复：检测 ControlMaster socket 损坏则清理并重试 ──
    if isSocketError result then
        $"[SSH] 检测到 ControlMaster socket 损坏，自动清理并重试..." |> yellow |> output
        cleanSshControlSockets output |> ignore
        System.Threading.Thread.Sleep(500)
        // 不带 ControlMaster 重试，首次连接重建 socket
        let retryArgs = buildArgs false
        let retryResult = execWithTimeout output "" "ssh" retryArgs timeoutMs
        // ── 重试成功后，后台尝试用 ControlMaster 再连一次建立健康 socket ──
        // 注意：重建结果不影响返回值——即使重建失败，命令本身已经成功执行
        if not (isSocketError retryResult) then
            System.Threading.Thread.Sleep(300)
            let rebuildArgs = buildArgs true
            let rebuildResult = execWithTimeout output "" "ssh" rebuildArgs timeoutMs
            if isSocketError rebuildResult then
                $"[SSH] ControlMaster 重建失败（不影响命令结果），后续连接将使用新连接" |> yellow |> output
            retryResult
        else
            retryResult
    else
        result

/// SSH 远程执行 — 短超时+自动重试，适合轻量级命令（目录检查、mkdir、rm 等）
/// timeoutMs: 单次超时（毫秒），maxRetries: 超时后最多重试次数
let bashWithRetry output credential cmd (timeoutMs: int) (maxRetries: int) : string =
    let mutable result = ""
    let mutable attempt = 0
    while attempt <= maxRetries && String.IsNullOrWhiteSpace result do
        if attempt > 0 then
            let delayMs = 1000 * (1 <<< (attempt - 1))  // 指数退避: 1s, 2s, 4s, 8s...
            System.Threading.Thread.Sleep(delayMs)
            $"重试 ({attempt}/{maxRetries}, 等待{delayMs/1000}s)..." |> yellow |> output
        result <- bashWithTimeout output credential cmd timeoutMs |> fun s -> s.Trim()
        attempt <- attempt + 1
    result

/// SSH 远程执行可能产生子进程的命令（bun/node 等），
/// 通过 </dev/null 关闭 stdin 防止子进程阻塞 SSH PTY 导致超时
let bashDetached output credential (cmd: string) (timeoutMs: int) : string =
    let effectiveCmd = cleanCommand cmd
    bashWithTimeout output credential $"( {effectiveCmd} ) </dev/null" timeoutMs

/// SSH 远程执行（默认120秒超时，适应慢速 GitHub 连接和大文件操作）
let bash output credential cmd : string =
    bashWithTimeout output credential cmd 120000

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
    psi.StandardOutputEncoding <- Encoding.UTF8
    psi.StandardErrorEncoding <- Encoding.UTF8

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
        
        let copyCmd = $"type \"{pubFile}\" | ssh {privateKeyArg} {portArg} {target} \"mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys\""
        let permCmd = $"chmod 700 ~/.ssh && chmod 600 ~/.ssh/authorized_keys"
        let testCmd = $"echo 'SSH configured successfully'"
        
        Some (pubFile, copyCmd, permCmd, testCmd)

/// 检查服务器是否启用公钥认证
let checkPubkeyAuthentication output credential =
    let porto, user, server = credential
    let cmd = "grep -E '^PubkeyAuthentication yes' /etc/ssh/sshd_config"
    let result = bashWithTimeout output credential cmd 60000
    
    if result.Contains("PubkeyAuthentication yes") then
        "✓ 服务器已启用公钥认证" |> green |> output
        true
    elif result.Contains("命令执行超时") || result.Contains("Connection timed out") then
        "⚠ SSH 连接超时，跳过公钥认证检查" |> yellow |> output
        true
    else
        "⚠ 服务器未启用公钥认证" |> yellow |> output
        false


// 构建 psql 命令 - 使用单引号包裹 SQL 避免 SSH 解析问题
let psqlpath__Cmd (psqlPath:string) (sql:string) = 
    let cleanPath = psqlPath.Trim().Replace("\n", "").Replace("\r", "")
    let escapedSql = sql.Replace("'", "'\\''")
    $"sudo -u postgres {cleanPath} -h /var/run/postgresql -p 5432 -c '{escapedSql}'"

// ==================== SSH 隧道管理 ====================

/// SSH 隧道进程注册表 (localPort -> Process)
let mutable tunnelProcesses : Dictionary<int, Process> = Dictionary<int, Process>()

/// 检查本地 TCP 端口是否可用
let isLocalPortAvailable (port:int) : bool =
    try
        use socket = new TcpListener(System.Net.IPAddress.Loopback, port)
        socket.Start()
        socket.Stop()
        true
    with _ -> false

/// 从 startPort 开始查找可用端口
let findAvailablePort (startPort:int) : int =
    let mutable port = startPort
    while port < startPort + 1000 && not (isLocalPortAvailable port) do
        port <- port + 1
    if port >= startPort + 1000 then
        failwith "No available ports in range"
    port

/// 启动 SSH 隧道: localPort -> remoteHost:remotePort
/// 在 Windows 上使用 -N（非 fork 模式），手动管理后台进程
let startSSHTunnel output (localPort:int) credential (remotePort:int) : bool =
    let porto, user, server = credential
    let portArg = match porto with Some p -> $"-p {p}" | None -> ""
    let privateKeyArg = getSshPrivateKeyArg()
    let controlOpts = getSshControlOpts()
    
    // -N: 不执行远程命令（纯转发）
    // -o ExitOnForwardFailure=yes: 转发失败则退出
    // -o ServerAliveInterval=60: 保持连接活跃
    let args = 
        $"{privateKeyArg} {portArg} -o StrictHostKeyChecking=no " +
        $"-o ExitOnForwardFailure=yes -o ServerAliveInterval=60 " +
        $"{controlOpts} " +
        $"-N -L {localPort}:localhost:{remotePort} {user}@{server}"
    
    $"启动 SSH 隧道: localhost:{localPort} -> {server}:{remotePort}" 
    |> cyan |> output
    
    let psi = ProcessStartInfo("ssh", args)
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    
    let proc = new Process(StartInfo = psi)
    
    // 捕获错误输出用于诊断
    let errBuilder = StringBuilder()
    proc.ErrorDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            errBuilder.AppendLine(e.Data) |> ignore)
    
    proc.Start() |> ignore
    proc.BeginErrorReadLine()
    
    // 等待 2 秒检查是否立即失败（如端口冲突、认证失败）
    System.Threading.Thread.Sleep 2000
    
    if proc.HasExited then
        let err = errBuilder.ToString()
        $"❌ SSH 隧道建立失败 (ExitCode={proc.ExitCode}): {err}" |> red |> output
        false
    else
        tunnelProcesses.[localPort] <- proc
        $"✓ SSH 隧道已建立: localhost:{localPort} -> {server}:{remotePort}" 
        |> green |> output
        true

/// 关闭指定端口的 SSH 隧道
let stopSSHTunnel output (localPort:int) =
    if tunnelProcesses.ContainsKey localPort then
        let proc = tunnelProcesses.[localPort]
        try
            if not proc.HasExited then
                proc.Kill()
                proc.WaitForExit(3000) |> ignore
            $"✓ 隧道 localhost:{localPort} 已关闭" |> green |> output
        with ex ->
            $"⚠ 关闭隧道时出错: {ex.Message}" |> yellow |> output
        tunnelProcesses.Remove localPort |> ignore

/// 关闭所有 SSH 隧道
let stopAllSshTunnels output =
    let ports : int array = tunnelProcesses.Keys |> Seq.toArray
    for port in ports do
        stopSSHTunnel output port
    $"所有 SSH 隧道已关闭（共 {ports.Length} 个）" |> cyan |> output

/// 检查本地端口是否已被 SSH 隧道占用（且进程存活）
let isTunnelActive (localPort:int) =
    tunnelProcesses.ContainsKey localPort && 
    not (tunnelProcesses.[localPort].HasExited)