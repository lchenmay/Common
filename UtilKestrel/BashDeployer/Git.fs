module UtilKestrel.BashDeployer.Git

open System
open System.Diagnostics
open System.IO
open System.Collections.Generic

open Util.Linux.Linux
open Util.Linux.Bash
open Util.Linux.Git
open Util.Monitor
open UtilKestrel.Types
open UtilKestrel.BashDeployer.Common

// =================== Git SSH ===================

/// SSH 密钥文件名（统一存放于 {disk}Dev/ 目录下）
let sshKeyFile = "id_git"

/// 检查本地 GitHub SSH 密钥是否存在
/// disk: "C:/" — 将在 {disk}Dev/ 下查找 {sshKeyFile} 和 {sshKeyFile}.pub
/// 不存在时直接失败，避免后台部署线程永久阻塞在控制台输入上
let checkLocalGitSshKey output disk =
    let keyPath = disk + "Dev/" + sshKeyFile
    let privKey = keyPath
    let pubKey  = keyPath + ".pub"

    if File.Exists pubKey && File.Exists privKey then
        $"✓ 本地 GitHub SSH 密钥已就绪: {pubKey}" |> green |> output
    else
        "⚠ 本地 GitHub SSH 密钥不存在" |> yellow |> output
        $"  生成方式: ssh-keygen -t ed25519 -f {keyPath} -C git -N ''" |> orange |> output
        $"  然后将 {pubKey} 添加到 https://github.com/settings/keys" |> yellow |> output
        failwith $"GitHub SSH 密钥不存在: {keyPath}"

/// 把本地 GitHub SSH 密钥部署到远程服务器
/// disk: "C:/" — 从 {disk}Dev/ 读取 {sshKeyFile} / {sshKeyFile}.pub
/// 自动完成: scp → chmod 600 → 覆写 ~/.ssh/config → 验证 ssh -T git@github.com
let setupRemoteGitSshKey output credential disk =
    let keyPath = disk + "Dev/" + sshKeyFile
    let localPriv = keyPath
    let localPub  = keyPath + ".pub"

    if not (File.Exists localPriv && File.Exists localPub) then
        "⚠ 本地 GitHub SSH 密钥不存在，跳过远程部署" |> yellow |> output
    else
        let _, _, _, target, portArg = credentialExpand credential
        let privKeyArg = getSshPrivateKeyArg()
        let controlOpts = getSshControlOpts()

        // 1. scp 密钥到远程（带 socket 损坏自动修复）
        "  1. 复制密钥到远程..." |> cyan |> output
        let buildScpArgs (includeControl: bool) =
            let opts = if includeControl then controlOpts else ""
            [ privKeyArg; portArg; "-o StrictHostKeyChecking=no"; opts ]
            |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s))
            |> String.concat " "
        let scpArgs = buildScpArgs true
        let scpCmd = $"{scpArgs} \"{localPriv}\" \"{localPub}\" {target}:~/.ssh/"
        let scpResult = execWithTimeout output "" "scp" scpCmd 30000

        // ── 自动修复：检测 ControlMaster socket 损坏则清理并重试 ──
        let scpResult =
            if isSocketError scpResult then
                "[SCP] 检测到 ControlMaster socket 损坏，自动清理并重试..." |> yellow |> output
                cleanSshControlSockets output |> ignore
                System.Threading.Thread.Sleep(500)
                let retryArgs = buildScpArgs false
                let retryCmd = $"{retryArgs} \"{localPriv}\" \"{localPub}\" {target}:~/.ssh/"
                execWithTimeout output "" "scp" retryCmd 30000
            else scpResult
        scpResult |> ignore

        // 2. 设权限
        "  2. 设置权限..." |> cyan |> output
        bash output credential $"chmod 600 ~/.ssh/{sshKeyFile} ~/.ssh/{sshKeyFile}.pub" |> ignore

        // 3. 覆写 SSH config（始终删除旧块后追加新块，防止 IdentityFile 过期残留）
        "  3. 覆写 SSH config（防过期残留）..." |> cyan |> output
        bash 
          output 
          credential 
          $"""sed -i '/^Host github\.com$/,/^\$/d' ~/.ssh/config 2>/dev/null
cat >> ~/.ssh/config << 'EOF'
Host github.com
    User git
    IdentityFile ~/.ssh/{sshKeyFile}
    IdentitiesOnly yes
    StrictHostKeyChecking accept-new
EOF""" 
          |> ignore

        // 4. 验证
        "  4. 验证 GitHub SSH 连接..." |> cyan |> output
        let testResult = bash output credential "ssh -T -o StrictHostKeyChecking=accept-new git@github.com 2>&1"
        // GitHub SSH 认证成功但无 Shell 时返回 ExitCode 1，属于正常行为
        // 注意：GitHub 的 "successfully authenticated" 消息输出在 stderr，2>&1 合并后进入返回值
        if testResult.Contains("successfully authenticated") then
            "  ✓ 远程 GitHub SSH 连接正常" |> green |> output
        elif testResult.Contains("Permission denied") || testResult.Contains("publickey") then
            $"  ❌ GitHub SSH 认证失败: 权限被拒绝或密钥无效" |> red |> output
            $"  请确认 {sshKeyFile}.pub 已添加到 https://github.com/settings/keys" |> yellow |> output
        else
            $"  ⚠ GitHub SSH 验证返回: {testResult.Trim()}" |> yellow |> output




// =================== Git 推送函数 ===================

/// 获取仓库本地 HEAD 和远程 origin/main 的 commit hash
let private getRepoHeadAndRemote output repoPath =
    try
        let localCmd = $"cd {repoPath}; git rev-parse HEAD"
        let remoteCmd = $"cd {repoPath}; git rev-parse origin/main"
        let localHash = exec output repoPath "powershell" localCmd
        let remoteHash = exec output repoPath "powershell" remoteCmd
        let local = localHash.Trim()
        let remote = remoteHash.Trim()
        $"  本地 HEAD:  {local}" |> output
        $"  远程 origin/main: {remote}" |> output
        Some (local, remote)
    with _ ->
        $"⚠ 无法获取仓库 {repoPath} 的 hash" |> yellow |> output
        None

/// 推送本地仓库变更（单个仓库）- 使用分号分隔，兼容 Windows PowerShell
/// 返回是否推送成功（远程已包含本地最新提交）
let pushLocalRepo output repoPath gitName gitEmail =
    $"\n--- 推送 {repoPath} 仓库变更 ---" |> cyan |> output
    
    // PowerShell 使用分号分隔命令，不支持 &&
    let cmd = 
        $"cd {repoPath}; " +
        $"git config user.name \"{gitName}\"; " +
        $"git config user.email \"{gitEmail}\"; " +
        "git add .; " +
        "git commit -m \"auto-deploy\"; " +
        "git push"
    
    let result = exec output repoPath "powershell" cmd
    result |> output
    
    // 验证 push 是否成功：本地 HEAD 应等于 origin/main
    match getRepoHeadAndRemote output repoPath with
    | Some (local, remote) ->
        if local = remote then
            "✓ 推送成功 (本地 HEAD == origin/main)" |> green |> output
            true
        else
            "⚠ 推送后本地 HEAD 与 origin/main 不一致，可能需要重试" |> yellow |> output
            false
    | None ->
        "⚠ 无法验证推送结果" |> yellow |> output
        false

/// 推送单个仓库（带重试机制）
let private pushLocalRepoWithRetry output repoPath gitName gitEmail (maxRetries: int) =
    let mutable success = false
    let mutable attempt = 0
    
    while not success && attempt < maxRetries do
        attempt <- attempt + 1
        if attempt > 1 then
            $"\n--- 重试推送 (第 {attempt}/{maxRetries} 次) ---" |> yellow |> output
            // 等待一段时间再重试（避免 GitHub 限速）
            System.Threading.Thread.Sleep(3000)
        
        success <- pushLocalRepo output repoPath gitName gitEmail
    
    if not success then
        $"❌ 推送失败，已重试 {maxRetries} 次" |> red |> output
        $"仓库: {repoPath}" |> red |> output
        "请手动执行 git push 后再重新部署，或检查网络连接" |> yellow |> output
        failwith "Git push 失败，已中止后台部署"
    
    success

/// 前置验证 Git 可用性（在 git push 之前调用，防止盲目重试）
let private verifyGitAvailable output =
    try
        let gitExe =
            [| @"C:\Program Files\Git\cmd\git.exe"
               @"C:\Program Files\Git\bin\git.exe"
               @"C:\Program Files (x86)\Git\cmd\git.exe" |]
            |> Array.tryFind File.Exists
        match gitExe with
        | Some exe ->
            let psi = ProcessStartInfo(exe, "--version")
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.UseShellExecute <- false
            psi.CreateNoWindow <- true
            use p = Process.Start(psi)
            let ver = p.StandardOutput.ReadToEnd().Trim()
            p.WaitForExit(3000) |> ignore
            $"✓ Git 已就绪: {ver}" |> green |> output
            true
        | None ->
            "❌ 找不到 Git 可执行文件，请确认 Git 已安装在默认路径" |> red |> output
            false
    with ex ->
        $"❌ Git 检查异常: {ex.Message}" |> red |> output
        false

/// 推送所有本地仓库变更（带验证和重试）
/// 返回：所有仓库是否都成功推送
let pushAllLocalRepos output code gitName gitEmail disk =
    $"\n=== 开始推送所有本地仓库变更 ===" |> yellow |> output
    
    // 前置检查：确保 Git 可用再推送
    $"前置检查: 验证 Git 可用性..." |> cyan |> output
    if not (verifyGitAvailable output) then
        "❌ Git 不可用，跳过 GitHub 推送（部署将使用 scp 直推方案）" |> yellow |> output
        false
    else
        let repos = [|
            ("主项目", $"{disk}Dev/{code}")
            ("Common", $"{disk}Dev/Common")
            ("JCS", $"{disk}Dev/JCS")
        |]
        
        let mutable allSuccess = true
        repos |> Array.iter (fun (name, path) ->
            if Directory.Exists(path) then
                let ok = pushLocalRepoWithRetry output path gitName gitEmail 3
                if not ok then allSuccess <- false
            else
                $"⚠ 目录不存在: {path}" |> yellow |> output
            "\n" |> output)
        
        "=== 所有本地仓库推送完成 ===" |> yellow |> output
        allSuccess

/// 通过 scp 直接推送源码到目标服务器（内网优先方案 / GitHub 不可用时的 fallback）
/// 将本地 Dev/{code}, Dev/Common, Dev/JCS 目录直接 scp 到远程服务器
/// 参数 `isPrimary`: true=内网优先方案（日志友好）, false=GitHub 失败后的 fallback
let pushSourceViaScp output code credential disk isPrimary =
    if isPrimary then
        $"\n📡 目标服务器在内网，优先使用 scp 直推源码（跳过 GitHub 中转）..." |> cyan |> output
    else
        $"\n⚠ GitHub push 失败，改用 scp 直接推送源码到服务器..." |> orange |> output
    
    let porto, user, server = credential
    let portArg = match porto with Some p -> $"-P {p}" | None -> ""
    let privateKeyArg = getSshPrivateKeyArg()
    let controlOpts = getSshControlOpts()
    
    // 源目录 → 目标目录映射（远程路径使用绝对路径 /root/Dev/）
    let repos = [|
        (code, Path.Combine(disk, $"Dev\{code}"), $"/root/Dev/{code}")
        ("Common", Path.Combine(disk, "Dev\Common"), $"/root/Dev/Common")
        ("JCS", Path.Combine(disk, "Dev\JCS"), $"/root/Dev/JCS")
    |]
    
    let mutable allSuccess = true
    
    repos |> Array.iter (fun (name, localPath, remotePath) ->
        if not (Directory.Exists localPath) then
            $"⚠ 本地目录不存在，跳过: {localPath}" |> yellow |> output
        else
            $"\n--- scp 推送 {name}: {localPath} -> {user}@{server}:{remotePath} ---" |> cyan |> output
            
            // 先确保远程目录存在
            let mkdirCmd = $"mkdir -p {remotePath}"
            bash output credential mkdirCmd |> ignore
            
            // Windows 路径需要转换：将反斜杠替换为正斜杠（scp 需要）
            let normalizedPath = localPath.Replace('\\', '/')
            let scpArgs = 
                $"{privateKeyArg} {portArg} -r -o StrictHostKeyChecking=no {controlOpts} " +
                $"\"{normalizedPath}/*\" {user}@{server}:{remotePath}/"
            
            $"  scp {localPath}\\* -> {server}:{remotePath}/" |> cyan |> output
            let result = execWithTimeout output "" "scp" scpArgs 600000  // 10分钟超时（大文件）
            
            // ── 自动修复：检测 ControlMaster socket 损坏则清理并重试 ──
            let result =
                if isSocketError result then
                    $"[SCP] 检测到 ControlMaster socket 损坏，自动清理并重试..." |> yellow |> output
                    cleanSshControlSockets output |> ignore
                    System.Threading.Thread.Sleep(500)
                    // 不带 ControlMaster 重试
                    let retryArgs =
                        let argsNoControl = [ privateKeyArg; portArg; "-r"; "-o StrictHostKeyChecking=no" ]
                                            |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s))
                                            |> String.concat " "
                        $"{argsNoControl} \"{normalizedPath}/*\" {user}@{server}:{remotePath}/"
                    execWithTimeout output "" "scp" retryArgs 600000
                else result
            
            let hasError = result.Contains("fatal") || result.Contains("Error") || 
                           result.Contains("Permission denied") || result.Contains("No such file") ||
                           result.Contains("Connection closed") || result.Contains("getsockname failed") ||
                           result.Contains("Unknown error") || result.Contains("connection unexpectedly")
            if hasError then
                $"❌ scp 推送 {name} 失败" |> red |> output
                result |> output
                allSuccess <- false
            else
                $"{name} 通过 scp 推送完成" |> green |> output
                if not (String.IsNullOrWhiteSpace result) then result |> output)
    
    if allSuccess then
        "✓ 所有仓库通过 scp 推送完成" |> green |> output
    else
        "⚠ 部分仓库 scp 推送失败，继续后续流程" |> yellow |> output
    
    allSuccess


// =================== 并行 Git Pull ===================

/// 检测远程服务器上 gh CLI 是否已认证（返回 true 表示可用 HTTPS 协议操作 GitHub）
let private checkGhAuthenticated output credential =
    try
        let result = bashWithTimeout output credential "gh auth status 2>&1" 10000
        let ok = result.Contains("Logged in to github.com")
        if ok then
            "✓ 远程 gh CLI 已认证，优先使用 HTTPS 协议操作 GitHub" |> green |> output
            true
        else
            false
    with _ -> false

/// 并行 git pull 三个仓库（智能跳过：远程无新提交则跳过）
/// 如果目录不是 git 仓库，先 clone；clone 失败时 fallback 到 scp 从本地推送
/// gh HTTPS 优先，SSH key 为备份方案
let parallelGitPull output credential (key__dir: Dictionary<string,string>) code (isScpPush: bool) disk =
    "\n--- 并行 git pull ---" |> cyan |> output
    let deployStampDir = "~/.deploy-stamps"
    
    // 检测远程 gh CLI 是否可用（仅非 scp 模式下检测，scp 模式不需要 git）
    let ghAvailable = not isScpPush && checkGhAuthenticated output credential
    
    // 如果 gh 可用，配置 git credential helper
    if ghAvailable then
        bash output credential "gh auth setup-git 2>/dev/null || true" |> ignore
    
    /// 根据 gh 可用性选择 repo URL（HTTPS 优先，SSH 备份）
    let selectRepoUrl name =
        if ghAvailable then getRepoUrlHttps name
        else getRepoUrl name
    
    /// clone 失败时，通过 scp 从本地直推源码到远程（fallback）
    let scpFallback (name: string) (dir: string) =
        $"\n⚠ [{name}] git clone 失败，改用 scp 从本地直推源码..." |> orange |> output
        let localPath = $"{disk}Dev/{name}".Replace('\\', '/')
        let privKeyArg = getSshPrivateKeyArg()
        let controlOpts = getSshControlOpts()
        let porto, user, server = credential
        let portArg = match porto with Some p -> $"-P {p}" | None -> ""
        let target = $"{user}@{server}"
        let remotePath = $"~/{dir}"
        let scpArgs =
            let args = [ privKeyArg; portArg; "-r"; "-o StrictHostKeyChecking=no"; controlOpts ]
                       |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s))
                       |> String.concat " "
            $"{args} \"{localPath}/*\" {target}:{remotePath}/"
        $"  scp {localPath}\\* -> {server}:{remotePath}/" |> cyan |> output
        let result = execWithTimeout output "" "scp" scpArgs 600000  // 10分钟超时
        
        // ── 自动修复：检测 ControlMaster socket 损坏则清理并重试 ──
        let result =
            if isSocketError result then
                $"[SCP] 检测到 ControlMaster socket 损坏，自动清理并重试..." |> yellow |> output
                cleanSshControlSockets output |> ignore
                System.Threading.Thread.Sleep(500)
                let retryArgs =
                    let argsNoControl = [ privKeyArg; portArg; "-r"; "-o StrictHostKeyChecking=no" ]
                                        |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s))
                                        |> String.concat " "
                    $"{argsNoControl} \"{localPath}/*\" {target}:{remotePath}/"
                execWithTimeout output "" "scp" retryArgs 600000
            else result
        
        if result.Contains("fatal") || result.Contains("Error") || result.Contains("No such file") ||
           result.Contains("Connection closed") || result.Contains("getsockname failed") ||
           result.Contains("Unknown error") || result.Contains("connection unexpectedly") then
            $"❌ [{name}] scp fallback 也失败了: {result}" |> red |> output
        else
            $"✓ [{name}] scp 推送完成" |> green |> output
            if not (String.IsNullOrWhiteSpace result) then result |> output
    
    let pullJob (name: string) (dir: string) =
        async {
            if isScpPush then
                $"[{name}] 代码已通过 scp 同步，跳过 git pull" |> output
            else
                // ✅ 检查目录是否存在且是 git 仓库
                let checkDirCmd = $"if [ -d ~/{dir} ] && [ -d ~/{dir}/.git ]; then echo 'IS_GIT'; else echo 'NOT_GIT'; fi"
                let dirCheck = bashWithRetry output credential checkDirCmd 10000 5
                
                if dirCheck <> "IS_GIT" then
                    // 目录不存在或不是 git 仓库 → clone
                    if dirCheck = "NOT_GIT" then
                        $"[{name}] 目录 ~/{dir} 不是 git 仓库，先 clone..." |> yellow |> output
                    else
                        $"[{name}] 目录 ~/{dir} 不存在，clone..." |> yellow |> output
                    
                    // 如果目录存在但不是 git 仓库，先删除
                    let cleanCmd = $"if [ -d ~/{dir} ] && [ ! -d ~/{dir}/.git ]; then rm -rf ~/{dir}; echo 'CLEANED'; fi"
                    bashWithRetry output credential cleanCmd 10000 5 |> ignore
                    
                    // clone 仓库
                    let repoUrl = selectRepoUrl name
                    // dir 格式: "Dev/WYI" → parentDir: "Dev", repoName: "WYI"
                    let lastSlash = dir.LastIndexOf('/')
                    let parentDir = if lastSlash > 0 then dir.Substring(0, lastSlash) else "."
                    let repoName = if lastSlash > 0 then dir.Substring(lastSlash + 1) else dir
                    let cloneCmd = $"cd ~/{parentDir} && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 clone {repoUrl} {repoName}"
                    $"[{name}] 执行: git clone {repoUrl} ~/{dir}" |> cyan |> output
                    let cloneResult = bashWithTimeout output credential cloneCmd 60000  // 1分钟超时
                    
                    let cloneFailed = 
                        cloneResult.Contains("fatal") || cloneResult.Contains("error") || 
                        cloneResult.Contains("Permission denied") || 
                        String.IsNullOrWhiteSpace cloneResult
                    if cloneFailed then
                        let reason = if String.IsNullOrWhiteSpace cloneResult then "超时无响应" else cloneResult
                        $"❌ [{name}] git clone 失败: {reason}" |> red |> output
                        scpFallback name dir
                    else
                        $"✓ [{name}] git clone 完成" |> green |> output
                else
                    // 目录是 git 仓库 → 检查远程是否有新提交
                    let checkCmd = $"""mkdir -p {deployStampDir}
STAMP_FILE={deployStampDir}/git-hash-{name}
OLD_HASH=$(cat $STAMP_FILE 2>/dev/null || echo 'NO_STAMP')
NEW_HASH=$(cd ~/{dir} && git ls-remote origin HEAD 2>/dev/null | cut -f1 | cut -c1-8 || echo 'FETCH_FAIL')
if [[ -z "$NEW_HASH" ]] || [[ "$NEW_HASH" = "FETCH_FAIL" ]]; then
    echo 'FETCH_FAIL'
elif [[ "$NEW_HASH" = "$OLD_HASH" ]]; then
    echo "SKIP:$NEW_HASH"
else
    echo "PULL:$NEW_HASH"
fi"""
                    let checkResult = bashWithRetry output credential checkCmd 15000 3
                    
                    if checkResult.StartsWith("SKIP:") then
                        let hash = checkResult.Substring(5)
                        $"[{name}] 远程无新提交 (hash: {hash})，跳过 git pull" |> output
                    elif checkResult.StartsWith("PULL:") then
                        let hash = checkResult.Substring(5)
                        $"[{name}] 远程有新提交 (hash: {hash})，git pull..." |> output
                        updateSingleRepo output credential name (selectRepoUrl name) dir |> ignore
                        $"[{name}] git pull 完成" |> output
                    else
                        // FETCH_FAIL → 可能是 origin 配置错误，尝试重新 clone
                        $"[{name}] 无法获取远程 hash（网络问题？），尝试重新 clone..." |> yellow |> output
                        let repoUrl = selectRepoUrl name
                        let cleanCmd = $"rm -rf ~/{dir}"
                        bashWithRetry output credential cleanCmd 10000 5 |> ignore
                        
                        // clone 仓库
                        let lastSlash = dir.LastIndexOf('/')
                        let parentDir = if lastSlash > 0 then dir.Substring(0, lastSlash) else "."
                        let repoName = if lastSlash > 0 then dir.Substring(lastSlash + 1) else dir
                        let cloneCmd = $"cd ~/{parentDir} && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 clone {repoUrl} {repoName}"
                        $"[{name}] 执行: git clone {repoUrl} ~/{dir}" |> cyan |> output
                        let cloneResult = bashWithTimeout output credential cloneCmd 60000  // 1分钟超时
                        
                        let cloneFailed = 
                            cloneResult.Contains("fatal") || cloneResult.Contains("error") || 
                            cloneResult.Contains("Permission denied") || 
                            String.IsNullOrWhiteSpace cloneResult
                        if cloneFailed then
                            let reason = if String.IsNullOrWhiteSpace cloneResult then "超时无响应" else cloneResult
                            $"❌ [{name}] git clone 失败: {reason}" |> red |> output
                            scpFallback name dir
                        else
                            $"✓ [{name}] git clone 完成" |> green |> output
        }
    
    // 串行执行 3 个 repo 的 git 操作，避免并行 SSH 连接风暴
    [ pullJob code key__dir["code"]
      pullJob "Common" key__dir["Common"]
      pullJob "JCS" key__dir["JCS"] ]
    |> List.iter (fun job -> job |> Async.RunSynchronously)
