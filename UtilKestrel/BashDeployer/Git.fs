module UtilKestrel.BashDeployer.Git

open System
open System.IO
open System.Collections.Generic
open Util.Linux.Bash
open Util.Linux.Git
open Util.Monitor
open UtilKestrel.Types
open UtilKestrel.BashDeployer.Common

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
        "是否继续部署？(y/n): " |> yellow |> output
        let response = Console.ReadLine()
        if response <> "y" && response <> "Y" then
            "用户取消部署" |> red |> output
            failwith "Git push 失败，用户取消部署"
        else
            "⚠ 跳过 push 验证，继续部署（远程仓库可能不是最新）" |> yellow |> output
    
    success

/// 推送所有本地仓库变更（带验证和重试）
/// 返回：所有仓库是否都成功推送
let pushAllLocalRepos output code gitName gitEmail disk =
    $"\n=== 开始推送所有本地仓库变更 ===" |> yellow |> output
    
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
                $"{privateKeyArg} {portArg} -r -o StrictHostKeyChecking=no " +
                $"\"{normalizedPath}/*\" {user}@{server}:{remotePath}/"
            
            $"  scp {localPath}\\* -> {server}:{remotePath}/" |> cyan |> output
            let result = execWithTimeout output "" "scp" scpArgs 600000  // 10分钟超时（大文件）
            
            let hasError = result.Contains("fatal") || result.Contains("Error") || 
                           result.Contains("Permission denied") || result.Contains("No such file")
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

/// 并行 git pull 三个仓库（智能跳过：远程无新提交则跳过）
/// 如果目录不是 git 仓库，先 clone
let parallelGitPull output credential (key__dir: Dictionary<string,string>) code (isScpPush: bool) =
    "\n--- 并行 git pull ---" |> cyan |> output
    let deployStampDir = "~/.deploy-stamps"
    
    let pullJob (name: string) (dir: string) =
        async {
            if isScpPush then
                $"[{name}] 代码已通过 scp 同步，跳过 git pull" |> output
            else
                // ✅ 检查目录是否存在且是 git 仓库
                let checkDirCmd = $"if [ -d ~/{dir} ] && [ -d ~/{dir}/.git ]; then echo 'IS_GIT'; else echo 'NOT_GIT'; fi"
                let dirCheck = bash output credential checkDirCmd |> fun s -> s.Trim()
                
                if dirCheck <> "IS_GIT" then
                    // 目录不存在或不是 git 仓库 → clone
                    if dirCheck = "NOT_GIT" then
                        $"[{name}] 目录 ~/{dir} 不是 git 仓库，先 clone..." |> yellow |> output
                    else
                        $"[{name}] 目录 ~/{dir} 不存在，clone..." |> yellow |> output
                    
                    // 如果目录存在但不是 git 仓库，先删除
                    let cleanCmd = $"if [ -d ~/{dir} ] && [ ! -d ~/{dir}/.git ]; then rm -rf ~/{dir}; echo 'CLEANED'; fi"
                    bash output credential cleanCmd |> ignore
                    
                    // clone 仓库
                    let repoUrl = getRepoUrl name
                    // dir 格式: "Dev/WYI" → parentDir: "Dev", repoName: "WYI"
                    let lastSlash = dir.LastIndexOf('/')
                    let parentDir = if lastSlash > 0 then dir.Substring(0, lastSlash) else "."
                    let repoName = if lastSlash > 0 then dir.Substring(lastSlash + 1) else dir
                    let cloneCmd = $"cd ~/{parentDir} && git clone {repoUrl} {repoName}"
                    $"[{name}] 执行: git clone {repoUrl} ~/{dir}" |> cyan |> output
                    let cloneResult = bashWithTimeout output credential cloneCmd 120000  // 2分钟超时
                    
                    if cloneResult.Contains("fatal") || cloneResult.Contains("error") then
                        $"❌ [{name}] git clone 失败: {cloneResult}" |> red |> output
                    else
                        $"✓ [{name}] git clone 完成" |> green |> output
                else
                    // 目录是 git 仓库 → 检查远程是否有新提交
                    let checkCmd = $"""mkdir -p {deployStampDir}
STAMP_FILE={deployStampDir}/git-hash-{name}
OLD_HASH=$(cat $STAMP_FILE 2>/dev/null || echo 'NO_STAMP')
NEW_HASH=$(cd ~/{dir} && git ls-remote origin HEAD 2>/dev/null | cut -f1 | cut -c1-8 || echo 'FETCH_FAIL')
if [ -z "$NEW_HASH" ] || [ "$NEW_HASH" = "FETCH_FAIL" ]; then
    echo 'FETCH_FAIL'
elif [ "$NEW_HASH" = "$OLD_HASH" ]; then
    echo "SKIP:$NEW_HASH"
else
    echo "PULL:$NEW_HASH"
fi"""
                    let checkResult = bash output credential checkCmd |> fun s -> s.Trim()
                    
                    if checkResult.StartsWith("SKIP:") then
                        let hash = checkResult.Substring(5)
                        $"[{name}] 远程无新提交 (hash: {hash})，跳过 git pull" |> output
                    elif checkResult.StartsWith("PULL:") then
                        let hash = checkResult.Substring(5)
                        $"[{name}] 远程有新提交 (hash: {hash})，git pull..." |> output
                        updateSingleRepo output credential name (getRepoUrl name) dir |> ignore
                        $"[{name}] git pull 完成" |> output
                    else
                        // FETCH_FAIL → 可能是 origin 配置错误，尝试重新 clone
                        $"[{name}] 无法获取远程 hash（网络问题？），尝试重新 clone..." |> yellow |> output
                        let repoUrl = getRepoUrl name
                        let cleanCmd = $"rm -rf ~/{dir}"
                        bash output credential cleanCmd |> ignore
                        
                        // clone 仓库
                        let lastSlash = dir.LastIndexOf('/')
                        let parentDir = if lastSlash > 0 then dir.Substring(0, lastSlash) else "."
                        let repoName = if lastSlash > 0 then dir.Substring(lastSlash + 1) else dir
                        let cloneCmd = $"cd ~/{parentDir} && git clone {repoUrl} {repoName}"
                        $"[{name}] 执行: git clone {repoUrl} ~/{dir}" |> cyan |> output
                        let cloneResult = bashWithTimeout output credential cloneCmd 120000  // 2分钟超时
                        
                        if cloneResult.Contains("fatal") || cloneResult.Contains("error") then
                            $"❌ [{name}] git clone 失败: {cloneResult}" |> red |> output
                            $"[{name}] 跳过 git pull" |> yellow |> output
                        else
                            $"✓ [{name}] git clone 完成" |> green |> output
        }
    
    [ pullJob code key__dir["code"]
      pullJob "Common" key__dir["Common"]
      pullJob "JCS" key__dir["JCS"] ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
