module Util.Linux.Git

open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Text
open System.IO

open Util.Linux.Bash
open Util.Linux.Linux


/// 判断 git 操作结果是否成功
let private isGitSuccess (result: string) =
    result.Contains("Cloning into") || result.Contains("done") || 
    result.Contains("HEAD is now at") || result.Contains("Already up to date")

/// 判断 git 操作是否明确失败（超时、网络错误、认证失败）
let private isGitFailure (result: string) =
    result.Contains("fatal:") || result.Contains("Connection timed out") ||
    result.Contains("Could not resolve host") || result.Contains("Failed to connect") ||
    result.Contains("命令执行超时") || result.Contains("Permission denied") ||
    result.Contains("Repository not found")

/// 通过 scp 从源服务器同步代码到目标服务器（GitHub 不可用时的 fallback）
let private syncViaScp output credential (repoName: string) (targetDir: string) =
    $"\n⚠ GitHub 不可用，改用 scp 从源服务器同步 {repoName}..." |> orange |> output
    
    let porto, user, server = credential
    let portArg = match porto with Some p -> $"-P {p}" | None -> ""
    let privateKeyArg = getSshPrivateKeyArg()
    let controlOpts = getSshControlOpts()
    
    // 从同一服务器的 ~/source/{repoName}/ 目录同步
    // 假设源服务器上已有代码（可能是之前通过其他方式部署的）
    let sourcePath = $"~/source/{repoName}/"
    let destPath = $"~/{targetDir}/"
    
    // 先确保目标目录存在
    let mkdirCmd = $"mkdir -p {destPath}"
    bash output credential mkdirCmd |> ignore
    
    // rsync 首选（增量同步），scp 备用
    let scpArgs = 
        $"{privateKeyArg} {portArg} -r -o StrictHostKeyChecking=no {controlOpts} " +
        $"{user}@{server}:{sourcePath}* {destPath}"
    
    $"  scp {user}@{server}:{sourcePath}* -> {destPath}" |> cyan |> output
    let result = execWithTimeout output "" "scp" scpArgs 300000  // 5分钟超时
    result |> output
    
    if result.Contains("No such file") || result.Contains("not found") then
        $"❌ scp 同步 {repoName} 失败：源目录 {sourcePath} 不存在" |> red |> output
        false
    elif String.IsNullOrWhiteSpace result then
        $"⚠ scp 同步 {repoName} 可能成功（无输出），请检查目标目录" |> yellow |> output
        true
    else
        let hasError = result.Contains("fatal") || result.Contains("Error") || result.Contains("Permission denied")
        if hasError then
            $"❌ scp 同步 {repoName} 失败" |> red |> output
            false
        else
            $"{repoName} 通过 scp 同步完成" |> green |> output
            true


/// 检查并克隆/更新单个仓库（逐条执行）
/// 如果 GitHub 不可用，自动 fallback 到 scp
let updateSingleRepo output credential (repoName: string) (repoUrl: string) (targetDir: string) =
    $"\n--- 处理 {repoName} 仓库 ---" |> yellow |> output
    
    // 1. 检查目录是否存在（轻量命令，10s足够）
    $"\n检查目录 ~/{targetDir} 是否存在..." |> cyan |> output
    let checkDirCmd = $"if [ -d ~/{targetDir} ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
    let dirExists = bashWithRetry output credential checkDirCmd 10000 3
    dirExists |> output
    
    if dirExists.Contains("NOT_EXISTS") then
        // 目录不存在 → 创建目录并克隆
        $"目录不存在，创建 ~/{targetDir}..." |> cyan |> output
        let mkdirCmd = $"mkdir -p ~/{targetDir}"
        bash output credential mkdirCmd |> ignore
        
        $"克隆 {repoName} 仓库到 ~/{targetDir}..." |> cyan |> output
        let cloneCmd = $"cd ~ && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 clone {repoUrl} {targetDir}"
        let cloneResult = bashWithTimeout output credential cloneCmd 120000  // 2分钟超时
        cloneResult |> output
        
        if isGitSuccess cloneResult then
            $"{repoName} 克隆成功" |> green |> output
            true
        elif isGitFailure cloneResult then
            // GitHub 不可用 → fallback 到 scp
            $"⚠ git clone 失败，GitHub 可能不可用" |> yellow |> output
            syncViaScp output credential repoName targetDir
        else
            $"❌ {repoName} 克隆失败，请检查仓库地址: {repoUrl}" |> red |> output
            // 删除空目录
            let rmCmd = $"rm -rf ~/{targetDir}"
            bash output credential rmCmd |> ignore
            false
    else
        // 目录存在 → 检查是否是 git 仓库
        $"目录存在，检查是否是 Git 仓库..." |> cyan |> output
        let checkGitCmd = $"if [ -d ~/{targetDir}/.git ]; then echo 'IS_GIT'; else echo 'NOT_GIT'; fi"
        let isGit = bashWithRetry output credential checkGitCmd 10000 3
        isGit |> output
        
        if isGit.Contains("NOT_GIT") then
            // 目录存在但不是 git 仓库 → 先尝试 git init
            $"⚠ 目录存在但不是 Git 仓库，尝试初始化..." |> yellow |> output
            let initCmd = $"cd ~/{targetDir} && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 init && git remote add origin {repoUrl}"
            let initResult = bash output credential initCmd
            initResult |> output
            
            // 然后 fetch 和 reset
            $"从远程拉取..." |> cyan |> output
            let fetchCmd = $"cd ~/{targetDir} && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 fetch --all"
            let fetchResult = bashWithTimeout output credential fetchCmd 60000
            fetchResult |> output
            
            if isGitFailure fetchResult then
                $"⚠ git fetch 失败，GitHub 可能不可用，改用 scp 同步" |> yellow |> output
                syncViaScp output credential repoName targetDir
            else
                let resetCmd = $"cd ~/{targetDir} && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 reset --hard origin/main"
                let resetResult = bashWithTimeout output credential resetCmd 30000
                resetResult |> output
                
                if resetResult.Contains("HEAD is now at") then
                    $"{repoName} 恢复成功" |> green |> output
                    true
                else
                    $"❌ {repoName} 恢复失败，尝试删除后重新克隆..." |> yellow |> output
                    let rmCmd = $"rm -rf ~/{targetDir}"
                    bash output credential rmCmd |> ignore
                    
                    $"克隆 {repoName} 仓库到 ~/{targetDir}..." |> cyan |> output
                    let cloneCmd = $"cd ~ && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 clone {repoUrl} {targetDir}"
                    let cloneResult = bashWithTimeout output credential cloneCmd 120000  // 2分钟超时
                    cloneResult |> output
                    
                    if isGitSuccess cloneResult then
                        $"{repoName} 克隆成功" |> green |> output
                        true
                    elif isGitFailure cloneResult then
                        $"⚠ git clone 失败，GitHub 可能不可用，改用 scp 同步" |> yellow |> output
                        syncViaScp output credential repoName targetDir
                    else
                        $"❌ {repoName} 克隆失败，请检查仓库地址: {repoUrl}" |> red |> output
                        false
        else
            // 是 git 仓库 → git pull 更新
            $"更新 {repoName} 仓库..." |> cyan |> output
            
            // 1. fetch（-c gc.auto=0 防止后台 gc 导致 SSH 进程挂起不退出）
            "  - git fetch --all" |> cyan |> output
            let fetchCmd = $"cd ~/{targetDir} && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 fetch --all"
            let fetchResult = bashWithTimeout output credential fetchCmd 60000
            fetchResult |> output
            
            if isGitFailure fetchResult then
                $"⚠ git fetch 失败，GitHub 可能不可用，改用 scp 同步" |> yellow |> output
                syncViaScp output credential repoName targetDir
            else
                // 2. reset --hard 确保与远程同步（fetch 后 reset 即可，无需单独 pull）
                "  - git reset --hard origin/main" |> cyan |> output
                let resetCmd = $"cd ~/{targetDir} && GIT_TERMINAL_PROMPT=0 git -c gc.auto=0 reset --hard origin/main"
                let resetResult = bashWithTimeout output credential resetCmd 30000
                resetResult |> output
                
                if resetResult.Contains("HEAD is now at") then
                    $"{repoName} 已更新到最新" |> green |> output
                else
                    $"⚠ {repoName} 更新完成（请检查输出）" |> yellow |> output
                
                true

/// 显示单个仓库状态（逐条执行）- 修复 cd 命令问题
let showRepoStatus output credential (repoName: string) (targetDir: string) =
    $"\n📁 {repoName} 状态:" |> cyan |> output
    
    // 检查目录是否存在
    let checkCmd = $"if [ -d ~/{targetDir} ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
    let checkResult = bash output credential checkCmd
    
    if checkResult.Contains("NOT_EXISTS") then
        $"⚠ 目录 ~/{targetDir} 不存在" |> yellow |> output
    else
        // 检查是否是 git 仓库
        let checkGitCmd = $"if [ -d ~/{targetDir}/.git ]; then echo 'IS_GIT'; else echo 'NOT_GIT'; fi"
        let isGit = bash output credential checkGitCmd
        
        if isGit.Contains("NOT_GIT") then
            $"⚠ ~/{targetDir} 不是 Git 仓库" |> yellow |> output
        else
            // 使用 && 连接命令，确保在正确目录下执行
            // 获取分支
            "  Branch:" |> cyan |> output
            let branchCmd = $"cd ~/{targetDir} && git branch --show-current 2>/dev/null || echo 'N/A'"
            let branchResult = bash output credential branchCmd
            branchResult |> output
            
            // 获取提交哈希
            "  Commit:" |> cyan |> output
            let commitCmd = $"cd ~/{targetDir} && git rev-parse --short HEAD 2>/dev/null || echo 'N/A'"
            let commitResult = bash output credential commitCmd
            commitResult |> output
            
            // 获取提交信息
            "  Message:" |> cyan |> output
            let msgCmd = "cd ~/" + targetDir + " && git log -1 --pretty=%s 2>/dev/null || echo 'N/A'"
            let msgResult = bash output credential msgCmd
            msgResult |> output