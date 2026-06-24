module Util.Linux.Git

open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Text
open System.IO

open Util.Linux.Bash
open Util.Linux.Linux


/// 检查并克隆/更新单个仓库（逐条执行）
let updateSingleRepo output credential (repoName: string) (repoUrl: string) (targetDir: string) =
    $"\n--- 处理 {repoName} 仓库 ---" |> yellow |> output
    
    // 1. 检查目录是否存在
    $"\n检查目录 ~/{targetDir} 是否存在..." |> cyan |> output
    let checkDirCmd = $"if [ -d ~/{targetDir} ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
    let dirExists = bash output credential checkDirCmd
    dirExists |> output
    
    if dirExists.Contains("NOT_EXISTS") then
        // 目录不存在 → 创建目录并克隆
        $"目录不存在，创建 ~/{targetDir}..." |> cyan |> output
        let mkdirCmd = $"mkdir -p ~/{targetDir}"
        bash output credential mkdirCmd |> ignore
        
        $"克隆 {repoName} 仓库到 ~/{targetDir}..." |> cyan |> output
        let cloneCmd = $"cd ~ && git clone {repoUrl} {targetDir}"
        let cloneResult = bash output credential cloneCmd
        cloneResult |> output
        
        if cloneResult.Contains("Cloning into") || cloneResult.Contains("done") then
            $"{repoName} 克隆成功" |> green |> output
            true
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
        let isGit = bash output credential checkGitCmd
        isGit |> output
        
        if isGit.Contains("NOT_GIT") then
            // 目录存在但不是 git 仓库 → 先尝试 git init
            $"⚠ 目录存在但不是 Git 仓库，尝试初始化..." |> yellow |> output
            let initCmd = $"cd ~/{targetDir} && git init && git remote add origin {repoUrl}"
            let initResult = bash output credential initCmd
            initResult |> output
            
            // 然后 fetch 和 reset
            $"从远程拉取..." |> cyan |> output
            let fetchCmd = $"cd ~/{targetDir} && git fetch --all"
            let fetchResult = bashWithTimeout output credential fetchCmd 60000
            fetchResult |> output
            
            let resetCmd = $"cd ~/{targetDir} && git reset --hard origin/main"
            let resetResult = bash output credential resetCmd
            resetResult |> output
            
            if resetResult.Contains("HEAD is now at") then
                $"{repoName} 恢复成功" |> green |> output
                true
            else
                $"❌ {repoName} 恢复失败，尝试删除后重新克隆..." |> yellow |> output
                let rmCmd = $"rm -rf ~/{targetDir}"
                bash output credential rmCmd |> ignore
                
                $"克隆 {repoName} 仓库到 ~/{targetDir}..." |> cyan |> output
                let cloneCmd = $"cd ~ && git clone {repoUrl} {targetDir}"
                let cloneResult = bash output credential cloneCmd
                cloneResult |> output
                
                if cloneResult.Contains("Cloning into") || cloneResult.Contains("done") then
                    $"{repoName} 克隆成功" |> green |> output
                    true
                else
                    $"❌ {repoName} 克隆失败，请检查仓库地址: {repoUrl}" |> red |> output
                    false
        else
            // 是 git 仓库 → git pull 更新
            $"更新 {repoName} 仓库..." |> cyan |> output
            
            // 1. fetch（增加超时到 60s 适应慢速 GitHub 连接）
            "  - git fetch --all" |> cyan |> output
            let fetchCmd = $"cd ~/{targetDir} && git fetch --all"
            let fetchResult = bashWithTimeout output credential fetchCmd 60000
            fetchResult |> output
            
            // 2. reset --hard 确保与远程同步（fetch 后 reset 即可，无需单独 pull）
            "  - git reset --hard origin/main" |> cyan |> output
            let resetCmd = $"cd ~/{targetDir} && git reset --hard origin/main"
            let resetResult = bash output credential resetCmd
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