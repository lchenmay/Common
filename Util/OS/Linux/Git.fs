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
    
    // 检查目录是否存在
    $"\n检查目录 ~/{targetDir} 是否存在..." |> cyan |> output
    let checkCmd = $"""
if [ -d ~/{targetDir} ]; then
    echo "EXISTS"
else
    echo "NOT_EXISTS"
fi
"""
    let checkResult = bash output credential checkCmd
    checkResult |> output
    
    if checkResult.Contains("NOT_EXISTS") then
        // 目录不存在，克隆仓库
        $"克隆 {repoName} 仓库到 ~/{targetDir}..." |> cyan |> output
        let cloneCmd = $"""
cd ~
git clone {repoUrl} {targetDir}
"""
        let cloneResult = bash output credential cloneCmd
        cloneResult |> output
        if cloneResult.Contains("Cloning into") then
            $"{repoName} 克隆成功" |> green |> output
            true
        else
            $"❌ {repoName} 克隆失败" |> red |> output
            false
    else
        // 目录存在，更新仓库
        $"更新 {repoName} 仓库..." |> cyan |> output
        
        // 1. fetch
        "  - git fetch --all" |> cyan |> output
        let fetchCmd = $"""
cd ~/{targetDir}
git fetch --all
"""
        let fetchResult = bash output credential fetchCmd
        fetchResult |> output
        
        // 2. reset
        "  - git reset --hard origin/main" |> cyan |> output
        let resetCmd = $"""
cd ~/{targetDir}
git reset --hard origin/main
"""
        let resetResult = bash output credential resetCmd
        resetResult |> output
        
        // 3. pull
        "  - git pull origin main" |> cyan |> output
        let pullCmd = $"""
cd ~/{targetDir}
git pull origin main
"""
        let pullResult = bash output credential pullCmd
        pullResult |> output
        
        if pullResult.Contains("Already up to date") then
            $"{repoName} 已是最新" |> green |> output
            true
        elif pullResult.Contains("fast-forward") || pullResult.Contains("Updated") then
            $"{repoName} 更新成功" |> green |> output
            true
        else
            $"⚠ {repoName} 更新可能有问题，尝试强制重置..." |> yellow |> output
            
            // 4. 强制重置
            "  - git fetch --all (强制)" |> cyan |> output
            let fetchForceCmd = $"""
cd ~/{targetDir}
git fetch --all
"""
            let fetchForceResult = bash output credential fetchForceCmd
            fetchForceResult |> output
            
            "  - git reset --hard origin/main (强制)" |> cyan |> output
            let resetForceCmd = $"""
cd ~/{targetDir}
git reset --hard origin/main
"""
            let resetForceResult = bash output credential resetForceCmd
            resetForceResult |> output
            
            if resetForceResult.Contains("HEAD is now at") then
                $"{repoName} 强制重置完成" |> green |> output
                true
            else
                $"❌ {repoName} 更新失败" |> red |> output
                false

/// 显示单个仓库状态（逐条执行）
let showRepoStatus output credential (repoName: string) (targetDir: string) =
    $"\n📁 {repoName} 状态:" |> cyan |> output
    
    // 切换到目标目录
    let cdCmd = $"cd ~/{targetDir}"
    bash output credential cdCmd |> ignore
    
    // 获取分支
    "  Branch:" |> cyan |> output
    let branchCmd = "git branch --show-current 2>/dev/null || echo 'N/A'"
    let branchResult = bash output credential branchCmd
    branchResult |> output
    
    // 获取提交哈希
    "  Commit:" |> cyan |> output
    let commitCmd = "git rev-parse --short HEAD 2>/dev/null || echo 'N/A'"
    let commitResult = bash output credential commitCmd
    commitResult |> output
    
    // 获取提交信息
    "  Message:" |> cyan |> output
    let msgCmd = "git log -1 --pretty=%s 2>/dev/null || echo 'N/A'"
    let msgResult = bash output credential msgCmd
    msgResult |> output