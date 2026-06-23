module UtilKestrel.BashDeployer

open System
open System.IO
open System.Threading
open System.Collections.Generic

open Util.Linux.Bash
open Util.Linux.Linux
open Util.Linux.PSQL
open Util.Linux.Git

open UtilKestrel.Types

let gitPush(gitName,gitEmail) = 
    [|  $"config user.name \"{gitName}\""
        $"config user.email \"{gitEmail}\""
        "add ."
        "commit -m \"auto-deploy\""
        "push" |]

let mutable pathGit = @"C:\Program Files\Git\bin\git.exe"

let remote(deployDir) = 
    [|  $"cd ~/{deployDir}"
        "git fetch --all"
        "git reset --hard origin/main"
        "sudo killall -9 dotnet || true"
        "sudo fuser -k 80/tcp || true"
        "sudo fuser -k 443/tcp || true"
        $"cd ~/{deployDir}/vscode"
        "/root/.bun/bin/bun install"
        "/root/.bun/bin/bun add vite @vitejs/plugin-vue @vitejs/plugin-vue-jsx @vitejs/plugin-basic-ssl -D"
        "/root/.bun/bin/bun generateRoutes.cjs"
        "/root/.bun/bin/bunx --/root/.bun/bin/bun vite build --emptyOutDir"
        "cd .."
        "cd Server"
        "sudo dotnet run" |]


// ==================== 目录管理函数 ====================

/// 删除所有仓库目录
let deleteAllRepos output credential (code: string) =
    $"\n=== 开始删除所有仓库目录 ===" |> yellow |> output
    
    let repos = [
        ("主项目", $"Dev/{code}")
        ("Common", "Dev/Common")
        ("JCS", "Dev/JCS")
        ("Dev 根目录", "Dev")
    ]
    
    for (name, path) in repos do
        deleteRemoteDir output credential path |> ignore
    
    "=== 所有仓库目录删除完成 ===" |> yellow |> output

// ==================== 检查和验证函数 ====================

/// 检查服务是否运行
let checkServiceRunning output credential =
    let checkCmd = "ps aux | grep -q '[d]otnet.*Aiarwa' && echo 'RUNNING' || echo 'NOT_RUNNING'"
    let result = bash output credential checkCmd
    
    if result.Contains("RUNNING") then
        "✓ Aiarwa 服务正在运行" |> green |> output
        true
    else
        "⚠ Aiarwa 服务未运行" |> yellow |> output
        false

/// 启动服务
let startService output credential (deployDir: string) =
    $"\n--- 启动 Aiarwa 服务 ---" |> cyan |> output
    
    let cmds = [|
        $"cd ~/{deployDir}/Server"
        "sudo killall -9 dotnet || true"
        "sudo fuser -k 80/tcp || true"
        "sudo fuser -k 443/tcp || true"
        "sudo nohup dotnet run > /tmp/aiarwa.log 2>&1 &"
        "sleep 3"
    |]
    
    cmds |> String.concat " && " |> bash output credential |> ignore
    
    // 验证服务是否启动
    let running = checkServiceRunning output credential
    if running then
        "✓ Aiarwa 服务启动成功" |> green |> output
        "日志文件: /tmp/aiarwa.log" |> yellow |> output
    else
        "❌ Aiarwa 服务启动失败，请检查日志" |> red |> output
    
    running


/// 构建前端（逐条执行）
let buildFrontend output credential (deployDir: string) =
    "\n--- 构建前端 ---" |> cyan |> output
    
    let vscodeDir = $"{deployDir}/vscode"
    
    // 检查 package.json 是否存在
    "检查 package.json..." |> cyan |> output
    let checkPackageCmd = $"""
if [ -f ~/{vscodeDir}/package.json ]; then
    echo "EXISTS"
else
    echo "NOT_EXISTS"
fi
"""
    let packageExists = bash output credential checkPackageCmd
    
    if packageExists.Contains("NOT_EXISTS") then
        "⚠ 未找到 package.json，跳过前端构建" |> yellow |> output
        false
    else
        "✓ package.json 存在" |> green |> output
        
        // 切换到 vscode 目录
        $"cd ~/{vscodeDir}" |> bash output credential |> ignore
        
        // 1. bun install
        "  - bun install" |> cyan |> output
        let installResult = bash output credential "/root/.bun/bin/bun install"
        installResult |> output
        
        // 2. bun add vite 插件
        "  - bun add vite 插件" |> cyan |> output
        let addResult = bash output credential "/root/.bun/bin/bun add vite @vitejs/plugin-vue @vitejs/plugin-vue-jsx @vitejs/plugin-basic-ssl -D || true"
        addResult |> output
        
        // 3. bun generateRoutes.cjs
        "  - bun generateRoutes.cjs" |> cyan |> output
        let generateResult = bash output credential "/root/.bun/bin/bun generateRoutes.cjs 2>/dev/null || true"
        generateResult |> output
        
        // 4. bun vite build
        "  - bun vite build" |> cyan |> output
        let buildResult = bash output credential "/root/.bun/bin/bunx --/root/.bun/bin/bun vite build --emptyOutDir || true"
        buildResult |> output
        
        "✓ 前端构建完成" |> green |> output
        true

/// 构建后端（逐条执行）
let buildBackend output credential (deployDir: string) =
    "\n--- 构建后端 ---" |> cyan |> output
    
    let serverDir = $"{deployDir}/Server"
    
    // 检查 .fsproj 是否存在
    "检查项目文件..." |> cyan |> output
    let checkProjCmd = $"""
if ls ~/{serverDir}/*.fsproj 1> /dev/null 2>&1; then
    echo "EXISTS"
else
    echo "NOT_EXISTS"
fi
"""
    let projExists = bash output credential checkProjCmd
    
    if projExists.Contains("NOT_EXISTS") then
        "⚠ 未找到 .fsproj 文件，跳过后端构建" |> yellow |> output
        false
    else
        "✓ 项目文件存在" |> green |> output
        
        // 切换到 server 目录
        $"cd ~/{serverDir}" |> bash output credential |> ignore
        
        // 1. dotnet restore
        "  - dotnet restore" |> cyan |> output
        let restoreResult = bash output credential "dotnet restore"
        restoreResult |> output
        
        // 2. 添加 Common 引用
        "  - 检查并添加 Common 引用..." |> cyan |> output
        let commonRefCmds = [|
            "if [ -d ~/Dev/Common ]; then echo 'Common 存在'; else echo 'Common 不存在，跳过'; exit 0; fi"
            "dotnet add reference ~/Dev/Common/Common.fsproj 2>/dev/null || echo '引用已存在或添加失败'"
        |]
        for cmd in commonRefCmds do
            let result = bash output credential cmd
            result |> output
        
        // 3. 添加 JCS 引用
        "  - 检查并添加 JCS 引用..." |> cyan |> output
        let jcsRefCmds = [|
            "if [ -d ~/Dev/JCS ]; then echo 'JCS 存在'; else echo 'JCS 不存在，跳过'; exit 0; fi"
            "dotnet add reference ~/Dev/JCS/JCS.fsproj 2>/dev/null || echo '引用已存在或添加失败'"
        |]
        for cmd in jcsRefCmds do
            let result = bash output credential cmd
            result |> output
        
        // 4. dotnet build
        "  - dotnet build --configuration Release" |> cyan |> output
        let buildResult = bash output credential "dotnet build --configuration Release"
        buildResult |> output
        
        "✓ 后端构建完成" |> green |> output
        true

/// 启动服务（逐条执行）
let startServiceVerbose output credential (deployDir: string) =
    "\n--- 启动服务 ---" |> cyan |> output
    
    let serverDir = $"{deployDir}/Server"
    
    // 切换到 server 目录
    $"cd ~/{serverDir}" |> bash output credential |> ignore
    
    // 1. 停止现有服务
    "  1. 停止现有服务..." |> cyan |> output
    
    let stopCmds = [|
        "sudo killall -9 dotnet || echo '没有运行中的 dotnet 进程'"
        "sudo fuser -k 80/tcp || echo '端口 80 未被占用'"
        "sudo fuser -k 443/tcp || echo '端口 443 未被占用'"
    |]
    for cmd in stopCmds do
        let result = bash output credential cmd
        result |> output
    
    // 2. 启动服务
    "  2. 启动服务..." |> cyan |> output
    let startCmd = "sudo nohup dotnet run > /tmp/aiarwa.log 2>&1 &"
    let startResult = bash output credential startCmd
    startResult |> output
    
    // 3. 等待启动
    "  3. 等待服务启动（3秒）..." |> cyan |> output
    bash output credential "sleep 3" |> ignore
    
    // 4. 验证服务是否运行
    "  4. 验证服务状态..." |> cyan |> output
    let running = checkServiceRunning output credential
    
    if running then
        "✓ 服务启动成功" |> green |> output
        "日志文件: /tmp/aiarwa.log" |> yellow |> output
    else
        "❌ 服务启动失败，请检查日志" |> red |> output
        // 显示日志末尾
        "--- 日志末尾 ---" |> yellow |> output
        let logCmd = "tail -20 /tmp/aiarwa.log 2>/dev/null || echo '日志文件不存在'"
        let logResult = bash output credential logCmd
        logResult |> output
    
    running

/// 部署代码（从 GitHub 更新所有仓库）- 逐条执行
let exeDeployCode
    output
    credential
    (code: string) =

    let porto,user,server,target,portArg = credentialExpand credential
    let devRoot = "Dev"

    try
        // ========================================
        // 1. 确保所有目录存在
        // ========================================
        "1. 检查并创建所有必要目录..." |> cyan |> output
        
        // 使用 Dictionary 存储路径
        let key__dir = new Dictionary<string,string>()
        key__dir["Dev"] <- devRoot
        key__dir["code"] <- devRoot + "/" + code
        key__dir["Common"] <- devRoot + "/Common"
        key__dir["JCS"] <- devRoot + "/JCS"

        // 定义需要检查的目录列表
        let dirs = [|
            ("Dev 根目录", key__dir["Dev"])
            ("主项目目录", key__dir["code"])
            ("Common 目录", key__dir["Common"])
            ("JCS 目录", key__dir["JCS"])
        |]
        
        let allDirsExist = ensureDirectories output credential dirs
        
        if not allDirsExist then
            "⚠ 部分目录创建失败，尝试继续..." |> yellow |> output
        
        // ========================================
        // 2. 更新主项目仓库
        // ========================================
        "2. 从 GitHub 更新主项目仓库..." |> cyan |> output
        updateSingleRepo output credential 
            code $"https://github.com/siduochen/{code}.git" key__dir["code"] |> ignore
        
        // ========================================
        // 3. 更新 Common 仓库
        // ========================================
        "3. 从 GitHub 更新 Common 仓库..." |> cyan |> output
        updateSingleRepo output credential 
            "Common" "https://github.com/lchenmay/Common.git" key__dir["Common"] |> ignore
        
        // ========================================
        // 4. 更新 JCS 仓库
        // ========================================
        "4. 从 GitHub 更新 JCS 仓库..." |> cyan |> output
        updateSingleRepo output credential 
            "JCS" "https://github.com/lchenmay/JCS.git" key__dir["JCS"] |> ignore
        
        // ========================================
        // 5. 显示所有仓库状态
        // ========================================
        "5. 显示所有仓库状态..." |> cyan |> output
        showRepoStatus output credential code key__dir["code"]
        showRepoStatus output credential "Common" key__dir["Common"]
        showRepoStatus output credential "JCS" key__dir["JCS"]
        
        // ========================================
        // 6. 构建前端
        // ========================================
        "6. 构建前端..." |> cyan |> output
        buildFrontend output credential key__dir["code"] |> ignore
        
        // ========================================
        // 7. 构建后端
        // ========================================
        "7. 构建后端..." |> cyan |> output
        buildBackend output credential key__dir["code"] |> ignore
        
        // ========================================
        // 8. 启动服务
        // ========================================
        "8. 启动服务..." |> cyan |> output
        let serviceRunning = checkServiceRunning output credential
        if serviceRunning then
            "✓ 服务已在运行" |> green |> output
            "如需重启，请手动执行: sudo systemctl restart aiarwa" |> yellow |> output
        else
            startServiceVerbose output credential key__dir["code"] |> ignore
        
        // ========================================
        // 9. 显示部署摘要
        // ========================================
        let summary = $"""
========================================
✅ {server} 代码部署完成
========================================
📁 部署目录结构:
   - {code}: ~/{key__dir["code"]}
   - Common: ~/{key__dir["Common"]}
   - JCS: ~/{key__dir["JCS"]}
🔗 PostgreSQL: Host={server};Port=5432;Username=postgres;Password=***
📋 日志文件: /tmp/aiarwa.log
========================================
"""
        summary |> cyan |> output
        
    with ex ->
        $"部署过程中发生错误: {ex.Message}" |> red |> output
        "请检查远程服务器状态" |> yellow |> output

// ==================== 主流程 ====================

let routine 
    runtime =

    let host = runtime.host
    let credential = host.deploy.credential
    let porto,user,server,target,portArg = credentialExpand credential
    let devDir = host.disk + "Dev/" + runtime.projectCode
    let output = runtime.output

    // 设置 SSH 私钥路径
    sshPrivateKeyPath <- devDir + "/id_rsa"
    
    $">>> 开始部署至 {user}@{server}..." |> cyan |> output
        
    // 1. 本地：切换目录
    "1. 切换到项目目录: " + devDir |> cyan |> output
    let exeLocal args = exec output devDir "powershell" args |> ignore
    "cd " + devDir |> exeLocal
        
    // 2. 检查 SSH 免密登录是否已配置
    "2. 检查 SSH 免密登录状态..." |> cyan |> output
    checkSSHAuth output credential (host.disk + runtime.projectCode, host.deploy.gitEmail)
        
    // 3. 验证 PostgreSQL（确保服务运行）
    "3. 验证 PostgreSQL..." |> cyan |> output
    exeRemoteValidatePSQL output host.deploy.credential

    // 4. 配置 PostgreSQL 远程访问（仅当未配置时）
    "4. 配置 PostgreSQL 远程访问..." |> cyan |> output
    exeRemoteConfigurePSQL output host.deploy.credential host.deploy.postgresPwd

    // 5. 部署代码（从 GitHub 更新）
    "5. 部署代码..." |> cyan |> output
    exeDeployCode output host.deploy.credential runtime.projectCode
        
    "\n✅ 部署流程完成!" |> green |> output