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


// ==================== Git 推送函数 ====================

/// 推送本地仓库变更（单个仓库）
let pushLocalRepo output (repoPath: string) (gitName: string) (gitEmail: string) =
    $"\n--- 推送 {repoPath} 仓库变更 ---" |> cyan |> output
    
    let cmds = [|
        $"cd {repoPath}"
        $"git config user.name \"{gitName}\""
        $"git config user.email \"{gitEmail}\""
        "git add ."
        "git commit -m \"auto-deploy\" || echo '没有变更需要提交'"
        "git push"
    |]
    
    for cmd in cmds do
        let result = exec output repoPath "powershell" cmd
        result |> output
    
    "✓ 推送完成" |> green |> output

/// 推送所有本地仓库变更
let pushAllLocalRepos output (code: string) (gitName: string) (gitEmail: string) (disk: string) =
    $"\n=== 开始推送所有本地仓库变更 ===" |> yellow |> output
    
    let repos = [
        ("主项目", $"{disk}Dev/{code}")
        ("Common", $"{disk}Dev/Common")
        ("JCS", $"{disk}Dev/JCS")
    ]
    
    for (name, path) in repos do
        pushLocalRepo output path gitName gitEmail |> ignore
        "\n" |> output
    
    "=== 所有本地仓库推送完成 ===" |> yellow |> output


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


// ==================== 构建函数 ====================

/// 构建前端（逐条执行）- 使用 code 参数
let buildFrontend output credential (code: string) =
    "\n--- 构建前端 ---" |> cyan |> output
    
    let vscodeDir = $"Dev/{code}/vscode"
    
    // 检查 package.json 是否存在
    "检查 package.json..." |> cyan |> output
    let checkPackageCmd = $"if [ -f ~/{vscodeDir}/package.json ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
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

/// 构建后端（逐条执行）- 使用 code 参数
let buildBackend output credential (code: string) =
    "\n--- 构建后端 ---" |> cyan |> output
    
    let serverDir = $"Dev/{code}/Server"
    
    // 检查 .fsproj 是否存在
    "检查项目文件..." |> cyan |> output
    let checkProjCmd = $"if ls ~/{serverDir}/*.fsproj 1> /dev/null 2>&1; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
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
        let commonCheckCmd = $"if [ -d ~/Dev/Common ]; then echo 'Common 存在'; else echo 'Common 不存在，跳过'; fi"
        let commonCheckResult = bash output credential commonCheckCmd
        commonCheckResult |> output
        
        let commonRefCmd = "dotnet add reference ~/Dev/Common/Common.fsproj 2>/dev/null || echo '引用已存在或添加失败'"
        let commonRefResult = bash output credential commonRefCmd
        commonRefResult |> output
        
        // 3. 添加 JCS 引用
        "  - 检查并添加 JCS 引用..." |> cyan |> output
        let jcsCheckCmd = $"if [ -d ~/Dev/JCS ]; then echo 'JCS 存在'; else echo 'JCS 不存在，跳过'; fi"
        let jcsCheckResult = bash output credential jcsCheckCmd
        jcsCheckResult |> output
        
        let jcsRefCmd = "dotnet add reference ~/Dev/JCS/JCS.fsproj 2>/dev/null || echo '引用已存在或添加失败'"
        let jcsRefResult = bash output credential jcsRefCmd
        jcsRefResult |> output
        
        // 4. dotnet build
        "  - dotnet build --configuration Release" |> cyan |> output
        let buildResult = bash output credential "dotnet build --configuration Release"
        buildResult |> output
        
        "✓ 后端构建完成" |> green |> output
        true


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
        // 6. 构建前端 - 使用 code 参数
        // ========================================
        "6. 构建前端..." |> cyan |> output
        buildFrontend output credential code |> ignore
        
        // ========================================
        // 7. 构建后端 - 使用 code 参数
        // ========================================
        "7. 构建后端..." |> cyan |> output
        buildBackend output credential code |> ignore
        
        // ========================================
        // 8. 启动服务 - 使用 code 参数
        // ========================================
        "8. 启动服务..." |> cyan |> output
        let serviceRunning = checkDotNetServiceRunning output credential code
        if serviceRunning then
            $"✓ {code} 服务已在运行" |> green |> output
            $"如需重启，请手动执行: sudo systemctl restart {code.ToLower()}" |> yellow |> output
        else
            startServiceVerbose output credential code |> ignore
        
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
📋 日志文件: /tmp/{code.ToLower()}.log
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
    let code = runtime.projectCode

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
    
    // 3. 推送本地所有仓库变更到 GitHub
    "3. 推送本地所有仓库变更到 GitHub..." |> cyan |> output
    pushAllLocalRepos output code host.deploy.gitName host.deploy.gitEmail host.disk
        
    // 4. 验证 PostgreSQL（确保服务运行）
    "4. 验证 PostgreSQL..." |> cyan |> output
    exeRemoteValidatePSQL output host.deploy.credential

    // 5. 配置 PostgreSQL 远程访问（仅当未配置时）
    "5. 配置 PostgreSQL 远程访问..." |> cyan |> output
    exeRemoteConfigurePSQL output host.deploy.credential host.deploy.postgresPwd

    // 6. 部署代码（从 GitHub 更新）
    "6. 部署代码..." |> cyan |> output
    exeDeployCode output host.deploy.credential code
        
    "\n✅ 部署流程完成!" |> green |> output