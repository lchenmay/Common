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

/// 推送本地仓库变更（单个仓库）- 使用分号分隔，兼容 Windows PowerShell
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
    
    "✓ 推送完成" |> green |> output

/// 推送所有本地仓库变更
let pushAllLocalRepos output code gitName gitEmail disk =
    $"\n=== 开始推送所有本地仓库变更 ===" |> yellow |> output
    
    let repos = [|
        ("主项目", $"{disk}Dev/{code}")
        ("Common", $"{disk}Dev/Common")
        ("JCS", $"{disk}Dev/JCS")
    |]
    
    repos |> Array.iter (fun (name, path) ->
        if Directory.Exists(path) then
            pushLocalRepo output path gitName gitEmail |> ignore
        else
            $"⚠ 目录不存在: {path}" |> yellow |> output
        "\n" |> output)
    
    "=== 所有本地仓库推送完成 ===" |> yellow |> output


// ==================== 环境检查函数 ====================

/// 检查并安装 Node.js
let ensureNodeInstalled output credential =
    "\n--- 检查 Node.js ---" |> cyan |> output
    
    // 检查 node 是否已安装
    let checkNodeCmd = "command -v node > /dev/null 2>&1 && echo 'INSTALLED' || echo 'NOT_INSTALLED'"
    let nodeStatus = bash output credential checkNodeCmd
    
    if nodeStatus.Contains("INSTALLED") then
        // 显示版本
        let versionCmd = "node --version 2>/dev/null || echo 'unknown'"
        let version = bash output credential versionCmd
        $"✓ Node.js 已安装: {version.Trim()}" |> green |> output
        true
    else
        "⚠ Node.js 未安装，正在安装..." |> yellow |> output
        
        // 使用 nodesource 安装 Node.js 18.x
        let installCmds = [|
            "curl -fsSL https://rpm.nodesource.com/setup_18.x | bash -"
            "yum install -y nodejs || dnf install -y nodejs"
        |]
        
        installCmds |> Array.iter (fun cmd ->
            let result = bash output credential cmd
            result |> output)
        
        // 验证安装
        let verifyCmd = "command -v node > /dev/null 2>&1 && echo 'INSTALLED' || echo 'NOT_INSTALLED'"
        let verifyResult = bash output credential verifyCmd
        
        if verifyResult.Contains("INSTALLED") then
            let versionCmd = "node --version 2>/dev/null || echo 'unknown'"
            let version = bash output credential versionCmd
            $"✓ Node.js 安装成功: {version.Trim()}" |> green |> output
            true
        else
            "❌ Node.js 安装失败" |> red |> output
            false

/// 检查并安装 Bun（修复版）
let ensureBunInstalled output credential =
    "\n--- 检查 Bun ---" |> cyan |> output
    
    // 检查 bun 是否已安装 - 检查实际文件是否存在
    let checkBunCmd = "if [ -f /root/.bun/bin/bun ] && [ -x /root/.bun/bin/bun ]; then echo 'INSTALLED'; else echo 'NOT_INSTALLED'; fi"
    let bunStatus = bash output credential checkBunCmd
    
    if bunStatus.Contains("INSTALLED") then
        // 显示版本
        let versionCmd = "/root/.bun/bin/bun --version 2>/dev/null || echo 'unknown'"
        let version = bash output credential versionCmd
        $"✓ Bun 已安装: {version.Trim()}" |> green |> output
        true
    else
        "⚠ Bun 未安装，正在安装..." |> yellow |> output
        
        // 先安装 Node.js（Bun 安装脚本可能需要）
        ensureNodeInstalled output credential |> ignore
        
        // 安装 bun - 使用官方脚本
        let installCmd = "curl -fsSL https://bun.sh/install | bash"
        let installResult = bash output credential installCmd
        installResult |> output
        
        // 创建软链接到 /usr/local/bin
        let linkCmd = "ln -sf /root/.bun/bin/bun /usr/local/bin/bun 2>/dev/null || true"
        bash output credential linkCmd |> ignore
        
        // 验证安装 - 检查实际文件
        let verifyCmd = "if [ -f /root/.bun/bin/bun ] && [ -x /root/.bun/bin/bun ]; then echo 'INSTALLED'; else echo 'NOT_INSTALLED'; fi"
        let verifyResult = bash output credential verifyCmd
        
        if verifyResult.Contains("INSTALLED") then
            let versionCmd = "/root/.bun/bin/bun --version 2>/dev/null || echo 'unknown'"
            let version = bash output credential versionCmd
            $"✓ Bun 安装成功: {version.Trim()}" |> green |> output
            true
        else
            "❌ Bun 安装失败，尝试使用 npm 安装..." |> yellow |> output
            // 备用方案：使用 npm 安装 bun
            let npmInstallCmd = "npm install -g bun"
            let npmResult = bash output credential npmInstallCmd
            npmResult |> output
            
            // 再次验证
            let retryVerifyCmd = "if command -v bun > /dev/null 2>&1; then echo 'INSTALLED'; else echo 'NOT_INSTALLED'; fi"
            let retryResult = bash output credential retryVerifyCmd
            if retryResult.Contains("INSTALLED") then
                "✓ Bun 通过 npm 安装成功" |> green |> output
                true
            else
                "❌ Bun 安装失败" |> red |> output
                false

/// 确保环境就绪（Node.js + Bun）
let ensureEnvironment output credential =
    "\n=== 确保环境就绪 ===" |> cyan |> output
    
    let nodeOk = ensureNodeInstalled output credential
    let bunOk = ensureBunInstalled output credential
    
    if nodeOk && bunOk then
        "✓ 环境就绪" |> green |> output
        true
    else
        "⚠ 部分环境安装失败" |> yellow |> output
        false


// ==================== 目录管理函数 ====================

/// 删除所有仓库目录
let deleteAllRepos output credential code =
    $"\n=== 开始删除所有仓库目录 ===" |> yellow |> output
    
    let repos = [|
        ("主项目", $"Dev/{code}")
        ("Common", "Dev/Common")
        ("JCS", "Dev/JCS")
        ("Dev 根目录", "Dev")
    |]
    
    repos |> Array.iter (fun (name, path) ->
        deleteRemoteDir output credential path |> ignore)
    
    "=== 所有仓库目录删除完成 ===" |> yellow |> output


// ==================== 构建函数 ====================

/// 构建前端（逐条执行）- 使用 code 参数
let buildFrontend output credential code =
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
        
        // 检查 bun 是否存在，如果不存在则使用 npm
        "  - 安装依赖..." |> cyan |> output
        
        let checkBunCmd = "if [ -f /root/.bun/bin/bun ]; then echo 'BUN_EXISTS'; else echo 'BUN_NOT_EXISTS'; fi"
        let bunExists = bash output credential checkBunCmd
        
        if bunExists.Contains("BUN_EXISTS") then
            "  使用 Bun 安装..." |> cyan |> output
            let installResult = bash output credential $"cd ~/{vscodeDir} && /root/.bun/bin/bun install"
            installResult |> output
            
            // bun add vite 插件
            let addResult = bash output credential $"cd ~/{vscodeDir} && /root/.bun/bin/bun add vite @vitejs/plugin-vue @vitejs/plugin-vue-jsx @vitejs/plugin-basic-ssl -D || true"
            addResult |> output
            
            // bun generateRoutes.cjs
            let generateResult = bash output credential $"cd ~/{vscodeDir} && /root/.bun/bin/bun generateRoutes.cjs 2>/dev/null || true"
            generateResult |> output
            
            // bun vite build
            let buildResult = bash output credential $"cd ~/{vscodeDir} && /root/.bun/bin/bunx vite build --emptyOutDir || true"
            buildResult |> output
        else
            "  使用 npm 安装..." |> cyan |> output
            let npmResult = bash output credential $"cd ~/{vscodeDir} && npm install"
            npmResult |> output
            
            let buildResult = bash output credential $"cd ~/{vscodeDir} && npm run build 2>/dev/null || echo 'npm run build 不存在，跳过'"
            buildResult |> output
        
        "✓ 前端构建完成" |> green |> output
        true

/// 构建后端（逐条执行）- 使用 code 参数
let buildBackend output credential code =
    "\n--- 构建后端 ---" |> cyan |> output
    
    let serverDir = $"Dev/{code}/Server"
    
    // 检查 dotnet SDK 是否安装
    "检查 .NET SDK..." |> cyan |> output
    let dotnetCheckCmd = "command -v dotnet > /dev/null 2>&1 && dotnet --version 2>/dev/null || echo 'NOT_INSTALLED'"
    let dotnetVersion = bash output credential dotnetCheckCmd
    
    if dotnetVersion.Contains("NOT_INSTALLED") then
        "⚠ .NET SDK 未安装，尝试自动安装..." |> yellow |> output
        
        // 自动检测 Ubuntu 版本并安装 .NET 10.0 SDK
        let installDotnetCmds = [|
            "UBUNTU_VERSION=$(lsb_release -rs) && wget https://packages.microsoft.com/config/ubuntu/${UBUNTU_VERSION}/packages-microsoft-prod.deb -O /tmp/packages-microsoft-prod.deb"
            "dpkg -i /tmp/packages-microsoft-prod.deb"
            "apt update"
            "apt install -y dotnet-sdk-10.0"
            "rm /tmp/packages-microsoft-prod.deb"
        |]
        let installResult = bashMultiple output credential installDotnetCmds
        installResult |> output
        
        // 重新验证
        let retryCheckCmd = "command -v dotnet > /dev/null 2>&1 && dotnet --version 2>/dev/null || echo 'NOT_INSTALLED'"
        let retryVersion = bash output credential retryCheckCmd
        
        if retryVersion.Contains("NOT_INSTALLED") then
            "❌ .NET SDK 自动安装失败，请手动安装后重新部署" |> red |> output
            "手动安装命令: UBUNTU_VERSION=$(lsb_release -rs) && wget https://packages.microsoft.com/config/ubuntu/${UBUNTU_VERSION}/packages-microsoft-prod.deb -O /tmp/packages-microsoft-prod.deb && dpkg -i /tmp/packages-microsoft-prod.deb && apt update && apt install -y dotnet-sdk-10.0" |> yellow |> output
            false
        else
            $"✓ .NET SDK 自动安装成功: {retryVersion.Trim()}" |> green |> output
            // 继续执行后续构建
            let projCheckCmd = $"if ls ~/{serverDir}/*.fsproj 1> /dev/null 2>&1; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
            let projExists = bash output credential projCheckCmd
            if projExists.Contains("NOT_EXISTS") then
                "⚠ 未找到 .fsproj 文件，跳过后端构建" |> yellow |> output
                false
            else
                "✓ 项目文件存在" |> green |> output
                
                // 1. dotnet restore (suppress NU1603 FSharp.Core version warnings)
                "  - dotnet restore" |> cyan |> output
                let restoreResult = bash output credential $"cd ~/{serverDir} && dotnet restore --verbosity quiet"
                if restoreResult.Trim().Length > 0 then restoreResult |> output
                
                // 2. 添加正确的 Common 子项目引用 (suppress stderr noise)
                "  - 添加项目引用..." |> cyan |> output
                let utilRefCmd = $"cd ~/{serverDir} && dotnet add reference ~/Dev/Common/Util/Util.fsproj 2>/dev/null; dotnet add reference ~/Dev/Common/UtilKestrel/UtilKestrel.fsproj 2>/dev/null; dotnet add reference ~/Dev/JCS/JCS.Shared/JCS.Shared.fsproj 2>/dev/null; dotnet add reference ~/Dev/JCS/JCS.BizLogics/JCS.BizLogics.fsproj 2>/dev/null; echo '✓ 项目引用检查完成'"
                let refResult = bash output credential utilRefCmd
                refResult |> output
                
                // 3. dotnet build (suppress NU1603 warnings)
                "  - dotnet build --configuration Release" |> cyan |> output
                let buildResult = bash output credential $"cd ~/{serverDir} && dotnet build --configuration Release --verbosity normal /nowarn:NU1603"
                buildResult |> output
                
                "✓ 后端构建完成" |> green |> output
                true
    else
        $"✓ .NET SDK 已安装: {dotnetVersion.Trim()}" |> green |> output
        
        // 检查 .fsproj 是否存在
        "检查项目文件..." |> cyan |> output
        let checkProjCmd = $"if ls ~/{serverDir}/*.fsproj 1> /dev/null 2>&1; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
        let projExists = bash output credential checkProjCmd
        
        if projExists.Contains("NOT_EXISTS") then
            "⚠ 未找到 .fsproj 文件，跳过后端构建" |> yellow |> output
            false
        else
            "✓ 项目文件存在" |> green |> output
            
            // 1. dotnet restore (suppress NU1603)
            "  - dotnet restore" |> cyan |> output
            let restoreResult = bash output credential $"cd ~/{serverDir} && dotnet restore --verbosity quiet"
            if restoreResult.Trim().Length > 0 then restoreResult |> output
            
            // 2. 添加项目引用 (suppress stderr noise)
            "  - 添加项目引用..." |> cyan |> output
            let refCmd = $"cd ~/{serverDir} && dotnet add reference ~/Dev/Common/Util/Util.fsproj 2>/dev/null; dotnet add reference ~/Dev/Common/UtilKestrel/UtilKestrel.fsproj 2>/dev/null; dotnet add reference ~/Dev/JCS/JCS.Shared/JCS.Shared.fsproj 2>/dev/null; dotnet add reference ~/Dev/JCS/JCS.BizLogics/JCS.BizLogics.fsproj 2>/dev/null; echo '✓ 项目引用检查完成'"
            let refResult = bash output credential refCmd
            refResult |> output
            
            // 3. dotnet build (suppress NU1603)
            "  - dotnet build --configuration Release" |> cyan |> output
            let buildResult = bash output credential $"cd ~/{serverDir} && dotnet build --configuration Release --verbosity normal /nowarn:NU1603"
            buildResult |> output
            
            "✓ 后端构建完成" |> green |> output
            true


/// 获取仓库 URL
let getRepoUrl code =
    match code with
    | "Aiarwa" -> "https://github.com/lchenmay/Aiarwa.git"
    | "Common" -> "https://github.com/lchenmay/Common.git"
    | "JCS" -> "https://github.com/lchenmay/JCS.git"
    | _ -> $"https://github.com/siduochen/{code}.git"

/// 部署代码（从 GitHub 更新所有仓库）- 逐条执行
let exeDeployCode
    output
    credential
    code
    (logPath: string option) =

    let porto,user,server,target,portArg = credentialExpand credential
    let devRoot = "Dev"

    // 如果指定了日志路径，创建双输出函数（同时写控制台和日志文件）
    let output =
        match logPath with
        | Some path ->
            let dir = Path.GetDirectoryName(path)
            if not (String.IsNullOrEmpty(dir)) && not (Directory.Exists(dir)) then
                Directory.CreateDirectory(dir) |> ignore
            fun (msg: string) ->
                output msg
                try File.AppendAllText(path, msg + Environment.NewLine) with _ -> ()
        | None -> output

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
        
        if allDirsExist |> not then
            "⚠ 部分目录创建失败，尝试继续..." |> yellow |> output
        
        // ========================================
        // 2. 更新主项目仓库
        // ========================================
        "2. 从 GitHub 更新主项目仓库..." |> cyan |> output
        updateSingleRepo output credential 
            code (getRepoUrl code) key__dir["code"] |> ignore
        
        // ========================================
        // 3. 更新 Common 仓库
        // ========================================
        "3. 从 GitHub 更新 Common 仓库..." |> cyan |> output
        updateSingleRepo output credential 
            "Common" (getRepoUrl "Common") key__dir["Common"] |> ignore
        
        // ========================================
        // 4. 更新 JCS 仓库
        // ========================================
        "4. 从 GitHub 更新 JCS 仓库..." |> cyan |> output
        updateSingleRepo output credential 
            "JCS" (getRepoUrl "JCS") key__dir["JCS"] |> ignore
        
        // ========================================
        // 5. 确保环境就绪（Node.js + Bun）
        // ========================================
        "5. 确保环境就绪..." |> cyan |> output
        ensureEnvironment output credential |> ignore
        
        // ========================================
        // 6. 更新项目依赖（在 git pull 之后）
        // ========================================
        "6. 更新项目依赖..." |> cyan |> output
        
        // 前端依赖
        let vscodeDir = key__dir["code"] + "/vscode"
        let updateFrontendCmd = $"""
cd ~/{vscodeDir}
if [ -f package.json ]; then
    if [ -f /root/.bun/bin/bun ]; then
        /root/.bun/bin/bun install 2>/dev/null || echo 'bun install 跳过'
    else
        npm install 2>/dev/null || echo 'npm install 跳过'
    fi
    echo '前端依赖更新完成'
else
    echo '未找到 package.json，跳过'
fi
"""
        let updateResult = bash output credential updateFrontendCmd
        updateResult |> output
        
        // 后端依赖
        let serverDir = key__dir["code"] + "/Server"
        let updateBackendCmd = $"""
cd ~/{serverDir}
if ls *.fsproj 1>/dev/null 2>&1; then
    dotnet restore --verbosity quiet 2>/dev/null || echo 'dotnet restore 跳过'
    echo '后端依赖更新完成'
else
    echo '未找到 .fsproj，跳过'
fi
"""
        let updateBackendResult = bash output credential updateBackendCmd
        updateBackendResult |> output
        
        // ========================================
        // 7. 显示所有仓库状态
        // ========================================
        "7. 显示所有仓库状态..." |> cyan |> output
        showRepoStatus output credential code key__dir["code"]
        showRepoStatus output credential "Common" key__dir["Common"]
        showRepoStatus output credential "JCS" key__dir["JCS"]
        
        // ========================================
        // 8. 构建前端 - 使用 code 参数
        // ========================================
        "8. 构建前端..." |> cyan |> output
        buildFrontend output credential code |> ignore
        
        // ========================================
        // 9. 构建后端 - 使用 code 参数
        // ========================================
        "9. 构建后端..." |> cyan |> output
        buildBackend output credential code |> ignore
        
        // ========================================
        // 10. 启动服务 - 使用 code 参数
        // ========================================
        "10. 启动服务..." |> cyan |> output
        let serviceRunning = checkDotNetServiceRunning output credential code
        if serviceRunning then
            $"✓ {code} 服务已在运行" |> green |> output
            $"如需重启，请手动执行: sudo systemctl restart {code.ToLower()}" |> yellow |> output
        else
            startServiceVerbose output credential code |> ignore
        
        // ========================================
        // 11. 显示部署摘要
        // ========================================
        let logFileInfo = 
            match logPath with
            | Some p -> $"📋 部署日志: {p}"
            | None -> $"📋 服务日志: /tmp/{code.ToLower()}.log"
        let summary = $"""
========================================
✅ {server} 代码部署完成
========================================
📁 部署目录结构:
   - {code}: ~/{key__dir["code"]}
   - Common: ~/{key__dir["Common"]}
   - JCS: ~/{key__dir["JCS"]}
🔗 PostgreSQL: Host={server};Port=5432;Username=postgres;Password=***
{logFileInfo}
========================================
"""
        summary |> cyan |> output
        
    with ex ->
        $"部署过程中发生错误: {ex.Message}" |> red |> output
        "请检查远程服务器状态" |> yellow |> output

// ==================== 主流程 ====================

let routine 
    (runtime: RuntimeTemplate<_,_,_,_>)
    (deployLogPath: string option) =

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
    checkSSHAuth output credential (host.disk + "Dev/" + runtime.projectCode, host.deploy.gitEmail)
    
    // 3. 推送本地所有仓库变更到 GitHub
    "3. 推送本地所有仓库变更到 GitHub..." |> cyan |> output
    pushAllLocalRepos output code host.deploy.gitName host.deploy.gitEmail host.disk
        
    // 4. 验证 PostgreSQL（确保服务运行）
    "4. 验证 PostgreSQL..." |> cyan |> output
    exeRemoteValidatePSQL output host.deploy.credential

    // 5. 配置 PostgreSQL 远程访问（仅当未配置时）
    "5. 配置 PostgreSQL 远程访问..." |> cyan |> output
    let conn = exeRemoteConfigurePSQL output host.deploy.credential host.deploy.postgresPwd

    // 6. 部署代码（从 GitHub 更新）
    "6. 部署代码..." |> cyan |> output
    exeDeployCode output host.deploy.credential code deployLogPath
        
    // 7. 清理 SSH 隧道
    "7. 清理 SSH 隧道..." |> cyan |> output
    stopAllSshTunnels output
    
    $"\n✅ {conn}" |> green |> output
    "\n✅ 部署流程完成 " |> green |> output