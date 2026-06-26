module UtilKestrel.BashDeployer

open System
open System.IO
open System.Threading
open System.Collections.Generic

open Util.Linux.Bash
open Util.Linux.Linux
open Util.Linux.PSQL
open Util.Linux.Git
open Util.Monitor

open UtilKestrel.Types


// ==================== Git 推送函数 ====================

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

/// 通过 scp 直接推送源码到目标服务器（GitHub 不可用时的 fallback）
/// 将本地 Dev/{code}, Dev/Common, Dev/JCS 目录直接 scp 到远程服务器
let private pushSourceViaScp output code credential disk =
    $"\n⚠ GitHub push 失败，改用 scp 直接推送源码到服务器..." |> orange |> output
    
    let porto, user, server = credential
    let portArg = match porto with Some p -> $"-P {p}" | None -> ""
    let privateKeyArg = getSshPrivateKeyArg()
    
    // 源目录 → 目标目录映射
    let repos = [|
        (code, $"{disk}Dev/{code}", $"~/Dev/{code}")
        ("Common", $"{disk}Dev/Common", $"~/Dev/Common")
        ("JCS", $"{disk}Dev/JCS", $"~/Dev/JCS")
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
            
            // Windows 路径需要转换：C:\Dev\Xxx -> /cygdrive/c/Dev/Xxx 或使用原生路径
            // PowerShell 的 scp 支持 Windows 路径格式
            let scpArgs = 
                $"{privateKeyArg} {portArg} -r -o StrictHostKeyChecking=no " +
                $"\"{localPath}\\*\" {user}@{server}:{remotePath}/"
            
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


// ==================== 环境检查函数 ====================

/// 检查并安装 Node.js（要求 ≥ 22.12，Vite 8 最低要求）
let ensureNodeInstalled output credential =
    "\n--- 检查 Node.js ---" |> cyan |> output
    
    // 检查 node 是否已安装且版本 ≥ 22.12
    // 使用 node -e 提取 major.minor.patch 并比较
    let checkVersionCmd = 
        "node -e \"var v=process.versions.node.split('.');var ok=(+v[0]>22)||(+v[0]==22&&+v[1]>=12);console.log(ok?'OK':'OLD')\" 2>/dev/null || echo 'NOT_INSTALLED'"
    let nodeStatus = bash output credential checkVersionCmd
    
    if nodeStatus.Contains("OK") then
        // 显示版本
        let versionCmd = "node --version 2>/dev/null || echo 'unknown'"
        let version = bash output credential versionCmd
        $"✓ Node.js 已安装且版本满足要求: {version.Trim()}" |> green |> output
        true
    elif nodeStatus.Contains("OLD") then
        // Node.js 已安装但版本太旧
        let versionCmd = "node --version 2>/dev/null || echo 'unknown'"
        let version = bash output credential versionCmd
        $"⚠ Node.js 已安装但版本过旧: {version.Trim()}（Vite 8 需要 ≥ 22.12），正在升级..." |> yellow |> output
        
        // 使用 nodesource 安装 Node.js 22.x
        let installCmds = [|
            "curl -fsSL https://rpm.nodesource.com/setup_22.x | bash -"
            "yum install -y nodejs || dnf install -y nodejs || (apt update && apt install -y nodejs)"
        |]
        
        installCmds |> Array.iter (fun cmd ->
            let result = bash output credential cmd
            result |> output)
        
        // 验证升级
        let verifyCmd = "node -e \"var v=process.versions.node.split('.');var ok=(+v[0]>22)||(+v[0]==22&&+v[1]>=12);console.log(ok?'OK':'STILL_OLD')\" 2>/dev/null || echo 'FAILED'"
        let verifyResult = bash output credential verifyCmd
        
        if verifyResult.Contains("OK") then
            let newVersionCmd = "node --version 2>/dev/null || echo 'unknown'"
            let newVersion = bash output credential newVersionCmd
            $"✓ Node.js 升级成功: {newVersion.Trim()}" |> green |> output
            true
        else
            "❌ Node.js 升级失败" |> red |> output
            false
    else
        "⚠ Node.js 未安装，正在安装..." |> yellow |> output
        
        // 使用 nodesource 安装 Node.js 22.x
        let installCmds = [|
            "curl -fsSL https://rpm.nodesource.com/setup_22.x | bash -"
            "yum install -y nodejs || dnf install -y nodejs || (apt update && apt install -y nodejs)"
        |]
        
        installCmds |> Array.iter (fun cmd ->
            let result = bash output credential cmd
            result |> output)
        
        // 验证安装
        let verifyCmd = "node -e \"var v=process.versions.node.split('.');var ok=(+v[0]>22)||(+v[0]==22&&+v[1]>=12);console.log(ok?'OK':'STILL_OLD')\" 2>/dev/null || echo 'FAILED'"
        let verifyResult = bash output credential verifyCmd
        
        if verifyResult.Contains("OK") then
            let versionCmd = "node --version 2>/dev/null || echo 'unknown'"
            let version = bash output credential versionCmd
            $"✓ Node.js 安装成功: {version.Trim()}" |> green |> output
            true
        else
            "❌ Node.js 安装失败（或版本不满足 ≥ 22.12）" |> red |> output
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


// ==================== 版本检查函数 ====================

/// 远程获取仓库的 git hash
let remoteGitHash output credential repoPath =
    try
        let cmd = $"cd ~/{repoPath} && git rev-parse --short=8 HEAD 2>/dev/null || echo '-'"
        let result = bash output credential cmd
        result.Trim()
    with _ -> "-"

/// 远程检查 dist 目录是否有产物（返回文件数）
let remoteDistFileCount output credential vscodeDir =
    try
        let cmd = $"if [ -d ~/{vscodeDir}/dist ]; then ls ~/{vscodeDir}/dist 2>/dev/null | wc -l; else echo '0'; fi"
        let result = bash output credential cmd
        result.Trim()
    with _ -> "0"

/// 远程通过 curl 查询 monitorVersion API 获取运行时版本
let remoteQueryVersion output credential port code =
    try
        let cmd = $"curl -s -X POST http://localhost:{port}/api/admin/monitorVersion -H 'Content-Type: application/json' -d '{{\"act\":\"monitorversion\"}}' 2>/dev/null || echo '{{\"Er\":\"N/A\"}}'"
        let result = bash output credential cmd
        result.Trim()
    with _ -> "N/A"

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
            "  使用 Bun 安装依赖 + 构建前端..." |> cyan |> output
            
            // ====== 调试信息 ======
            "[DEBUG] --- 构建调试开始 ---" |> yellow |> output
            
            // 调试1: Node.js 版本和路径
            let debugNode = bash output credential "echo '[DEBUG] which node:' && which node 2>&1 || echo 'NOT_FOUND'; echo '[DEBUG] node -v:' && node -v 2>&1 || echo 'NODE_NOT_FOUND'; echo '[DEBUG] node -e check:' && node -e \"console.log('node works')\" 2>&1 || echo 'NODE_EVAL_FAILED'"
            debugNode |> output
            
            // 调试2: 检查 vite.js 是否存在
            let debugVite = bash output credential $"echo '[DEBUG] vite.js exists:' && ls -la ~/{vscodeDir}/node_modules/vite/bin/vite.js 2>&1 || echo 'VITE_NOT_FOUND'"
            debugVite |> output
            
            // 调试3: 构建前 dist 目录内容
            let debugDistBefore = bash output credential $"echo '[DEBUG] dist before build:' && ls -laR ~/{vscodeDir}/dist/ 2>&1 || echo 'DIST_NOT_FOUND'"
            debugDistBefore |> output
            
            // ====== 正式构建 ======
            // 步骤1: bun install（180s 超时，适应慢速网络和大依赖）
            "[DEBUG] --- 步骤1: bun install ---" |> yellow |> output
            let installResult = bashWithTimeout output credential $"cd ~/{vscodeDir} && /root/.bun/bin/bun install 2>&1; echo '[DEBUG] bun install exit code:' $?" 180000
            installResult |> output
            
            // 步骤2: bun generateRoutes.cjs（生成路由）
            "[DEBUG] --- 步骤2: bun generateRoutes.cjs ---" |> yellow |> output
            let genResult = bash output credential $"cd ~/{vscodeDir} && /root/.bun/bin/bun generateRoutes.cjs 2>&1; echo '[DEBUG] generateRoutes exit code:' $?"
            genResult |> output
            
            // 步骤3: 用系统 node 运行 vite build（不用 bun bd，因 bun 内嵌 Node 版本可能不够 Vite 8 要求）
            // 参考：bun 1.2.15 内嵌 Node 22.6.0，Vite 8 要求 22.12+，导致构建静默失败
            "[DEBUG] --- 步骤3: node vite build ---" |> yellow |> output
            let buildResult = bashWithTimeout output credential $"cd ~/{vscodeDir} && node ./node_modules/vite/bin/vite.js build --emptyOutDir 2>&1; echo '[DEBUG] vite build exit code:' $?" 180000
            buildResult |> output
            
            // 调试4: 构建后 dist 目录内容
            "[DEBUG] --- 构建后检查 ---" |> yellow |> output
            let debugDistAfter = bash output credential $"echo '[DEBUG] dist after build:' && ls -laR ~/{vscodeDir}/dist/ 2>&1 || echo 'DIST_NOT_FOUND'"
            debugDistAfter |> output
            
            // 调试5: 检查是否有 vite.config.ts 和 tsconfig
            let debugConfig = bash output credential $"echo '[DEBUG] config files:' && ls -la ~/{vscodeDir}/vite.config.* ~/{vscodeDir}/tsconfig*.json 2>&1 || echo 'CONFIG_NOT_FOUND'"
            debugConfig |> output
            
            // 调试6: 检查 node_modules/.bin/vite
            let debugBinVite = bash output credential $"echo '[DEBUG] node_modules/.bin/vite:' && ls -la ~/{vscodeDir}/node_modules/.bin/vite 2>&1 || echo 'BIN_VITE_NOT_FOUND'; echo '[DEBUG] vite package.json version:' && cat ~/{vscodeDir}/node_modules/vite/package.json 2>/dev/null | grep '\"version\"' || echo 'NO_PKG_JSON'"
            debugBinVite |> output
            
            "[DEBUG] --- 构建调试结束 ---" |> yellow |> output
            
            // 验证 dist 产物是否生成
            let distCount = remoteDistFileCount output credential vscodeDir
            if distCount <> "0" then
                $"✓ 前端构建完成 (dist 产物: {distCount} 项)" |> green |> output
                true
            else
                "❌ 前端构建后 dist 目录为空或不存在！" |> red |> output
                false
        else
            "  使用 npm 安装..." |> cyan |> output
            let npmResult = bash output credential $"cd ~/{vscodeDir} && npm install"
            npmResult |> output
            
            let buildResult = bash output credential $"cd ~/{vscodeDir} && npm run build 2>&1"
            buildResult |> output
            
            let distCount = remoteDistFileCount output credential vscodeDir
            if distCount <> "0" then
                $"✓ 前端构建完成 (dist 产物: {distCount} 项)" |> green |> output
                true
            else
                "❌ 前端构建后 dist 目录为空或不存在！" |> red |> output
                false

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
                let restoreResult = bash output credential $"cd ~/{serverDir} && dotnet restore --verbosity quiet /p:NoWarn=NU1603"
                if restoreResult.Trim().Length > 0 then restoreResult |> output
                
                // 2. 添加正确的 Common 子项目引用 (suppress stderr noise)
                "  - 添加项目引用..." |> cyan |> output
                let utilRefCmd = $"cd ~/{serverDir} && dotnet add reference ~/Dev/Common/Util/Util.fsproj 2>/dev/null; dotnet add reference ~/Dev/Common/UtilKestrel/UtilKestrel.fsproj 2>/dev/null; dotnet add reference ~/Dev/JCS/JCS.Shared/JCS.Shared.fsproj 2>/dev/null; dotnet add reference ~/Dev/JCS/JCS.BizLogics/JCS.BizLogics.fsproj 2>/dev/null; echo '✓ 项目引用检查完成'"
                let refResult = bash output credential utilRefCmd
                refResult |> output
                
                // 3. dotnet build (suppress NU1603 warnings, 300s timeout for large solution)
                "  - dotnet build --configuration Release" |> cyan |> output
                let buildResult = bashWithTimeout output credential $"cd ~/{serverDir} && dotnet build --configuration Release --verbosity minimal /nowarn:NU1603" 300000
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
            
            // 1. dotnet restore (suppress NU1603, 120s timeout)
            "  - dotnet restore" |> cyan |> output
            let restoreResult = bashWithTimeout output credential $"cd ~/{serverDir} && dotnet restore --verbosity quiet /p:NoWarn=NU1603" 120000
            if restoreResult.Trim().Length > 0 then restoreResult |> output
            
            // 2. 添加项目引用 (suppress stderr noise)
            "  - 添加项目引用..." |> cyan |> output
            let refCmd = $"cd ~/{serverDir} && dotnet add reference ~/Dev/Common/Util/Util.fsproj 2>/dev/null; dotnet add reference ~/Dev/Common/UtilKestrel/UtilKestrel.fsproj 2>/dev/null; dotnet add reference ~/Dev/JCS/JCS.Shared/JCS.Shared.fsproj 2>/dev/null; dotnet add reference ~/Dev/JCS/JCS.BizLogics/JCS.BizLogics.fsproj 2>/dev/null; echo '✓ 项目引用检查完成'"
            let refResult = bash output credential refCmd
            refResult |> output
            
            // 3. dotnet build (suppress NU1603, 300s timeout)
            "  - dotnet build --configuration Release" |> cyan |> output
            let buildResult = bashWithTimeout output credential $"cd ~/{serverDir} && dotnet build --configuration Release --verbosity minimal /nowarn:NU1603" 300000
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
/// scpAlreadyPushed: 如果为 true，git pull 失败时不会报错（因为代码已通过 scp 同步）
let exeDeployCode
    output
    credential
    code
    (logPath: string option)
    (scpAlreadyPushed: bool) =

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
        key__dir["FsRoot"] <- "FsRoot"
        key__dir["FsRootCode"] <- "FsRoot/" + code

        // 定义需要检查的目录列表
        let dirs = [|
            ("Dev 根目录", key__dir["Dev"])
            ("主项目目录", key__dir["code"])
            ("Common 目录", key__dir["Common"])
            ("JCS 目录", key__dir["JCS"])
            ("FsRoot 根目录", key__dir["FsRoot"])
            ("FsRoot 项目目录", key__dir["FsRootCode"])
        |]
        
        let allDirsExist = ensureDirectories output credential dirs
        
        if allDirsExist |> not then
            "⚠ 部分目录创建失败，尝试继续..." |> yellow |> output
        
        // ========================================
        // 2. 更新主项目仓库
        // ========================================
        if scpAlreadyPushed then
            $"2. 代码已通过 scp 同步，跳过 git pull（主项目）" |> cyan |> output
        else
            "2. 从 GitHub 更新主项目仓库..." |> cyan |> output
            updateSingleRepo output credential 
                code (getRepoUrl code) key__dir["code"] |> ignore
        
        // ========================================
        // 3. 更新 Common 仓库
        // ========================================
        if scpAlreadyPushed then
            $"3. 代码已通过 scp 同步，跳过 git pull（Common）" |> cyan |> output
        else
            "3. 从 GitHub 更新 Common 仓库..." |> cyan |> output
            updateSingleRepo output credential 
                "Common" (getRepoUrl "Common") key__dir["Common"] |> ignore
        
        // ========================================
        // 4. 更新 JCS 仓库
        // ========================================
        if scpAlreadyPushed then
            $"4. 代码已通过 scp 同步，跳过 git pull（JCS）" |> cyan |> output
        else
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
        let updateResult = bashWithTimeout output credential updateFrontendCmd 180000
        updateResult |> output
        
        // 后端依赖
        let serverDir = key__dir["code"] + "/Server"
        let updateBackendCmd = $"""
cd ~/{serverDir}
if ls *.fsproj 1>/dev/null 2>&1; then
    dotnet restore --verbosity quiet /p:NoWarn=NU1603 2>/dev/null || echo 'dotnet restore 跳过'
    echo '后端依赖更新完成'
else
    echo '未找到 .fsproj，跳过'
fi
"""
        let updateBackendResult = bashWithTimeout output credential updateBackendCmd 120000
        updateBackendResult |> output
        
        // ========================================
        // 7. 显示所有仓库状态 + 记录构建前 git hash
        // ========================================
        "7. 仓库状态 + 版本快照..." |> cyan |> output
        let vscodeDir = key__dir["code"] + "/vscode"
        let preGitHash = remoteGitHash output credential (key__dir["code"])
        let preCommonHash = remoteGitHash output credential key__dir["Common"]
        let preJcsHash = remoteGitHash output credential key__dir["JCS"]
        $"  部署前 Git Hash → {code}: {preGitHash} | Common: {preCommonHash} | JCS: {preJcsHash}" |> cyan |> output
        
        showRepoStatus output credential code key__dir["code"]
        showRepoStatus output credential "Common" key__dir["Common"]
        showRepoStatus output credential "JCS" key__dir["JCS"]
        
        // ========================================
        // 8. 构建前端 - 使用 code 参数
        // ========================================
        "8. 构建前端..." |> cyan |> output
        let frontendOk = buildFrontend output credential code
        
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
            $"✓ {code} systemd 服务已在运行，自动重启加载新代码..." |> green |> output
            let restartCmd = $"systemctl restart {code.ToLower()}"
            let restartResult = bash output credential restartCmd
            $"  重启结果: {restartResult.Trim()}" |> output
            // 等待 3 秒让服务完全启动
            System.Threading.Thread.Sleep(3000)
            // 验证重启后服务状态
            let postRestartCheck = bash output credential $"systemctl is-active {code.ToLower()}"
            if postRestartCheck.Trim() = "active" then
                $"✓ {code} 服务已成功重启" |> green |> output
            else
                $"⚠ 服务重启后状态: {postRestartCheck.Trim()}" |> yellow |> output
        else
            startServiceVerbose output credential code |> ignore
        
        // ========================================
        // 11. 部署后版本验证 + 汇总报告
        // ========================================
        "\n========================================" |> cyan |> output
        "📊 部署结果汇总报告" |> cyan |> output
        "========================================" |> cyan |> output
        
        // 11a. 部署后 git hash
        let postGitHash = remoteGitHash output credential (key__dir["code"])
        let postCommonHash = remoteGitHash output credential key__dir["Common"]
        let postJcsHash = remoteGitHash output credential key__dir["JCS"]
        
        let hashChanged pre post = if pre <> "-" && post <> "-" && pre <> post then "✅ 已更新" else if pre = post then "⚠ 未变化" else "—"
        $"  Git Hash:" |> output
        $"    {code}:  {preGitHash} → {postGitHash}  {hashChanged preGitHash postGitHash}" |> output
        $"    Common: {preCommonHash} → {postCommonHash}  {hashChanged preCommonHash postCommonHash}" |> output
        $"    JCS:    {preJcsHash} → {postJcsHash}  {hashChanged preJcsHash postJcsHash}" |> output
        
        // 11b. 前端构建结果
        let distCount = remoteDistFileCount output credential vscodeDir
        let frontendStatus = if frontendOk then $"✅ 成功 (dist 产物: {distCount} 项)" else $"❌ 失败 (dist: {distCount} 项)"
        $"  前端构建: {frontendStatus}" |> output
        
        // 11c. 运行时版本（通过 API 查询）
        let port = "9020"  // Aiarwa HTTP 端口（runServer 9020 9021）
        $"  运行时版本 (API):" |> output
        let versionJson = remoteQueryVersion output credential port code
        $"    {versionJson}" |> output
        
        // 11d. 服务状态
        let serviceStatus = bash output credential $"systemctl is-active {code.ToLower()}"
        let serviceIcon = if serviceStatus.Trim() = "active" then "✅" else "❌"
        $"  服务状态: {serviceIcon} {serviceStatus.Trim()}" |> output
        
        let logFileInfo = 
            match logPath with
            | Some p -> $"📋 部署日志: {p}"
            | None -> $"📋 服务日志: /tmp/{code.ToLower()}.log"
        let deployDir = key__dir.["code"]
        $"\n📁 部署目录: ~/{deployDir}  |  {logFileInfo}" |> output
        "========================================" |> cyan |> output
        
    with ex ->
        $"部署过程中发生错误: {ex.Message}" |> red |> output
        "请检查远程服务器状态" |> yellow |> output

// ==================== Cloudflare Tunnel 检查 ====================

/// 检测 cloudflared 服务是否存在并检查配置
/// 如果存在但 originService 端口不对，自动修复
let private checkCloudflareTunnel output credential (httpsPort: int) =
    "\n--- Cloudflare Tunnel 检查 ---" |> cyan |> output
    
    // 1. 检查 cloudflared 是否安装
    let checkInstalledCmd = "command -v cloudflared > /dev/null 2>&1 && echo 'INSTALLED' || echo 'NOT_INSTALLED'"
    let installedStatus = bash output credential checkInstalledCmd
    
    if installedStatus.Contains("NOT_INSTALLED") then
        "  cloudflared 未安装，跳过" |> cyan |> output
        true  // 不是错误，只是没有 Tunnel
    else
        "✓ cloudflared 已安装" |> green |> output
        
        // 2. 检查 systemd 服务是否存在
        let checkServiceCmd = "systemctl list-unit-files cloudflared.service 2>/dev/null | grep cloudflared || echo 'NO_SERVICE'"
        let serviceStatus = bash output credential checkServiceCmd
        
        if serviceStatus.Contains("NO_SERVICE") then
            "  cloudflared systemd 服务不存在，跳过" |> cyan |> output
            true
        else
            // 3. 检查 config.yml 是否存在
            let checkConfigCmd = "if [ -f /root/.cloudflared/config.yml ]; then echo 'EXISTS'; elif [ -f /etc/cloudflared/config.yml ]; then echo 'EXISTS_ETC'; else echo 'NOT_EXISTS'; fi"
            let configStatus = bash output credential checkConfigCmd
            
            if configStatus.Contains("NOT_EXISTS") then
                "⚠ cloudflared 服务存在但未找到 config.yml" |> yellow |> output
                true  // 不阻断部署
            else
                let configPath = if configStatus.Contains("EXISTS_ETC") then "/etc/cloudflared/config.yml" else "/root/.cloudflared/config.yml"
                $"  配置文件: {configPath}" |> cyan |> output
                
                // 4. 读取当前 originService 端口
                let readOriginCmd = $"grep -oP 'service:\\s*https?://localhost:\\K\\d+' {configPath} 2>/dev/null | head -1 || echo 'UNKNOWN'"
                let currentOriginPort = bash output credential readOriginCmd
                let currentOriginPort = currentOriginPort.Trim()
                
                $"  当前 Tunnel origin 端口: {currentOriginPort}" |> cyan |> output
                $"  实际 HTTPS 端口: {httpsPort}" |> cyan |> output
                
                // 5. 比较端口
                if currentOriginPort = httpsPort.ToString() then
                    $"✓ Cloudflare Tunnel 配置正确 (origin → localhost:{httpsPort})" |> green |> output
                    
                    // 确保服务在运行
                    let isActiveCmd = "systemctl is-active cloudflared 2>/dev/null || echo 'inactive'"
                    let isActive = bash output credential isActiveCmd
                    if isActive.Trim() = "active" then
                        "✓ cloudflared 服务正在运行" |> green |> output
                    else
                        "⚠ cloudflared 服务未运行，正在启动..." |> yellow |> output
                        bash output credential "systemctl restart cloudflared" |> ignore
                        System.Threading.Thread.Sleep(2000)
                        let postCheck = bash output credential "systemctl is-active cloudflared"
                        if postCheck.Trim() = "active" then
                            "✓ cloudflared 服务已启动" |> green |> output
                        else
                            "⚠ cloudflared 服务启动可能失败" |> yellow |> output
                    true
                else
                    $"⚠ Cloudflare Tunnel 端口不匹配! (Tunnel: {currentOriginPort} ≠ 实际: {httpsPort})" |> yellow |> output
                    "  自动修复中..." |> yellow |> output
                    
                    // 6. 用 sed 替换 config.yml 中的端口
                    // 匹配 service: http://localhost:PORT 或 service: https://localhost:PORT
                    let fixCmd = $"sed -i 's|\\(service:\\s*\\)https\\?://localhost:[0-9]\\+|\\1https://localhost:{httpsPort}|g' {configPath}"
                    let fixResult = bash output credential fixCmd
                    
                    // 验证修复
                    let verifyCmd = $"grep -oP 'service:\\s*https?://localhost:\\K\\d+' {configPath} 2>/dev/null | head -1 || echo 'UNKNOWN'"
                    let newOriginPort = bash output credential verifyCmd
                    
                    if newOriginPort.Trim() = httpsPort.ToString() then
                        $"✓ 已修复: origin → https://localhost:{httpsPort}" |> green |> output
                        
                        // 重启 cloudflared
                        "  重启 cloudflared..." |> cyan |> output
                        bash output credential "systemctl restart cloudflared" |> ignore
                        System.Threading.Thread.Sleep(3000)
                        
                        // 检查重启后状态
                        let postRestartCheck = bash output credential "systemctl is-active cloudflared"
                        if postRestartCheck.Trim() = "active" then
                            "✓ cloudflared 已重启" |> green |> output
                            
                            // 快速验证 Tunnel 日志无错误
                            let logCheckCmd = "journalctl -u cloudflared --since '10 sec ago' --no-pager 2>&1 | grep -i 'error\|connection refused' || echo 'NO_ERRORS'"
                            let logCheck = bash output credential logCheckCmd
                            if logCheck.Contains("NO_ERRORS") then
                                "✓ Tunnel 日志无错误" |> green |> output
                            else
                                "⚠ Tunnel 日志有错误，请检查:" |> yellow |> output
                                logCheck |> output
                            true
                        else
                            "⚠ cloudflared 重启后状态异常" |> yellow |> output
                            true  // 不阻断部署
                    else
                        $"❌ 自动修复失败，请手动编辑 {configPath}" |> red |> output
                        $"  将 service 端口改为 https://localhost:{httpsPort}" |> yellow |> output
                        true  // 不阻断部署

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
    
    // 部署前版本快照（本地）
    let localGitHash = gitHashLocal()
    $"  本地 {code} Git Hash: {localGitHash}" |> cyan |> output

    // 记录服务是否在部署前运行（用于最终恢复）
    let mutable serviceWasRunning = false
    
    try
        // 1. 本地：切换目录
        "1. 切换到项目目录: " + devDir |> cyan |> output
        let exeLocal args = exec output devDir "powershell" args |> ignore
        "cd " + devDir |> exeLocal
            
        // 2. 检查 SSH 免密登录是否已配置
        "2. 检查 SSH 免密登录状态..." |> cyan |> output
        checkSSHAuth output credential (host.disk + "Dev/" + runtime.projectCode, host.deploy.gitEmail)
        
        // 3. 推送本地所有仓库变更到 GitHub
        "3. 推送本地所有仓库变更到 GitHub..." |> cyan |> output
        let gitPushOk = pushAllLocalRepos output code host.deploy.gitName host.deploy.gitEmail host.disk
        
        // 3b. 如果 GitHub push 失败，用 scp 直接把源码推送到目标服务器
        let scpPushOk =
            if gitPushOk then
                true  // GitHub 已成功，不需要 scp
            else
                $"\n⚠ GitHub push 失败，启动 scp 直推方案..." |> orange |> output
                pushSourceViaScp output code credential host.disk
            
        // 3.5. 在操作 PostgreSQL 之前，先停止远程服务（防止 PG 重启导致服务 crash）
        "3.5. 暂停远程服务（保护数据库连接）..." |> cyan |> output
        serviceWasRunning <- checkDotNetServiceRunning output credential code
        if serviceWasRunning then
            $"  停止 {code} 服务..." |> yellow |> output
            let stopCmd = $"systemctl stop {code.ToLower()}"
            let stopResult = bash output credential stopCmd
            $"  停止结果: {stopResult.Trim()}" |> output
            // 等待服务完全停止
            System.Threading.Thread.Sleep(2000)
            let postStopCheck = bash output credential $"systemctl is-active {code.ToLower()}"
            if postStopCheck.Trim() = "inactive" then
                $"✓ {code} 服务已停止" |> green |> output
            else
                $"⚠ 服务状态: {postStopCheck.Trim()}，继续..." |> yellow |> output
        
        // 4. 验证 PostgreSQL（确保服务运行）
        "4. 验证 PostgreSQL..." |> cyan |> output
        exeRemoteValidatePSQL output host.deploy.credential

        // 5. 配置 PostgreSQL 远程访问（仅当未配置时）
        "5. 配置 PostgreSQL 远程访问..." |> cyan |> output
        let conn = exeRemoteConfigurePSQL output host.deploy.credential host.deploy.postgresPwd

        // 5b. 检查并创建项目数据库用户（新装数据库必须检查）
        "5b. 检查项目数据库用户..." |> cyan |> output
        // 从 runtime.host.conn 解析出用户名、密码和数据库名
        let dbParts = 
            let connStr = host.conn
            let pairs = 
                connStr.Split(';') 
                |> Array.map (fun s -> 
                    let parts = s.Trim().Split('=')
                    if parts.Length >= 2 then (parts[0].Trim(), parts[1].Trim()) 
                    else ("", ""))
                |> Map.ofArray
            (pairs |> Map.tryFind "Username" |> Option.defaultValue "postgres",
             pairs |> Map.tryFind "Password" |> Option.defaultValue "",
             pairs |> Map.tryFind "Database" |> Option.defaultValue "postgres")
        let dbUser, dbPwd, dbName = dbParts
        $"  项目数据库用户: {dbUser}, 数据库: {dbName}" |> cyan |> output
        Util.Linux.PSQL.exeEnsureDatabaseUser output host.deploy.credential dbUser dbPwd dbName |> ignore

        // 6. 部署代码（从 GitHub 更新；如果已通过 scp 推送则跳过 git pull）
        "6. 部署代码..." |> cyan |> output
        exeDeployCode output host.deploy.credential code deployLogPath scpPushOk
        
        // 6.5. 检查 Cloudflare Tunnel 配置（部署完成后，服务已启动）
        // HTTPS 端口 = HTTP 端口 + 363（Aiarwa: 8081+363=8444, 通用约定）
        let httpsPort = host.port + 363
        checkCloudflareTunnel output host.deploy.credential httpsPort |> ignore
            
        // 7. 清理 SSH 隧道
        "7. 清理 SSH 隧道..." |> cyan |> output
        stopAllSshTunnels output
        
        $"\n✅ {conn}" |> green |> output
        "\n✅ 部署流程完成 " |> green |> output

    with ex ->
        $"\n❌ 部署过程中发生错误: {ex.Message}" |> red |> output
        $"{ex.StackTrace}" |> output
        "请检查远程服务器状态" |> yellow |> output
        
        // 无论如何，尝试恢复服务
        if serviceWasRunning then
            $"\n⚠ 尝试恢复 {code} 服务..." |> yellow |> output
            try
                let startCmd = $"systemctl start {code.ToLower()}"
                let startResult = bash output credential startCmd
                $"  启动结果: {startResult.Trim()}" |> output
                System.Threading.Thread.Sleep(3000)
                let postStartCheck = bash output credential $"systemctl is-active {code.ToLower()}"
                if postStartCheck.Trim() = "active" then
                    $"✓ {code} 服务已恢复运行" |> green |> output
                else
                    $"⚠ 服务恢复失败，状态: {postStartCheck.Trim()}" |> red |> output
            with ex2 ->
                $"❌ 服务恢复也失败了: {ex2.Message}" |> red |> output
        
        // 7. 清理 SSH 隧道（即使出错也要清理）
        try
            stopAllSshTunnels output
        with _ -> ()