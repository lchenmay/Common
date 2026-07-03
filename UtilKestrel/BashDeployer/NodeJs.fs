module UtilKestrel.BashDeployer.NodeJs

open Util.Linux.Bash
open Util.Monitor
open UtilKestrel.Types
open UtilKestrel.BashDeployer.Common

// ==================== 环境检查函数 ====================

/// 检查并安装 Node.js（要求 ≥ 22.12，Vite 8 最低要求）
let ensureNodeInstalled output credential =
    "\n--- 检查 Node.js ---" |> cyan |> output

    // 检查 node 是否已安装
    let versionCmd = "node --version 2>/dev/null || echo 'NOT_INSTALLED'"
    let versionStr = bash output credential versionCmd

    // 解析版本号并检查是否 ≥ 22.12
    let checkVersion (verStr: string) =
        if verStr.Contains("NOT_INSTALLED") then
            false, "未安装"
        else
            let ver = verStr.Trim().TrimStart('v')
            let parts = ver.Split('.') |> Array.map (fun s -> try int s with _ -> 0)
            let major = if parts.Length > 0 then parts.[0] else 0
            let minor = if parts.Length > 1 then parts.[1] else 0
            let ok = major > 22 || (major = 22 && minor >= 12)
            ok, verStr.Trim()

    let nodeOk, version = checkVersion versionStr

    if nodeOk then
        $"✓ Node.js 已安装且版本满足要求: {version}" |> green |> output
        true
    elif not (versionStr.Contains("NOT_INSTALLED")) then
        // 已安装但版本太旧
        $"⚠ Node.js 已安装但版本过旧: {versionStr.Trim()}（Vite 8 需要 ≥ 22.12），正在升级..." |> yellow |> output
        
        // 使用 nodesource 安装 Node.js 22.x（自动检测系统类型）
        let installCmd = "curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && apt-get install -y nodejs"
        let result = bashWithTimeout output credential installCmd 180000  // 3分钟超时
        result |> output
        
        // 验证升级
        let newVersionStr = bash output credential "node --version 2>/dev/null || echo 'FAILED'"
        let upgradeOk, newVersion = checkVersion newVersionStr
        
        if upgradeOk then
            $"✓ Node.js 升级成功: {newVersion}" |> green |> output
            true
        else
            // 备用方案：手动下载二进制文件
            $"⚠ nodesource 安装失败，尝试手动下载 Node.js 22.x..." |> yellow |> output
            let fallbackCmd = "cd /tmp && curl -fsSL https://nodejs.org/dist/v22.12.0/node-v22.12.0-linux-x64.tar.xz -o node.tar.xz && tar -xJf node.tar.xz && mv node-v22.12.0-linux-x64 /usr/local/lib/nodejs 2>/dev/null || true && ln -sf /usr/local/lib/nodejs/bin/node /usr/local/bin/node && ln -sf /usr/local/lib/nodejs/bin/npm /usr/local/bin/npm && ln -sf /usr/local/lib/nodejs/bin/npx /usr/local/bin/npx"
            let fallbackResult = bashWithTimeout output credential fallbackCmd 180000
            fallbackResult |> output
            
            let verifyVersionStr = bash output credential "node --version 2>/dev/null || echo 'FAILED'"
            let fallbackOk, fallbackVersion = checkVersion verifyVersionStr
            
            if fallbackOk then
                $"✓ Node.js 手动安装成功: {fallbackVersion}" |> green |> output
                true
            else
                "❌ Node.js 升级失败（nodesource + 手动安装均失败）" |> red |> output
                false
    else
        "⚠ Node.js 未安装，正在安装..." |> yellow |> output
        
        // 使用 nodesource 安装 Node.js 22.x（自动检测系统类型）
        let installCmd = "curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && apt-get install -y nodejs"
        let result = bashWithTimeout output credential installCmd 180000  // 3分钟超时
        result |> output
        
        // 验证安装
        let newVersionStr = bash output credential "node --version 2>/dev/null || echo 'FAILED'"
        let installOk, newVersion = checkVersion newVersionStr
        
        if installOk then
            $"✓ Node.js 安装成功: {newVersion}" |> green |> output
            true
        else
            // 备用方案：手动下载二进制文件
            $"⚠ nodesource 安装失败，尝试手动下载 Node.js 22.x..." |> yellow |> output
            let fallbackCmd = "cd /tmp && curl -fsSL https://nodejs.org/dist/v22.12.0/node-v22.12.0-linux-x64.tar.xz -o node.tar.xz && tar -xJf node.tar.xz && mv node-v22.12.0-linux-x64 /usr/local/lib/nodejs 2>/dev/null || true && ln -sf /usr/local/lib/nodejs/bin/node /usr/local/bin/node && ln -sf /usr/local/lib/nodejs/bin/npm /usr/local/bin/npm && ln -sf /usr/local/lib/nodejs/bin/npx /usr/local/bin/npx"
            let fallbackResult = bashWithTimeout output credential fallbackCmd 180000
            fallbackResult |> output
            
            let verifyVersionStr = bash output credential "node --version 2>/dev/null || echo 'FAILED'"
            let fallbackOk, fallbackVersion = checkVersion verifyVersionStr
            
            if fallbackOk then
                $"✓ Node.js 手动安装成功: {fallbackVersion}" |> green |> output
                true
            else
                "❌ Node.js 安装失败（nodesource + 手动安装均失败）" |> red |> output
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


// ==================== 构建函数 ====================

/// 构建前端（逐条执行）- 使用 code 参数
let buildFrontend output credential code =
    "\n--- 构建前端 ---" |> cyan |> output
    
    let vscodeDir = $"Dev/{code}/vscode"
    
    // 检查 package.json 是否存在
    "检查 package.json..." |> cyan |> output
    let checkPackageCmd = $"if [ -f $HOME/{vscodeDir}/package.json ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
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
            // 注意：避免 node -e 中使用单引号，否则会与外层 bash -c 的单引号冲突
            let debugNode = bash output credential "echo '[DEBUG] which node:' && which node 2>&1 || echo 'NOT_FOUND'; echo '[DEBUG] node -v:' && node -v 2>&1 || echo 'NODE_NOT_FOUND'; echo '[DEBUG] node -e check:' && node -e \"console.log(42)\" 2>&1 || echo 'NODE_EVAL_FAILED'"
            debugNode |> output
            
            // 调试2: 检查 vite.js 是否存在
            let debugVite = bash output credential $"echo '[DEBUG] vite.js exists:' && ls -la $HOME/{vscodeDir}/node_modules/vite/bin/vite.js 2>&1 || echo 'VITE_NOT_FOUND'"
            debugVite |> output
            
            // 调试3: 构建前 dist 目录内容
            let debugDistBefore = bash output credential $"echo '[DEBUG] dist before build:' && ls -laR $HOME/{vscodeDir}/dist/ 2>&1 || echo 'DIST_NOT_FOUND'"
            debugDistBefore |> output
            
            // ====== 正式构建 ======
            // 步骤0: 强制更新 @lchenmay/jcs-common（确保生产环境使用最新版）
            "[DEBUG] --- 步骤0: bun update @lchenmay/jcs-common ---" |> yellow |> output
            let updateJcsCommonResult = bashWithTimeout output credential $"cd $HOME/{vscodeDir} && /root/.bun/bin/bun update @lchenmay/jcs-common 2>&1; echo '[DEBUG] bun update jcs-common exit code:' $?" 120000
            updateJcsCommonResult |> output

            // 步骤1: bun install（180s 超时，适应慢速网络和大依赖）
            "[DEBUG] --- 步骤1: bun install ---" |> yellow |> output
            let installResult = bashWithTimeout output credential $"cd $HOME/{vscodeDir} && /root/.bun/bin/bun install 2>&1; echo '[DEBUG] bun install exit code:' $?" 180000
            installResult |> output
            
            // 步骤2: bun generateRoutes.cjs（生成路由）
            "[DEBUG] --- 步骤2: bun generateRoutes.cjs ---" |> yellow |> output
            let genResult = bash output credential $"cd $HOME/{vscodeDir} && /root/.bun/bin/bun generateRoutes.cjs 2>&1; echo '[DEBUG] generateRoutes exit code:' $?"
            genResult |> output
            
            // 步骤3: 用系统 node 运行 vite build（不用 bun bd，因 bun 内嵌 Node 版本可能不够 Vite 8 要求）
            // 参考：bun 1.2.15 内嵌 Node 22.6.0，Vite 8 要求 22.12+，导致构建静默失败
            "[DEBUG] --- 步骤3: node vite build ---" |> yellow |> output
            let buildResult = bashWithTimeout output credential $"cd $HOME/{vscodeDir} && node ./node_modules/vite/bin/vite.js build --emptyOutDir 2>&1; echo '[DEBUG] vite build exit code:' $?" 180000
            buildResult |> output
            
            // 调试4: 构建后 dist 目录内容
            "[DEBUG] --- 构建后检查 ---" |> yellow |> output
            let debugDistAfter = bash output credential $"echo '[DEBUG] dist after build:' && ls -laR $HOME/{vscodeDir}/dist/ 2>&1 || echo 'DIST_NOT_FOUND'"
            debugDistAfter |> output
            
            // 调试5: 检查是否有 vite.config.ts 和 tsconfig
            let debugConfig = bash output credential $"echo '[DEBUG] config files:' && ls -la $HOME/{vscodeDir}/vite.config.* $HOME/{vscodeDir}/tsconfig*.json 2>&1 || echo 'CONFIG_NOT_FOUND'"
            debugConfig |> output
            
            // 调试6: 检查 node_modules/.bin/vite
            let debugBinVite = bash output credential $"echo '[DEBUG] node_modules/.bin/vite:' && ls -la $HOME/{vscodeDir}/node_modules/.bin/vite 2>&1 || echo 'BIN_VITE_NOT_FOUND'; echo '[DEBUG] vite package.json version:' && cat $HOME/{vscodeDir}/node_modules/vite/package.json 2>/dev/null | grep '\"version\"' || echo 'NO_PKG_JSON'"
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
            let npmResult = bash output credential $"cd $HOME/{vscodeDir} && npm install"
            npmResult |> output
            
            let buildResult = bash output credential $"cd $HOME/{vscodeDir} && npm run build 2>&1"
            buildResult |> output
            
            let distCount = remoteDistFileCount output credential vscodeDir
            if distCount <> "0" then
                $"✓ 前端构建完成 (dist 产物: {distCount} 项)" |> green |> output
                true
            else
                "❌ 前端构建后 dist 目录为空或不存在！" |> red |> output
                false
