module UtilKestrel.BashDeployer.Deploy

open System
open System.IO
open System.Collections.Generic
open Util.Linux.Bash
open Util.Linux.Linux
open Util.Linux.Git
open Util.Linux.PSQL
open Util.Monitor
open UtilKestrel.Types
open UtilKestrel.BashDeployer.Common
open UtilKestrel.BashDeployer.Git
open UtilKestrel.BashDeployer.NodeJs
open UtilKestrel.BashDeployer.DotNet
open UtilKestrel.BashDeployer.PgSql
open UtilKestrel.BashDeployer.Process
open UtilKestrel.BashDeployer.File

// ==================== 服务健康探活 ====================

/// HTTP 探活：重试 curl 直到服务真正就绪或超时（超时 15s，间隔 2s）
/// 返回 (isReady, httpStatusCode)
let private waitForHttpReady output credential port maxSeconds =
    let rec loop remaining =
        if remaining <= 0 then
            $"  ⚠ HTTP 探活超时（{maxSeconds}s），服务可能尚未完全就绪" |> yellow |> output
            (false, "timeout")
        else
            let healthCmd = 
                "curl -sf -o /dev/null -w '%{http_code}' --connect-timeout 3 --max-time 5 http://localhost:" + port + "/api/admin/monitorVersion 2>/dev/null || echo '000'"
            let code = (bash output credential healthCmd).Trim()
            if code = "200" then
                $"  ✓ HTTP 探活通过，服务已就绪 (localhost:{port}/api/admin/monitorVersion → {code})" |> green |> output
                (true, code)
            else
                $"  ⏳ 等待服务就绪... HTTP {code}（{remaining}s 后超时）" |> cyan |> output
                System.Threading.Thread.Sleep(2000)
                loop (remaining - 2)
    loop maxSeconds

/// 检查 journalctl 近期是否有崩溃/重启循环迹象
let private checkCrashLoop output credential (code: string) =
    let svcName = code.ToLower()
    let crashCmd = 
        $"journalctl -u {svcName} --since '3 minutes ago' --no-pager -q 2>/dev/null | grep -ci 'signal\\|crash\\|killed\\|oom\\|fatal\\|start request repeated' || echo '0'"
    let count = 
        try
            let result = (bash output credential crashCmd).Trim()
            System.Int32.Parse(result)
        with _ -> 0
    if count > 3 then
        $"⚠ journalctl 检测到 {svcName} 近期 {count} 条异常日志，可能崩溃循环" |> yellow |> output
        let tailCmd = $"journalctl -u {svcName} --since '3 minutes ago' --no-pager -q -n 20 2>/dev/null"
        $"  最近 20 行日志:" |> cyan |> output
        bash output credential tailCmd |> output

/// 获取服务 HTTP 端口
let private servicePort code =
    match code with
    | "WYI" -> "9000"
    | "Aiarwa" -> "9020"
    | _ -> "5000"

// ==================== 并行依赖安装 ====================

/// 并行安装前后端依赖（智能跳过：lock 文件未变则跳过）
let private parallelDepInstall output credential (key__dir: Dictionary<string,string>) =
    "\n--- 并行依赖安装 ---" |> cyan |> output
    
    let vscodeDir = key__dir.["code"] + "/vscode"
    let serverDir = key__dir.["code"] + "/Server"
    let deployStampDir = "~/.deploy-stamps"
    
    let frontendJob = async {
        // ── 第一步：快速检查 hash（5秒超时）──
        let checkCmd = $"""mkdir -p {deployStampDir}
FE_STAMP={deployStampDir}/bun-lock-hash
NEW_HASH=$(cd ~/{vscodeDir} && md5sum bun.lockb 2>/dev/null | cut -d' ' -f1 || echo 'NO_LOCK')
OLD_HASH=$(cat $FE_STAMP 2>/dev/null || echo 'NO_STAMP')
if [[ "$NEW_HASH" = "NO_LOCK" ]]; then
    echo 'NO_LOCK'
elif [[ "$NEW_HASH" = "$OLD_HASH" ]]; then
    echo 'SKIP'
else
    echo 'INSTALL'
fi
"""
        do! Async.SwitchToThreadPool()
        let action = (bashWithTimeout output credential checkCmd 5000).Trim()

        // ── 第二步：按需安装（长超时，bun install 可能耗时较长）──
        if action = "SKIP" then
            "[前端] bun.lockb 未变化，跳过安装" |> output
        else
            if action = "NO_LOCK" then
                "[前端] 未找到 bun.lockb，强制安装" |> output
            else
                "[前端] bun.lockb 已变化，重新安装..." |> output
            let installCmd = $"""if [[ -f /root/.bun/bin/bun ]]; then
    cd ~/{vscodeDir} && /root/.bun/bin/bun install 2>&1 || echo '[DEPLOY-WARN] bun install 失败'
else
    cd ~/{vscodeDir} && npm install 2>&1 || echo '[DEPLOY-WARN] npm install 失败'
fi
NEW_HASH=$(cd ~/{vscodeDir} && md5sum bun.lockb 2>/dev/null | cut -d' ' -f1 || echo 'NO_LOCK')
echo "$NEW_HASH" > {deployStampDir}/bun-lock-hash
echo '[前端] 依赖安装完成'
"""
            do! Async.SwitchToThreadPool()
            bashWithTimeout output credential installCmd 180000 |> ignore
    }
    
    let backendJob = async {
        // ── 第一步：快速检查 hash（5秒超时）──
        let checkCmd = $"""mkdir -p {deployStampDir}
BE_STAMP={deployStampDir}/dotnet-lock-hash
NEW_HASH=$(cd ~/{serverDir} && md5sum packages.lock.json 2>/dev/null | cut -d' ' -f1 || echo 'NO_LOCK')
OLD_HASH=$(cat $BE_STAMP 2>/dev/null || echo 'NO_STAMP')
if [[ "$NEW_HASH" = "NO_LOCK" ]]; then
    echo 'NO_LOCK'
elif [[ "$NEW_HASH" = "$OLD_HASH" ]]; then
    echo 'SKIP'
else
    echo 'INSTALL'
fi
"""
        do! Async.SwitchToThreadPool()
        let action = (bashWithTimeout output credential checkCmd 5000).Trim()

        // ── 第二步：按需 restore（长超时）──
        if action = "SKIP" then
            "[后端] packages.lock.json 未变化，跳过 restore" |> output
        else
            if action = "NO_LOCK" then
                "[后端] 未找到 packages.lock.json，强制 restore" |> output
            else
                "[后端] packages.lock.json 已变化，重新 restore..." |> output
            let installCmd = $"""cd ~/{serverDir} && dotnet restore --verbosity quiet /p:NoWarn=NU1603 2>&1 || echo '[DEPLOY-WARN] dotnet restore 失败'
NEW_HASH=$(cd ~/{serverDir} && md5sum packages.lock.json 2>/dev/null | cut -d' ' -f1 || echo 'NO_LOCK')
echo "$NEW_HASH" > {deployStampDir}/dotnet-lock-hash
echo '[后端] 依赖安装完成'
"""
            do! Async.SwitchToThreadPool()
            bashWithTimeout output credential installCmd 120000 |> ignore
    }
    
    [ frontendJob; backendJob ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

/// 并行构建前后端
/// 返回 (前端成功, 后端成功)
let private parallelBuild output credential code =
    "\n--- 并行构建 ---" |> cyan |> output
    
    let frontendOk = ref true
    let backendOk = ref true
    
    let frontendJob = async {
        let ok = buildFrontend output credential code
        frontendOk := ok
        return if ok then "[前端] 构建成功" else "[DEPLOY-WARN] 前端构建失败"
    }
    
    let backendJob = async {
        let ok = buildBackend output credential code
        backendOk := ok
        return if ok then "[后端] 构建成功" else "[DEPLOY-WARN] 后端构建失败"
    }
    
    let results = 
        [ frontendJob; backendJob ]
        |> Async.Parallel
        |> Async.RunSynchronously
    
    results |> Array.iter (fun r -> r |> output)
    
    (!frontendOk, !backendOk)


// ==================== 部署代码（重构版） ====================

/// 部署代码（从 GitHub 更新所有仓库）- 内部并行化
let private exeDeployCodeV2
    output
    credential
    code
    (logPath: string option)
    (isScpPush: bool)
    disk =

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
        // === Phase 1: 目录检查 ===
        "1. 检查并创建所有必要目录..." |> cyan |> output
        
        let key__dir = new Dictionary<string,string>()
        key__dir["Dev"] <- devRoot
        key__dir["code"] <- devRoot + "/" + code
        key__dir["Common"] <- devRoot + "/Common"
        key__dir["JCS"] <- devRoot + "/JCS"
        
        let dirs = [|
            ("Dev 根目录", key__dir["Dev"])
            ("主项目目录", key__dir["code"])
            ("Common 目录", key__dir["Common"])
            ("JCS 目录", key__dir["JCS"])
        |]
        
        let allDirsExist = ensureDirectories output credential dirs
        if allDirsExist |> not then
            "⚠ 部分目录创建失败，尝试继续..." |> yellow |> output
        
        // === Phase 2: 并行 git pull ===
        "2. 并行更新仓库..." |> cyan |> output
        parallelGitPull output credential key__dir code isScpPush disk
        
        // ── Phase 2.1: 代码目录存在性前置检查，避免克隆失败后级联失败 ──
        "2.1 验证代码目录..." |> cyan |> output
        let codeDir = key__dir.["code"]
        let codeDirCheck = 
            bashWithRetry output credential $"if [ -d ~/{codeDir} ] && [ -d ~/{codeDir}/.git ]; then echo 'OK'; elif [ -d ~/{codeDir} ]; then echo 'NO_GIT'; else echo 'NOT_EXISTS'; fi" 10000 3
        match codeDirCheck with
        | "OK" -> $"✓ 代码目录 ~/{codeDir} 就绪" |> green |> output
        | "NO_GIT" -> 
            $"❌ 代码目录 ~/{codeDir} 存在但不是 git 仓库（clone 失败），中止部署" |> red |> output
            $"  请检查 GitHub SSH 密钥配置: {disk}Dev/{sshKeyFile}.pub 是否已添加到 https://github.com/settings/keys" |> yellow |> output
        | _ ->
            $"❌ 代码目录 ~/{codeDir} 不存在（clone 失败），中止部署" |> red |> output
            $"  请检查 GitHub SSH 密钥配置: {disk}Dev/{sshKeyFile}.pub 是否已添加到 https://github.com/settings/keys" |> yellow |> output
        
        if codeDirCheck <> "OK" then
            failwith $"代码目录 ~/{codeDir} 不存在或无效，无法继续部署"
        
        writeDeployProgress output credential code "phase3_pulled" "" "代码已拉取到远程" "" "" ""
        
        // === Phase 3: 环境检查 ===
        "3. 确保环境就绪..." |> cyan |> output
        ensureEnvironment output credential |> ignore
        
        // === Phase 4: 并行依赖安装 ===
        "4. 并行安装依赖..." |> cyan |> output
        parallelDepInstall output credential key__dir
        writeDeployProgress output credential code "phase3_deps" "" "依赖安装完成，准备构建..." "" "" ""
        
        // === Phase 5: 版本快照 ===
        "5. 版本快照..." |> cyan |> output
        let preGitHash = remoteGitHash output credential (key__dir["code"])
        let preCommonHash = remoteGitHash output credential key__dir["Common"]
        let preJcsHash = remoteGitHash output credential key__dir["JCS"]
        $"  部署前 Git Hash → {code}: {preGitHash} | Common: {preCommonHash} | JCS: {preJcsHash}" |> cyan |> output
        writeDeployProgress output credential code "phase3_build" "" "正在编译前后端..." preGitHash "" ""
        
        // === Phase 6: 并行构建 ===
        "6. 并行构建前后端..." |> cyan |> output
        let frontOk, backOk = parallelBuild output credential code
        if not backOk then
            "❌ 后端构建失败，请检查构建日志" |> red |> output
        if not frontOk then
            "❌ 前端构建失败，dist 产物缺失" |> red |> output
        writeDeployProgress output credential code "phase3_built" "" "编译完成，准备重启服务..." preGitHash "" ""
        
        // === Phase 7: 启动服务 ===
        writeDeployProgress output credential code "phase4_restarting" "" "重启远程服务..." "" "" ""
        "7. 启动服务..." |> cyan |> output
        let port = servicePort code
        let serviceRunning = checkDotNetServiceRunning output credential code
        if serviceRunning then
            $"✓ {code} systemd 服务已在运行，自动重启加载新代码..." |> green |> output
            let restartCmd = $"systemctl restart {code.ToLower()}"
            let restartResult = bash output credential restartCmd
            $"  重启结果: {restartResult.Trim()}" |> output
            // HTTP 探活确认服务真正就绪（最多 15 秒）
            let healthy, _ = waitForHttpReady output credential port 15
            if healthy then
                $"✓ {code} 服务已成功重启并就绪" |> green |> output
            else
                let status = (bash output credential $"systemctl is-active {code.ToLower()}").Trim()
                $"⚠ 服务状态: {status}，HTTP 探活未通过" |> yellow |> output
                checkCrashLoop output credential code
        else
            startServiceVerbose output credential code |> ignore
            // 首次启动也探活
            waitForHttpReady output credential port 15 |> ignore
        
        // === Phase 8: 部署后验证 ===
        "\n========================================" |> cyan |> output
        "📊 部署结果汇总报告" |> cyan |> output
        "========================================" |> cyan |> output
        
        let postGitHash = remoteGitHash output credential (key__dir["code"])
        let postCommonHash = remoteGitHash output credential key__dir["Common"]
        let postJcsHash = remoteGitHash output credential key__dir["JCS"]
        
        let hashChanged pre post = if pre <> "-" && post <> "-" && pre <> post then "✅ 已更新" else if pre = post then "⚠ 未变化" else "—"
        $"  Git Hash:" |> output
        $"    {code}:  {preGitHash} → {postGitHash}  {hashChanged preGitHash postGitHash}" |> output
        $"    Common: {preCommonHash} → {postCommonHash}  {hashChanged preCommonHash postCommonHash}" |> output
        $"    JCS:    {preJcsHash} → {postJcsHash}  {hashChanged preJcsHash postJcsHash}" |> output
        
        let vscodeDir = key__dir["code"] + "/vscode"
        let distCount = remoteDistFileCount output credential vscodeDir
        $"  前端构建: dist 产物 {distCount} 项" |> output
        
        let port = match code with | "WYI" -> "9000" | "Aiarwa" -> "9020" | _ -> "5000"
        $"  运行时版本 (API):" |> output
        let versionJson = remoteQueryVersion output credential port code
        $"    {versionJson}" |> output
        
        let serviceStatus = bash output credential $"systemctl is-active {code.ToLower()}"
        let serviceIcon = if serviceStatus.Trim() = "active" then "✅" else "❌"
        $"  服务状态: {serviceIcon} {serviceStatus.Trim()}" |> output
        writeDeployProgress output credential code "phase4_verified" "" "部署验证完成" preGitHash postGitHash ""
        
        // 保存部署后的 git hash，供下次部署时判断是否需要 git pull
        "  保存版本快照..." |> cyan |> output
        let saveHashCmd = $"""mkdir -p ~/.deploy-stamps
echo "{postGitHash}" > ~/.deploy-stamps/git-hash-{code}
echo "{postCommonHash}" > ~/.deploy-stamps/git-hash-Common
echo "{postJcsHash}" > ~/.deploy-stamps/git-hash-JCS
echo "[版本快照] 已保存" """
        bash output credential saveHashCmd |> ignore
        
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
        // 尝试恢复服务：如果异常发生在 Phase 7（启动服务）之前，服务可能已经停了
        try
            $"\n⚠ 尝试恢复 {code} 服务（容错）..." |> yellow |> output
            let port = servicePort code
            let running = checkDotNetServiceRunning output credential code
            if not running then
                startServiceVerbose output credential code |> ignore
                let healthy, _ = waitForHttpReady output credential port 15
                if healthy then
                    $"✓ {code} 服务已恢复运行（HTTP 探活通过）" |> green |> output
                else
                    $"⚠ 服务恢复失败，HTTP 探活未通过" |> red |> output
                    checkCrashLoop output credential code
        with ex2 ->
            $"❌ 服务恢复也失败了: {ex2.Message}" |> red |> output


// ==================== 主流程（重构版） ====================

let routine 
    (runtime: RuntimeTemplate<_,_,_,_>)
    (deployLogPath: string option) =

    let host = runtime.host
    let credential = host.deploy.credential
    let porto,user,server,target,portArg = credentialExpand credential
    let devDir = host.disk + "Dev/" + runtime.projectCode
    let output = runtime.output
    let code = runtime.projectCode
    
    let mutable startedAt = ""
    let mutable localGitHash = ""
    let mutable serviceWasRunning = false
    
    // 部署事件收集器（用于最终分类总结）
    let okEvents = ResizeArray<string>()
    let warnEvents = ResizeArray<string>()
    let errEvents = ResizeArray<string>()
    
    try
        sshPrivateKeyPath <- devDir + "/id_rsa"
        startedAt <- DateTime.UtcNow.ToString("o")
        $">>> 开始部署至 {user}@{server}..." |> cyan |> output
        localGitHash <- gitHashLocal()
        $"  本地 {code} Git Hash: {localGitHash}" |> cyan |> output
        writeDeployProgress output credential code "starting" startedAt "开始部署前期准备..." localGitHash "" ""
        
        serviceWasRunning <- false
        
        // === Phase 0: SSH ControlMaster socket 健康检查 ===
        "Phase 0: SSH ControlMaster socket 健康检查..." |> cyan |> output
        let socketCheckCmd = "echo 'SOCKET_OK'"
        let socketCheckResult = bashWithTimeout output credential socketCheckCmd 10000
        if socketCheckResult.Contains("SOCKET_OK") then
            "✓ SSH socket 健康状态正常" |> green |> output
            okEvents.Add("SSH Socket 连接正常")
        else
            $"[SSH] socket 响应异常: {socketCheckResult.Trim()}" |> yellow |> output
            "  正在清理损坏的 socket 并重建连接..." |> yellow |> output
            let cleaned = cleanSshControlSockets output
            if cleaned > 0 then
                $"  已清理 {cleaned} 个损坏 socket，重试验证..." |> yellow |> output
                System.Threading.Thread.Sleep(500)
                let retryResult = bashWithTimeout output credential socketCheckCmd 10000
                if retryResult.Contains("SOCKET_OK") then
                    "✓ SSH socket 已修复，连接正常" |> green |> output
                    warnEvents.Add("SSH Socket 异常，已自动恢复")
                else
                    $"[SSH] socket 修复失败，后续远程命令可能出错: {retryResult.Trim()}" |> red |> output
                    errEvents.Add("SSH Socket 修复失败")
        
        // === Phase 1: 本地准备（SSH 检查 + 源码推送） ===
        
        "Phase 1/4: 本地准备..." |> cyan |> output
        writeDeployProgress output credential code "phase1_pushing" startedAt "推送源码到远程..." localGitHash "" ""
        
        // 1.1 检查本地 GitHub SSH 密钥
        "1.1 检查本地 GitHub SSH 密钥..." |> cyan |> output
        checkLocalGitSshKey output host.disk

        "1.2 切换到项目目录: " + devDir |> cyan |> output
        let exeLocal args = exec output devDir "powershell" args |> ignore
        "cd " + devDir |> exeLocal
            
        $"1.3 检查 SSH -> [{server}] 免密登录..." |> cyan |> output
        checkSSHAuth output credential (host.disk + "Dev/" + runtime.projectCode, host.deploy.gitEmail)
        
        // 1.4 部署 GitHub SSH 密钥到远程
        "1.4 部署 GitHub SSH 密钥到远程..." |> cyan |> output
        setupRemoteGitSshKey output credential host.disk

        // 1.5 判断目标服务器是否在内网，选择最优推送路径
        let isPrivate = isPrivateNetwork server
        if isPrivate then
            $"1.5 检测到内网目标 ({server})，优先使用 scp 直推源码..." |> cyan |> output
        else
            $"1.5 目标为外网 ({server})，使用 GitHub 中转..." |> cyan |> output
       
        let isScpPush, codePushedOk =
            if isPrivate then
                // 内网优先：直接 scp 源码，快 3-5 倍且避免无意义 git push
                let ok = pushSourceViaScp output code credential host.disk true
                (true, ok)
            else
                // 外网：先尝试 GitHub 中转
                "推送本地所有仓库变更到 GitHub..." |> cyan |> output
                let gitPushOk = pushAllLocalRepos output code host.deploy.gitName host.deploy.gitEmail host.disk
                if gitPushOk then
                    (false, true)  // GitHub push 成功，不是 scp
                else
                    // GitHub 不可用时 fallback 到 scp
                    $"\n⚠ GitHub push 失败，启动 scp 直推方案..." |> orange |> output
                    let ok = pushSourceViaScp output code credential host.disk false
                    (true, ok)

        if codePushedOk then
            okEvents.Add("源码推送成功")
        else
            errEvents.Add("源码推送失败")

        // === Phase 2: 停服务 + 健康检查 ===
        writeDeployProgress output credential code "phase2_stopping" startedAt "停止远程服务..." localGitHash "" ""
        "\nPhase 2/4: 停服务 + 健康检查..." |> cyan |> output
        
        "2.1 暂停远程服务..." |> cyan |> output
        // 重载 systemd 配置（防止 unit file changed 警告）
        bash output credential "systemctl daemon-reload" |> ignore
        serviceWasRunning <- checkDotNetServiceRunning output credential code
        if serviceWasRunning then
            $"  停止 {code} 服务..." |> yellow |> output
            let stopCmd = $"systemctl stop {code.ToLower()}"
            let stopResult = bash output credential stopCmd
            $"  停止结果: {stopResult.Trim()}" |> output
            System.Threading.Thread.Sleep(2000)
            let postStopCheck = bash output credential $"systemctl is-active {code.ToLower()}"
            if postStopCheck.Trim() = "inactive" then
                $"✓ {code} 服务已停止" |> green |> output
                okEvents.Add($"服务 {code} 已停止")
            else
                $"⚠ 服务状态: {postStopCheck.Trim()}，继续..." |> yellow |> output
                warnEvents.Add($"服务停止后状态异常: {postStopCheck.Trim()}")
        
        // DB 和 CF 只做轻量健康检查，失败不阻断
        "2.2 DB 健康检查..." |> cyan |> output
        let dbHealthy = checkDbHealth output credential
        if not dbHealthy then
            warnEvents.Add("DB 健康检查未通过（已继续）")
        
        "2.3 CF 健康检查..." |> cyan |> output
        let cfHealthy = checkCfHealth output credential
        if not cfHealthy then
            warnEvents.Add("CF 健康检查未通过（已继续）")

        // === Phase 2.5: 确保运行时文件存储目录（独立于构建目录） ===
        "2.4 确保运行时 fsRoot 目录..." |> cyan |> output
        if System.String.IsNullOrWhiteSpace(host.fsroot) then
            "⚠ host.fsroot 为空，跳过运行时文件目录检查" |> yellow |> output
            warnEvents.Add("FsRoot 为空，已跳过")
        else
            $"  目标路径: {host.fsroot}" |> cyan |> output
            let fsRootOk = ensureAbsolutePath output credential host.fsroot
            if fsRootOk then
                okEvents.Add("FsRoot 目录就绪")
            else
                "⚠ FsRoot 目录检查有失败，尝试继续..." |> yellow |> output
                warnEvents.Add("FsRoot 目录检查有失败")

        // === Phase 3: 部署代码 ===
        writeDeployProgress output credential code "phase3_building" startedAt "远程构建前后端..." localGitHash "" ""
        "\nPhase 3/4: 部署代码..." |> cyan |> output
        
        // 检查源码推送是否成功
        if not codePushedOk then
            "❌ 源码推送失败，中止部署" |> red |> output
            $"  请检查 SSH 连接、服务器磁盘空间、或手动执行：`scp -r C:\\Dev\\{code} root@5.78.201.21:~/Dev/`" |> yellow |> output
            writeDeployProgress output credential code "phase3_failed" startedAt "源码推送失败，中止部署" localGitHash "" ""
        else
            exeDeployCodeV2 output credential code deployLogPath isScpPush host.disk
            // 部署后检查前端 dist 产物
            let distCount = remoteDistFileCount output credential $"Dev/{code}/vscode"
            if distCount = "0" then
                errEvents.Add("前端构建失败：dist 目录为空")
            else
                okEvents.Add("代码构建部署完成")
            writeDeployProgress output credential code "phase4_verifying" startedAt "部署后验证..." localGitHash "" ""
        
        // === Phase 4: 部署后检查 + 清理 ===
        // 无论部署是否执行，都要检查服务状态
        "\nPhase 4/4: 部署后检查..." |> cyan |> output
        "4.1 清理 SSH 隧道..." |> cyan |> output
        stopAllSshTunnels output
        
        // 4.2 最终服务健康检查（最后防线）
        "4.2 最终服务健康检查..." |> cyan |> output
        try
            let port = servicePort code
            let finalStatus = bash output credential $"systemctl is-active {code.ToLower()}"
            $"  服务状态: {finalStatus.Trim()}" |> output
            if finalStatus.Trim() <> "active" then
                $"⚠ 服务未运行（状态: {finalStatus.Trim()}），启动服务..." |> yellow |> output
                startServiceVerbose output credential code |> ignore
                let healthy, _ = waitForHttpReady output credential port 15
                if healthy then
                    if serviceWasRunning then
                        $"✓ {code} 服务已恢复运行（HTTP 探活通过）" |> green |> output
                        warnEvents.Add($"服务曾停止，已恢复运行（HTTP 探活通过）")
                    else
                        $"✓ {code} 服务已启动（HTTP 探活通过）" |> green |> output
                        okEvents.Add($"服务 {code} 已成功启动（HTTP 探活通过）")
                else
                    $"⚠ 服务启动失败：HTTP 探活未通过" |> red |> output
                    errEvents.Add("HTTP 探活未通过，服务可能未正常启动")
                    checkCrashLoop output credential code
            else
                // systemd active 但也要 HTTP 探活确认
                let healthy, httpCode = waitForHttpReady output credential port 10
                if healthy then
                    $"✓ {code} 服务运行正常（HTTP 探活通过）" |> green |> output
                    okEvents.Add($"服务 {code} 运行正常（HTTP 探活通过）")
                else
                    $"⚠ {code} systemd 运行中但 HTTP 探活未通过（{httpCode}），可能仍在启动中" |> yellow |> output
                    warnEvents.Add($"systemd 运行中但 HTTP 探活未通过（{httpCode}）")
        with exCheck ->
            $"[WARN] 最终健康检查异常: {exCheck.Message}" |> yellow |> output
            warnEvents.Add($"最终健康检查异常: {exCheck.Message}")
        
        // === 部署总结 ===
        let printSummary () =
            "\n" |> output
            "══════════════════════════════════════════════════" |> cyan |> output
            $"              部署总结 — {code} @ {server}" |> output
            "══════════════════════════════════════════════════" |> cyan |> output
            "" |> output
            
            if okEvents.Count > 0 then
                "✅ 成功项:" |> green |> output
                for e in okEvents do
                    $"  ✓ {e}" |> output
                "" |> output
            
            if warnEvents.Count > 0 then
                "⚠ 注意项:" |> yellow |> output
                for e in warnEvents do
                    $"  ⚠ {e}" |> output
                "" |> output
            
            if errEvents.Count > 0 then
                "❌ 失败项:" |> red |> output
                for e in errEvents do
                    $"  ✗ {e}" |> output
                "" |> output
            
            if errEvents.Count > 0 then
                "══════════════════════════════════════════════════" |> red |> output
                "  ⚠⚠⚠ 部署完成但存在问题，请检查上述失败项 ⚠⚠⚠" |> red |> output
                "══════════════════════════════════════════════════" |> red |> output
            elif warnEvents.Count > 0 then
                "══════════════════════════════════════════════════" |> yellow |> output
                "  ⚠ 部署完成但有注意事项，请查看上述注意项" |> yellow |> output
                "══════════════════════════════════════════════════" |> yellow |> output
            else
                "══════════════════════════════════════════════════" |> green |> output
                "  ✅ 全部检查通过，部署成功！" |> green |> output
                "══════════════════════════════════════════════════" |> green |> output

        printSummary()
        writeDeployProgress output credential code "done" startedAt "部署流程全部完成" localGitHash "" ""

    with ex ->
        $"\n❌ 部署过程中发生错误: {ex.Message}" |> red |> output
        $"{ex.StackTrace}" |> output
        "请检查远程服务器状态" |> yellow |> output
        writeDeployProgress output credential code "failed" startedAt "部署失败" localGitHash "" ex.Message
        
        errEvents.Add($"部署异常: {ex.Message}")
        
        if serviceWasRunning then
            $"\n⚠ 尝试恢复 {code} 服务..." |> yellow |> output
            try
                let port = servicePort code
                let startCmd = $"systemctl start {code.ToLower()}"
                let startResult = bash output credential startCmd
                $"  启动结果: {startResult.Trim()}" |> output
                let healthy, _ = waitForHttpReady output credential port 15
                if healthy then
                    $"✓ {code} 服务已恢复运行（HTTP 探活通过）" |> green |> output
                    warnEvents.Add("异常后服务已恢复运行")
                else
                    $"⚠ 服务恢复失败，HTTP 探活未通过" |> red |> output
                    errEvents.Add("异常后服务恢复失败，HTTP 探活未通过")
                    checkCrashLoop output credential code
            with ex2 ->
                $"❌ 服务恢复也失败了: {ex2.Message}" |> red |> output
                errEvents.Add($"服务恢复异常: {ex2.Message}")
        
        try
            stopAllSshTunnels output
        with _ -> ()
        
        // 输出部署总结（异常分支）
        "\n" |> output
        "══════════════════════════════════════════════════" |> red |> output
        $"              部署总结 — {code} @ {server} (异常终止)" |> output
        "══════════════════════════════════════════════════" |> red |> output
        "" |> output
        
        if okEvents.Count > 0 then
            "✅ 成功项:" |> green |> output
            for e in okEvents do
                $"  ✓ {e}" |> output
            "" |> output
        
        if warnEvents.Count > 0 then
            "⚠ 注意项:" |> yellow |> output
            for e in warnEvents do
                $"  ⚠ {e}" |> output
            "" |> output
        
        if errEvents.Count > 0 then
            "❌ 失败项:" |> red |> output
            for e in errEvents do
                $"  ✗ {e}" |> output
            "" |> output
        
        "══════════════════════════════════════════════════" |> red |> output
        "  ❌❌❌ 部署异常终止，请检查上述失败项 ❌❌❌" |> red |> output
        "══════════════════════════════════════════════════" |> red |> output
