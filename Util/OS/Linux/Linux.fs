module Util.Linux.Linux

open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Text
open System.IO

open Util.Linux.Bash


let credentialExpand credential = 
    let porto, user, server = credential
    let target = $"{user}@{server}"
    let portArg = 
        match porto with
        | Some port -> $"-p {port}"
        | None -> ""
    porto,user,server,target,portArg

let checkSSHAuth 
    output
    credential
    (rsaPath,email) =

    let credential = credential
    let porto,user,server,target,portArg = credentialExpand credential

    let isConfigured = checkSshKeyConfiguredWithTimeout output credential 30000

    if isConfigured then
        "✓ SSH 免密登录已配置，跳过密钥生成和复制步骤" |> green |> output
    else
        "⚠ SSH 免密登录未配置，开始配置..." |> yellow |> output
    
        "2a. 检查服务器公钥认证..." |> cyan |> output
        "提示：请确保服务器已启用 PubkeyAuthentication" |> yellow |> output
        let enableCmd = 
            $"ssh {portArg} {target} \"sed -i 's/^#*PubkeyAuthentication.*/PubkeyAuthentication yes/' /etc/ssh/sshd_config && systemctl restart sshd\""
        $"如未启用，请手动执行: {enableCmd}" |> orange |> output
        "" |> output
    
        let rsaFile = rsaPath + "/id_rsa"
        let pubFile = rsaFile + ".pub"
        
        if File.Exists pubFile then
            $"公钥已存在: {pubFile}" |> green |> output
        else
            $"生成 SSH 密钥 {pubFile}..." |> cyan |> output
            email__SshKey email |> orange |> output
            "请手动执行上述命令生成密钥（一路回车使用默认值）" |> yellow |> output
            "完成后按任意键继续..." |> yellow |> output
            Console.ReadKey() |> ignore
            "\n" |> output
    
        "复制公钥到服务器（需要输入密码）..." |> cyan |> output
        
        match remoteCopy_SshKeyCommands output credential rsaFile with
        | Some (pubFile, copyCmd, permCmd, testCmd) ->

            "\n========== 请按顺序手动执行以下命令 ==========" |> yellow |> output
            "" |> output

            "步骤 1: 复制公钥（输入服务器密码）" |> yellow |> output
            copyCmd |> orange |> output

            "" |> output
            $"步骤 2: 设置权限（ssh {portArg} {target} 再次输入服务器密码）" |> yellow |> output
            permCmd |> orange |> output

            "" |> output
            "步骤 3: 验证免密登录（无需密码）" |> yellow |> output
            $"ssh {portArg} {target} \"{testCmd}\"" |> orange |> output

            "" |> output
            "================================================" |> yellow |> output

            "提示: 请在另一个终端执行以上命令（按顺序）" |> yellow |> output
            "完成后按任意键继续，或输入 'q' 取消..." |> yellow |> output

            let key = Console.ReadKey()
            if key.KeyChar = 'q' || key.KeyChar = 'Q' then
                "\n用户取消部署" |> yellow |> output
                failwith "部署已取消"

            "\n" |> output
        
            let mutable retryCount = 0
            let maxRetries = 5
            let mutable configuredAfterRetry = false
            
            while not configuredAfterRetry && retryCount < maxRetries do
                retryCount <- retryCount + 1
                $"检查 SSH 配置 (第 {retryCount} 次)..." |> green |> output
                
                configuredAfterRetry <- 
                    checkSshKeyConfiguredWithTimeout output credential 30000
                
                if not configuredAfterRetry then
                    if retryCount < maxRetries then
                        "SSH 配置尚未成功，请确保已执行上述命令后按任意键重试..." |> yellow |> output
                        "（输入 'q' 取消）" |> yellow |> output
                        let k = Console.ReadKey()
                        if k.KeyChar = 'q' || k.KeyChar = 'Q' then
                            "\n用户取消部署" |> yellow |> output
                            failwith "部署已取消"
                        "\n" |> output
                    else
                        $"SSH 配置失败，已重试 {maxRetries} 次" |> red |> output
                        "是否继续部署？(y/n): " |> yellow |> output
                        let response = Console.ReadLine()
                        if response <> "y" && response <> "Y" then
                            "SSH 配置失败，用户取消部署" |> red |> output
                            failwith "SSH 配置失败，用户取消部署"
                        else
                            "⚠ 继续部署（跳过 SSH 验证）" |> yellow |> output
        
        | None ->
            $"密钥文件 {pubFile} 不存在" |> red |> output
            "是否继续部署？(y/n): " |> yellow |> output
            let response = Console.ReadLine()
            if response <> "y" && response <> "Y" then
                "密钥文件不存在，用户取消部署" |> red |> output
                failwith "密钥文件不存在，用户取消部署"
    
    "3. 测试 SSH 连接..." |> cyan |> output
    let isConnected = checkSshKeyConfiguredWithTimeout output credential 10000
    if isConnected then
        "✓ SSH 连接正常" |> green |> output
    else
        "⚠ SSH 连接异常，继续执行..." |> yellow |> output


/// 检查并创建目录（如果不存在）- 修复逻辑
let ensureDirectory output credential targetDir =
    $"\n--- 检查目录 ~/{targetDir} ---" |> cyan |> output
    
    // 检查目录是否存在 - 使用简单的 if 语句避免多行问题
    let checkCmd = $"if [ -d ~/{targetDir} ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
    let checkResult = bash output credential checkCmd
    
    if checkResult.Contains("NOT_EXISTS") then
        // 目录不存在，创建
        $"创建目录 ~/{targetDir}..." |> cyan |> output
        let mkdirCmd = $"mkdir -p ~/{targetDir}"
        let mkdirResult = bash output credential mkdirCmd
        mkdirResult |> output
        
        // 验证创建是否成功
        let verifyCmd = $"if [ -d ~/{targetDir} ]; then echo 'CREATED'; else echo 'FAILED'; fi"
        let verifyResult = bash output credential verifyCmd
        
        if verifyResult.Contains("CREATED") then
            $"✓ 目录 ~/{targetDir} 创建成功" |> green |> output
            // 设置权限
            let chmodCmd = $"chmod -R 755 ~/{targetDir}"
            bash output credential chmodCmd |> ignore
            "✓ 权限已设置为 755" |> green |> output
            true
        else
            $"❌ 目录 ~/{targetDir} 创建失败" |> red |> output
            false
    else
        $"✓ 目录 ~/{targetDir} 已存在" |> green |> output
        
        // 检查权限
        let permCmd = $"if [ -w ~/{targetDir} ] && [ -r ~/{targetDir} ] && [ -x ~/{targetDir} ]; then echo 'PERM_OK'; else echo 'PERM_BAD'; fi"
        let permResult = bash output credential permCmd
        
        if permResult.Contains("PERM_OK") then
            "✓ 权限正确" |> green |> output
            true
        else
            "⚠ 权限不正确，修复中..." |> yellow |> output
            let fixPermCmd = $"chmod -R 755 ~/{targetDir}"
            bash output credential fixPermCmd |> ignore
            "✓ 权限已修复为 755" |> green |> output
            true


/// 确保多个目录存在
let ensureDirectories output credential dirs =
    $"\n=== 确保所有目录存在 ===" |> cyan |> output
    
    let results = 
        dirs 
        |> Array.map (fun (name, path) ->
            let success = ensureDirectory output credential path
            (name, path, success))
    
    "\n--- 目录检查结果 ---" |> cyan |> output
    let allSuccess = 
        results 
        |> Array.forall (fun (_, _, success) -> success)
    
    results 
    |> Array.iter (fun (name, path, success) ->
        let status = if success then "✓" else "❌"
        $"{status} {name}: ~/{path}" |> output)
    
    if allSuccess then
        "✓ 所有目录检查通过" |> green |> output
    else
        "⚠ 部分目录创建失败" |> yellow |> output
    
    allSuccess

/// 删除远程目录（用于清理）
let deleteRemoteDir output credential targetDir =
    $"\n--- 删除目录 ~/{targetDir} ---" |> yellow |> output
    
    let checkCmd = $"if [ -d ~/{targetDir} ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
    let checkResult = bash output credential checkCmd
    
    if checkResult.Contains("NOT_EXISTS") then
        $"⚠ 目录 ~/{targetDir} 不存在，无需删除" |> yellow |> output
        true
    else
        let deleteCmd = $"rm -rf ~/{targetDir}"
        let deleteResult = bash output credential deleteCmd
        deleteResult |> output
        
        let verifyCmd = $"if [ -d ~/{targetDir} ]; then echo 'STILL_EXISTS'; else echo 'DELETED'; fi"
        let verifyResult = bash output credential verifyCmd
        
        if verifyResult.Contains("DELETED") then
            $"✓ 目录 ~/{targetDir} 已成功删除" |> green |> output
            true
        else
            $"❌ 目录 ~/{targetDir} 删除失败" |> red |> output
            false

/// 获取服务的 systemd 单元名称
let private getServiceName (code: string) = code.ToLower()

/// 检查 systemd 服务是否处于 active (running) 状态
let checkDotNetServiceRunning 
    output 
    credential
    code =

    let svcName = getServiceName code
    // 用 systemctl is-active 精确判断
    let checkCmd = $"systemctl is-active {svcName} 2>/dev/null || echo 'inactive'"
    let result = bash output credential checkCmd
    
    $"  [DEBUG] systemctl is-active {svcName}: {result.Trim()}" |> cyan |> output
    
    if result.Trim() = "active" then
        $"✓ {code} systemd 服务正在运行" |> green |> output
        true
    else
        $"⚠ {code} systemd 服务未运行" |> yellow |> output
        // 额外调试
        $"  [DEBUG] systemctl status {svcName}:" |> cyan |> output
        bash output credential $"systemctl status {svcName} 2>/dev/null || echo '(无状态信息)'" |> output
        $"  [DEBUG] 检查 build 产物:" |> cyan |> output
        let checkBuildCmd = $"ls -la ~/Dev/{code}/Server/bin/Release/net10.0/ 2>/dev/null | head -20 || echo '(build 输出目录不存在)'"
        bash output credential checkBuildCmd |> output
        false

/// 创建或更新 systemd 服务单元文件，并启用/启动服务
/// 参数: output, credential, code, 端口列表（用于依赖 postgresql 和 network-online）
let private createOrUpdateSystemdService output credential (code: string) =
    let svcName = getServiceName code
    let serverDir = $"/root/Dev/{code}/Server"
    let publishDir = $"/root/publish/{code}"
    let workingDir = publishDir
    let dllPath = $"{publishDir}/Server.dll"
    let logFile = $"/var/log/{svcName}.log"

    // 从服务名推断端口（WYI=9000, Aiarwa=9020, 其他默认 5000）
    let httpPort =
        match code with
        | "WYI" -> "9000"
        | "Aiarwa" -> "9020"
        | _ -> "5000"

    // systemd unit 文件内容
    let unitContent =
        "[Unit]\n" +
        $"Description={code} Kestrel HTTP Service\n" +
        "After=network-online.target postgresql.service\n" +
        "Wants=network-online.target postgresql.service\n" +
        "\n" +
        "[Service]\n" +
        "Type=simple\n" +
        $"WorkingDirectory={workingDir}\n" +
        $"ExecStart=/usr/bin/dotnet {dllPath} --urls=http://localhost:{httpPort}\n" +
        "Restart=always\n" +
        "RestartSec=10\n" +
        $"StandardOutput=append:{logFile}\n" +
        $"StandardError=append:{logFile}\n" +
        "Environment=DOTNET_PRINT_TELEMETRY_MESSAGE=false\n" +
        "Environment=ASPNETCORE_ENVIRONMENT=Production\n" +
        "TimeoutStopSec=30\n" +
        "KillSignal=SIGTERM\n" +
        "MemoryMax=512M\n" +
        "CPUQuota=200%\n" +
        "\n" +
        "[Install]\n" +
        "WantedBy=multi-user.target"
    // 写入 unit 文件
    let writeCmd = $"cat > /etc/systemd/system/{svcName}.service << 'UNIT_EOF'\n{unitContent}\nUNIT_EOF"
    bash output credential writeCmd |> output
    
    $"  systemd unit 文件已写入: /etc/systemd/system/{svcName}.service" |> green |> output
    
    // daemon-reload
    bash output credential "systemctl daemon-reload" |> ignore
    "  systemctl daemon-reload 完成" |> green |> output

/// 通过 systemd 启动服务（简单版）
let startDotNetService output credential (code: string) =
    $"\n--- 启动 {code} 服务 (systemd) ---" |> cyan |> output
    
    let svcName = getServiceName code
    let serverDir = $"~/Dev/{code}/Server"
    
    // 1. 检查 build 产物
    "  1. 检查 build 产物..." |> cyan |> output
    let buildOutCmd = $"ls -la {serverDir}/bin/Release/net10.0/Server.dll 2>/dev/null && echo 'EXISTS' || echo 'NOT_EXISTS'"
    let buildCheck = bash output credential buildOutCmd
    if buildCheck.Contains("NOT_EXISTS") then
        "❌ Server.dll 不存在，请先 build" |> red |> output
        false
    else
        $"✓ Server.dll 存在: {serverDir}/bin/Release/net10.0/Server.dll" |> green |> output
        
        // 2. 创建/更新 systemd unit
        "  2. 创建 systemd 服务单元..." |> cyan |> output
        createOrUpdateSystemdService output credential code
        
        // 3. 启用开机自启
        "  3. 启用开机自启..." |> cyan |> output
        bash output credential $"systemctl enable {svcName}" |> output
        
        // 4. 重启服务（或启动）
        "  4. 启动/重启服务..." |> cyan |> output
        // 先检查是否已在运行
        let isActive = bash output credential $"systemctl is-active {svcName} 2>/dev/null || echo 'inactive'"
        if isActive.Trim() = "active" then
            bash output credential $"systemctl restart {svcName}" |> output
            $"  已重启 {svcName} 服务" |> green |> output
        else
            bash output credential $"systemctl start {svcName}" |> output
            $"  已启动 {svcName} 服务" |> green |> output
        
        // 5. 等待启动
        "  5. 等待服务启动..." |> cyan |> output
        bash output credential "sleep 5" |> ignore
        
        // 6. 检查状态
        "  6. 检查服务状态..." |> cyan |> output
        let status = bash output credential $"systemctl is-active {svcName} 2>/dev/null || echo 'inactive'"
        let journal = bash output credential $"journalctl -u {svcName} --no-pager -n 20 2>/dev/null || echo '(无日志)'"
        $"  systemctl is-active: {status.Trim()}" |> output
        $"  最近日志:\n{journal}" |> output
        
        let running = status.Trim() = "active"
        if running then
            $"✓ {code} systemd 服务运行中" |> green |> output
        else
            $"❌ {code} systemd 服务未运行" |> red |> output
        running

/// 启动服务（逐条执行，详细调试）- systemd 版本
let startServiceVerbose output credential (code: string) =
    "\n--- 启动服务 (systemd) ---" |> cyan |> output
    
    let svcName = getServiceName code
    let serverDir = $"~/Dev/{code}/Server"
    
    // 0. 启动前诊断
    "  0. 启动前诊断..." |> cyan |> output
    
    "    - Server 目录内容:" |> cyan |> output
    bash output credential $"ls -la {serverDir}/" |> output
    
    "    - Publish 输出目录:" |> cyan |> output
    let publishDir = $"/root/publish/{code}"
    bash output credential $"ls -la {publishDir}/ 2>/dev/null || echo '(publish 输出目录不存在)'" |> output
    
    "    - 当前 systemd 服务状态:" |> cyan |> output
    bash output credential $"systemctl status {svcName} 2>/dev/null || echo '(服务尚未注册)'" |> output
    
    // 1. 停止现有进程（兼容旧版 nohup 方式）
    "  1. 清理旧版 nohup 进程..." |> cyan |> output
    bash output credential "sudo killall -9 dotnet 2>/dev/null; echo 'cleanup done'" |> output
    
    // 2. 创建/更新 systemd unit
    "  2. 创建 systemd 服务单元..." |> cyan |> output
    createOrUpdateSystemdService output credential code
    
    // 3. 启用开机自启
    "  3. 启用开机自启..." |> cyan |> output
    bash output credential $"systemctl enable {svcName} 2>&1" |> output
    
    // 4. 重启服务
    "  4. 启动/重启服务..." |> cyan |> output
    let isActive = bash output credential $"systemctl is-active {svcName} 2>/dev/null || echo 'inactive'"
    if isActive.Trim() = "active" then
        bash output credential $"systemctl restart {svcName} 2>&1" |> output
    else
        bash output credential $"systemctl start {svcName} 2>&1" |> output
    
    // 5. 等待启动
    "  5. 等待服务启动（5秒）..." |> cyan |> output
    bash output credential "sleep 5" |> ignore
    
    // 6. 启动后诊断
    "  6. 启动后诊断..." |> cyan |> output
    
    "    - systemd 服务状态:" |> cyan |> output
    bash output credential $"systemctl status {svcName} --no-pager -l 2>&1 | head -30" |> output
    
    "    - Kestrel 端口占用:" |> cyan |> output
    let httpPort =
        match code with
        | "WYI" -> "9000"
        | "Aiarwa" -> "9020"
        | _ -> "5000"
    bash output credential $"sudo ss -tlnp | grep ':{httpPort} ' || echo '(端口 {httpPort} 未被占用)'" |> output
    
    "    - 服务日志（最近 30 行）:" |> cyan |> output
    bash output credential $"journalctl -u {svcName} --no-pager -n 30 2>/dev/null || echo '(无日志)'" |> output
    
    // 7. 验证服务状态
    "  7. 验证服务状态..." |> cyan |> output
    let running = checkDotNetServiceRunning output credential code
    
    if running then
        "✓ 服务启动成功" |> green |> output
        $"管理命令: systemctl {{start|stop|restart|status}} {svcName}" |> yellow |> output
        $"日志命令: journalctl -u {svcName} -f" |> yellow |> output
    else
        "❌ 服务启动失败" |> red |> output
        "--- 完整调试信息已在上方输出 ---" |> yellow |> output
    
    running
