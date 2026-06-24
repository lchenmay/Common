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

/// 检查服务是否运行
let checkDotNetServiceRunning 
    output 
    credential
    code =

    // 先列出所有 dotnet 相关进程（调试用）
    let debugCmd = $"ps aux | grep -i '[d]otnet' || echo '(没有 dotnet 进程)'"
    let debugResult = bash output credential debugCmd
    $"  [DEBUG] ps aux | grep dotnet:\n{debugResult}" |> cyan |> output
    
    let checkCmd = $"ps aux | grep -q '[d]otnet.*Server' && echo 'RUNNING' || echo 'NOT_RUNNING'"
    let result = bash output credential checkCmd
    
    // "NOT_RUNNING".Contains("RUNNING") = true，必须精确匹配
    if result.Trim() = "RUNNING" then
        $"✓ {code} .NET 服务正在运行" |> green |> output
        true
    else
        $"⚠ {code} .NET 服务未运行" |> yellow |> output
        // 额外调试：检查 Server.dll 是否存在
        $"  [DEBUG] 检查 build 产物:" |> cyan |> output
        let checkBuildCmd = $"ls -la ~/Dev/{code}/Server/bin/Release/net10.0/ 2>/dev/null | head -20 || echo '(build 输出目录不存在)'"
        bash output credential checkBuildCmd |> output
        false

let startDotNetService output credential (code: string) =
    $"\n--- 启动 {code} 服务 ---" |> cyan |> output
    
    let serverDir = $"~/Dev/{code}/Server"
    let logFile = $"/tmp/{code.ToLower()}.log"
    
    // 调试：检查 Server 目录内容
    "  [DEBUG] Server 目录内容:" |> cyan |> output
    let lsCmd = $"ls -la {serverDir}/"
    bash output credential lsCmd |> output
    
    // 调试：检查 build 产物
    "  [DEBUG] Build 输出目录:" |> cyan |> output
    let buildOutCmd = $"ls -la {serverDir}/bin/Release/net10.0/ 2>/dev/null || echo '(build 输出目录不存在)'"
    bash output credential buildOutCmd |> output
    
    // 调试：检查 .fsproj 配置
    "  [DEBUG] 项目文件 OutputType:" |> cyan |> output
    let projCmd = $"grep -i 'OutputType\\|TargetFramework' {serverDir}/*.fsproj 2>/dev/null || echo '(无法读取)'"
    bash output credential projCmd |> output
    
    // 调试：启动前检查端口
    "  [DEBUG] 端口占用情况（启动前）:" |> cyan |> output
    let portCheckCmd = $"sudo ss -tlnp | grep -E ':80 |:443 ' || echo '端口 80/443 未被占用'"
    bash output credential portCheckCmd |> output
    
    // 调试：检查 dotnet 进程
    "  [DEBUG] dotnet 进程（启动前）:" |> cyan |> output
    let psCmd = $"ps aux | grep -i '[d]otnet' || echo '没有运行中的 dotnet 进程'"
    bash output credential psCmd |> output
    
    // 停止现有服务
    "  [DEBUG] 停止现有 dotnet 进程..." |> cyan |> output
    bash output credential "sudo killall -9 dotnet 2>/dev/null; echo 'killall done'" |> output
    
    // 清理端口
    bash output credential "sudo fuser -k 80/tcp 2>/dev/null; sudo fuser -k 443/tcp 2>/dev/null; echo 'port cleanup done'" |> output
    
    // 启动服务 - 用 nohup 并记录 PID
    "  [DEBUG] 启动 dotnet run..." |> cyan |> output
    let startCmd = $"cd {serverDir} && sudo nohup dotnet run --configuration Release > {logFile} 2>&1 & echo \"PID:$!\""
    let startResult = bash output credential startCmd
    "  [DEBUG] 启动结果:" |> cyan |> output
    startResult |> output
    
    // 等待
    "  [DEBUG] 等待 3 秒..." |> cyan |> output
    bash output credential "sleep 3" |> ignore
    
    // 调试：启动后检查进程
    "  [DEBUG] dotnet 进程（启动后）:" |> cyan |> output
    let psAfterCmd = $"ps aux | grep -i '[d]otnet' || echo '没有运行中的 dotnet 进程'"
    bash output credential psAfterCmd |> output
    
    // 调试：启动后检查端口
    "  [DEBUG] 端口占用情况（启动后）:" |> cyan |> output
    let portAfterCmd = $"sudo ss -tlnp | grep -E ':80 |:443 ' || echo '端口 80/443 未被占用'"
    bash output credential portAfterCmd |> output
    
    // 调试：打印服务日志
    "  [DEBUG] 服务日志内容:" |> cyan |> output
    let logCmd = $"cat {logFile} 2>/dev/null || echo '(日志文件不存在或为空)'"
    bash output credential logCmd |> output
    
    // 验证服务是否启动
    let running = checkDotNetServiceRunning output credential code
    if running then
        $"✓ {code} 服务启动成功" |> green |> output
        $"日志文件: {logFile}" |> yellow |> output
    else
        $"❌ {code} 服务启动失败" |> red |> output
        "--- 完整调试信息 ---" |> yellow |> output
    
    running

/// 启动服务（逐条执行，详细调试）
let startServiceVerbose output credential (code: string) =
    "\n--- 启动服务 (Verbose Debug) ---" |> cyan |> output
    
    let serverDir = $"~/Dev/{code}/Server"
    let logFile = $"/tmp/{code.ToLower()}.log"
    
    // 0. 启动前诊断
    "  0. 启动前诊断..." |> cyan |> output
    
    "    - Server 目录内容:" |> cyan |> output
    bash output credential $"ls -la {serverDir}/" |> output
    
    "    - Build 输出目录:" |> cyan |> output
    bash output credential $"ls -la {serverDir}/bin/Release/net10.0/ 2>/dev/null || echo '(build 输出目录不存在)'" |> output
    
    "    - .fsproj OutputType/TargetFramework:" |> cyan |> output
    bash output credential $"grep -i 'OutputType\\|TargetFramework' {serverDir}/*.fsproj 2>/dev/null || echo '(无法读取)'" |> output
    
    "    - 启动前 dotnet 进程:" |> cyan |> output
    bash output credential $"ps aux | grep -i '[d]otnet' || echo '(没有运行中的 dotnet 进程)'" |> output
    
    "    - 启动前端口占用:" |> cyan |> output
    bash output credential $"sudo ss -tlnp | grep -E ':80 |:443 ' || echo '(端口 80/443 未被占用)'" |> output
    
    // 1. 停止现有服务
    "  1. 停止现有服务..." |> cyan |> output
    
    let stopCmds = [|
        "sudo killall -9 dotnet || echo '没有运行中的 dotnet 进程'"
        "sudo fuser -k 80/tcp || echo '端口 80 未被占用'"
        "sudo fuser -k 443/tcp || echo '端口 443 未被占用'"
    |]
    stopCmds |> Array.iter (fun cmd ->
        let result = bash output credential cmd
        result |> output)
    
    "    - 停止后 dotnet 进程:" |> cyan |> output
    bash output credential $"ps aux | grep -i '[d]otnet' || echo '(没有运行中的 dotnet 进程)'" |> output
    
    // 2. 启动服务（捕获 PID）
    "  2. 启动服务..." |> cyan |> output
    let startCmd = $"cd {serverDir} && sudo nohup dotnet run --configuration Release > {logFile} 2>&1 & echo \"启动PID:$!\""
    let startResult = bash output credential startCmd
    "    启动命令输出:" |> cyan |> output
    startResult |> output
    
    // 3. 等待启动
    "  3. 等待服务启动（3秒）..." |> cyan |> output
    bash output credential "sleep 3" |> ignore
    
    // 4. 启动后诊断
    "  4. 启动后诊断..." |> cyan |> output
    
    "    - 启动后 dotnet 进程:" |> cyan |> output
    bash output credential $"ps aux | grep -i '[d]otnet' || echo '(没有运行中的 dotnet 进程)'" |> output
    
    "    - 启动后端口占用:" |> cyan |> output
    bash output credential $"sudo ss -tlnp | grep -E ':80 |:443 ' || echo '(端口 80/443 未被占用)'" |> output
    
    "    - 服务日志完整内容:" |> cyan |> output
    bash output credential $"cat {logFile} 2>/dev/null || echo '(日志文件不存在或为空)'" |> output
    
    // 5. 验证服务状态
    "  5. 验证服务状态..." |> cyan |> output
    let running = checkDotNetServiceRunning output credential code
    
    if running then
        "✓ 服务启动成功" |> green |> output
        $"日志文件: {logFile}" |> yellow |> output
    else
        "❌ 服务启动失败" |> red |> output
        "--- 完整调试信息已在上方输出 ---" |> yellow |> output
    
    running
