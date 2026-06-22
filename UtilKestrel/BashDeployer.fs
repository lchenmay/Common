module UtilKestrel.BashDeployer

open System
open System.IO
open System.Threading
open System.Collections.Generic

open Util.Bash
open Util.ADT
open Util.Bin
open Util.Db
open Util.DbTx
open Util.Collection
open Util.CollectionModDict
open Util.Orm
open Util.Http
open Util.HttpServer
open Util.Text
open Util.Json

open UtilKestrel.Types
open UtilKestrel.Ctx
open UtilKestrel.Db
open UtilKestrel.Api

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
    (devDir,gitEmail) = 

    let porto,user,server,target,portArg = credentialExpand credential

    let isConfigured = checkSshKeyConfiguredWithTimeout output credential 30000

    if isConfigured then
        "✓ SSH 免密登录已配置，跳过密钥生成和复制步骤" |> green |> output
    else
        "⚠ SSH 免密登录未配置，开始配置..." |> yellow |> output
    
        // 2a. 简要提示公钥认证（不检查，避免超时）
        "2a. 检查服务器公钥认证..." |> cyan |> output
        "提示：请确保服务器已启用 PubkeyAuthentication" |> yellow |> output
        let enableCmd = 
            $"ssh {portArg} {target} \"sed -i 's/^#*PubkeyAuthentication.*/PubkeyAuthentication yes/' /etc/ssh/sshd_config && systemctl restart sshd\""
        $"如未启用，请手动执行: {enableCmd}" |> orange |> output
        "" |> output
    
        // 2b. 生成 SSH 密钥
        let rsaFile = devDir + "/id_rsa"
        let pubFile = rsaFile + ".pub"
        
        if File.Exists pubFile then
            $"公钥已存在: {pubFile}" |> green |> output
        else
            $"生成 SSH 密钥 {pubFile}..." |> cyan |> output
            email__SshKey gitEmail |> orange |> output
            "请手动执行上述命令生成密钥（一路回车使用默认值）" |> yellow |> output
            "完成后按任意键继续..." |> yellow |> output
            Console.ReadKey() |> ignore
            "\n" |> output
    
        // 2c. 获取复制公钥的命令
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
        
            // 循环检查 SSH 配置，直到成功或用户取消
            let mutable retryCount = 0
            let maxRetries = 5
            let mutable configuredAfterRetry = false
            
            while not configuredAfterRetry && retryCount < maxRetries do
                retryCount <- retryCount + 1
                $"检查 SSH 配置 (第 {retryCount} 次)..." |> green |> output
                
                configuredAfterRetry <- checkSshKeyConfiguredWithTimeout output credential 30000
                
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
    
    // 3. 测试 SSH 连接
    "3. 测试 SSH 连接..." |> cyan |> output
    // 用快速检查代替完整命令
    let isConnected = checkSshKeyConfiguredWithTimeout output credential 10000
    if isConnected then
        "✓ SSH 连接正常" |> green |> output
    else
        "⚠ SSH 连接异常，继续执行..." |> yellow |> output

let exeRemote 
    output
    credential =

    let porto,user,server,target,portArg = credentialExpand credential

    let remoteCmds = 
        [| |]
        //[|  $"cd ~/{deployDir}"
        //    "git fetch --all"
        //    "git reset --hard origin/main"
        //    "sudo killall -9 dotnet || true"
        //    "sudo fuser -k 80/tcp || true"
        //    "sudo fuser -k 443/tcp || true"
        //    $"cd ~/{deployDir}/vscode"
        //    "/root/.bun/bin/bun install"
        //    "/root/.bun/bin/bun add vite @vitejs/plugin-vue @vitejs/plugin-vue-jsx @vitejs/plugin-basic-ssl -D"
        //    "/root/.bun/bin/bun generateRoutes.cjs"
        //    "/root/.bun/bin/bunx --/root/.bun/bin/bun vite build --emptyOutDir"
        //    "cd .."
        //    $"cd ~/{deployDir}/Server"
        //    "sudo dotnet run &" |]
        // [|  "ls"  |]
    
    try
        remoteCmds |> bashMultiple output credential |> ignore
        $">>> {server} 部署完成。" |> cyan |> output
    with ex ->
        $"部署过程中发生错误: {ex.Message}" |> red |> output
        "请检查远程服务器状态" |> yellow |> output

let routine output credential (gitName, gitEmail) (devDir, deployDir, fsRoot) =

    let porto,user,server,target,portArg = credentialExpand credential

    // 设置 SSH 私钥路径
    Util.Bash.sshPrivateKeyPath <- devDir + "/id_rsa"
    
    $">>> 开始部署至 {user}@{server}..." |> cyan |> output
    
    // 1. 本地：切换目录
    "1. 切换到项目目录: " + devDir |> cyan |> output
    let exeLocal args = exec output devDir "powershell" args |> ignore
    "cd " + devDir |> exeLocal
    
    // 2. 检查 SSH 免密登录是否已配置
    "2. 检查 SSH 免密登录状态..." |> cyan |> output
    checkSSHAuth
        output
        credential
        (devDir,gitEmail)
    
    // 4. 远程部署
    "4. 执行远程部署..." |> cyan |> output
    exeRemote
        output
        credential
