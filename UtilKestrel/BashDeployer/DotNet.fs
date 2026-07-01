module UtilKestrel.BashDeployer.DotNet

open Util.Linux.Bash
open Util.Monitor
open UtilKestrel.Types
open UtilKestrel.BashDeployer.Common

// ==================== 构建函数 ====================

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
                
                // 2. dotnet clean + dotnet publish (确保依赖 DLL 最新)
                // 注意：不执行 `dotnet add reference`，因为 `dotnet publish` 会自动解析项目引用
                "  - dotnet clean --configuration Release" |> cyan |> output
                bash output credential $"cd ~/{serverDir} && dotnet clean --configuration Release" |> ignore

                "  - dotnet publish --configuration Release --output /root/publish/{code}" |> cyan |> output
                let publishResult = bashWithTimeout output credential $"cd ~/{serverDir} && dotnet publish --configuration Release --output /root/publish/{code} --verbosity minimal /nowarn:NU1603" 300000
                publishResult |> output
                
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
            
            // 2. dotnet clean + dotnet publish (确保依赖 DLL 最新)
            // 注意：不执行 `dotnet add reference`，因为 `dotnet publish` 会自动解析项目引用
            "  - dotnet clean --configuration Release" |> cyan |> output
            bash output credential $"cd ~/{serverDir} && dotnet clean --configuration Release" |> ignore

            "  - dotnet publish --configuration Release --output /root/publish/{code}" |> cyan |> output
            let publishResult = bashWithTimeout output credential $"cd ~/{serverDir} && dotnet publish --configuration Release --output /root/publish/{code} --verbosity minimal /nowarn:NU1603" 300000
            publishResult |> output
            
            "✓ 后端构建完成" |> green |> output
            true
