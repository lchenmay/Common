module Util.Linux.PSQL

open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Text
open System.IO

open Util.Linux.Bash
open Util.Linux.Linux


let loadPathPSQL 
    output 
    credential =
    
    let porto,user,server,target,portArg = credentialExpand credential
    
    // 动态检测 psql 路径
    let getPsqlPathCmd = 
        "PSQL_PATH=$(ps aux | grep '[p]ostmaster' | grep -oP '/usr/pgsql-\\d+' | head -1)/bin/psql; " +
        "if [ ! -f \"$PSQL_PATH\" ]; then PSQL_PATH=$(find /usr -name psql -path '*/pgsql-*' 2>/dev/null | head -1); fi; " +
        "if [ ! -f \"$PSQL_PATH\" ]; then PSQL_PATH=$(which psql 2>/dev/null); fi; " +
        "echo $PSQL_PATH"
    
    let psqlPath = bash output credential getPsqlPathCmd
    
    // 验证路径是否有效 - 去除空白字符
    let cleanPath = psqlPath.Trim().Replace("\n", "").Replace("\r", "")
    
    if String.IsNullOrEmpty cleanPath || not (cleanPath.Contains("psql")) then
        "❌ psql not found on server" |> red |> output
        "请检查 PostgreSQL 是否已安装" |> yellow |> output
        // 尝试使用默认路径
        let defaultPaths = [|
            "/usr/bin/psql"
            "/usr/local/bin/psql"
            "/bin/psql"
            "/usr/pgsql-14/bin/psql"
            "/usr/pgsql-13/bin/psql"
            "/usr/pgsql-12/bin/psql"
        |]
        
        let mutable found = false
        let mutable foundPath = ""
        for defaultPath in defaultPaths do
            if not found then
                $"尝试默认路径: {defaultPath}" |> yellow |> output
                let checkCmd = $"if [ -f {defaultPath} ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
                let checkResult = bash output credential checkCmd
                if checkResult.Contains("EXISTS") then
                    $"✅ 找到 psql: {defaultPath}" |> green |> output
                    found <- true
                    foundPath <- defaultPath
                else
                    ()
        done
        
        // 如果仍然找不到，抛出异常
        if not found then
            failwith "psql not found on server. Please install PostgreSQL."
        else
            foundPath
    else
        $"✅ psql found: {cleanPath}" |> green |> output
        cleanPath


/// 检查 PostgreSQL 是否已配置远程访问
let checkPostgresRemoteConfigured output credential =
    try
        let psqlPath = loadPathPSQL output credential
        let psql__cmd = psqlpath__Cmd psqlPath
        
        // 检查 listen_addresses
        let checkListenCmd = "SHOW listen_addresses;" |> psql__cmd
        let result = bash output credential checkListenCmd
        
        if result.Contains("*") then
            "✓ PostgreSQL 已配置为监听所有地址 (*)" |> green |> output
            true
        elif result.Contains("localhost") then
            "⚠ PostgreSQL 当前只监听 localhost" |> yellow |> output
            false
        else
            "⚠ 无法确定 PostgreSQL 监听配置" |> yellow |> output
            false
    with ex ->
        $"⚠ 检查 PostgreSQL 配置时出错: {ex.Message}" |> yellow |> output
        false

/// 检查 PostgreSQL 端口是否可访问（从外部）
let checkPostgresPortAccessible output credential =
    let porto,user,server,target,portArg = credentialExpand credential
    
    // 使用 nc 或 telnet 检查端口
    let checkCmd = 
        $"nc -zv {server} 5432 2>&1 || " +
        $"timeout 5 telnet {server} 5432 2>&1 || " +
        $"echo 'PORT_CHECK_FAILED'"
    
    let result = bash output credential checkCmd
    
    if result.Contains("succeeded") || result.Contains("Connected") || result.Contains("open") then
        "✓ PostgreSQL 端口 5432 可从外部访问" |> green |> output
        true
    elif result.Contains("refused") || result.Contains("timeout") then
        "⚠ PostgreSQL 端口 5432 无法从外部访问" |> yellow |> output
        false
    else
        "⚠ 无法确定端口状态" |> yellow |> output
        false

/// 自动修复防火墙
let autoFixFirewall output credential =
    "\n--- 自动修复防火墙 ---" |> cyan |> output
    
    let cmds = [|
        // 检查 firewalld 是否运行
        "systemctl is-active firewalld > /dev/null 2>&1 && echo 'FIREWALLD_RUNNING' || echo 'FIREWALLD_NOT_RUNNING'"
        // 如果 firewalld 运行，开放端口
        "if systemctl is-active firewalld > /dev/null 2>&1; then firewall-cmd --permanent --add-port=5432/tcp && firewall-cmd --reload; fi"
        // 如果 iptables 存在，添加规则
        "if command -v iptables > /dev/null 2>&1; then iptables -C INPUT -p tcp --dport 5432 -j ACCEPT 2>/dev/null || iptables -I INPUT -p tcp --dport 5432 -j ACCEPT; fi"
        // 保存 iptables 规则
        "if command -v iptables-save > /dev/null 2>&1; then iptables-save > /etc/sysconfig/iptables 2>/dev/null || true; fi"
    |]
    
    for cmd in cmds do
        let result = bash output credential cmd
        result |> output
    
    "✓ 防火墙配置完成" |> green |> output

/// 验证 PostgreSQL 状态的 SQL 语句
let sqls_Validate psqlPath  = 

    let psql__cmd = psqlpath__Cmd psqlPath

    [|  
        "export LANG=en_US.UTF-8 && export LC_ALL=en_US.UTF-8"
        
        // 尝试启动 PostgreSQL（使用 pg_ctl 替代 systemctl）
        "echo '=== Ensuring PostgreSQL is running ==='"
        "sudo -u postgres /usr/pgsql-14/bin/pg_ctl -D /var/lib/pgsql/14/data status > /dev/null 2>&1 || sudo -u postgres /usr/pgsql-14/bin/pg_ctl -D /var/lib/pgsql/14/data start -w -t 30"
        "sleep 2"
        
        "echo '=== PostgreSQL 14 Status ==='"
        "ps aux | grep -q '[p]ostmaster.*14/data' && echo '[OK] PostgreSQL 14 is running' || echo '[FAIL] PostgreSQL 14 is not running'"
        "cd /tmp"
        
        // 显示 PostgreSQL 版本
        "echo '--- PostgreSQL Version ---'"
        "SELECT version();" |> psql__cmd
        
        // 显示连接信息 - 使用 \\conninfo 转义
        "echo '--- Connection Info ---'"
        "\\conninfo" |> psql__cmd
        
        // 显示当前用户信息
        "echo '--- Current User Info ---'"
        "SELECT current_user, session_user;" |> psql__cmd
        
        // 显示 postgres 用户密码状态
        "echo '--- Postgres User Password Status ---'"
        "SELECT usename, CASE WHEN passwd IS NULL THEN 'No password set' ELSE 'Password is set (hashed)' END as password_status FROM pg_shadow WHERE usename = 'postgres';" |> psql__cmd
        
        // 检查当前监听地址配置
        "echo '--- Current listen_addresses ---'"
        "SHOW listen_addresses;" |> psql__cmd
        
        // 检查当前端口配置
        "echo '--- Current port ---'"
        "SHOW port;" |> psql__cmd
        
        // 检查 pg_hba.conf 配置
        "echo '--- Current pg_hba.conf (filtered) ---'"
        "cat /var/lib/pgsql/14/data/pg_hba.conf | grep -v '^#' | grep -v '^$' | head -10" 
    |]

/// 配置 PostgreSQL 允许远程连接的 SQL 语句（仅当未配置时执行）
let sqls_ConfigureRemote psqlPath (password: string) =

    let psql__cmd = psqlpath__Cmd psqlPath

    [|  
        "export LANG=en_US.UTF-8 && export LC_ALL=en_US.UTF-8"
        
        // 确保 PostgreSQL 正在运行（使用 pg_ctl 替代 systemctl）
        "echo '=== Ensuring PostgreSQL is running ==='"
        "sudo -u postgres /usr/pgsql-14/bin/pg_ctl -D /var/lib/pgsql/14/data status > /dev/null 2>&1 || sudo -u postgres /usr/pgsql-14/bin/pg_ctl -D /var/lib/pgsql/14/data start -w -t 30"
        "sleep 2"
        
        "echo '=== Configuring PostgreSQL for remote access ==='"
        
        // 1. 备份配置文件
        "echo '--- Backing up config files ---'"
        "cp /var/lib/pgsql/14/data/postgresql.conf /var/lib/pgsql/14/data/postgresql.conf.bak.$(date +%Y%m%d_%H%M%S)"
        "cp /var/lib/pgsql/14/data/pg_hba.conf /var/lib/pgsql/14/data/pg_hba.conf.bak.$(date +%Y%m%d_%H%M%S)"
        
        // 2. 修改 postgresql.conf - 设置 listen_addresses（使用 @ 分隔符避免引号问题）
        "echo '--- Setting listen_addresses to * ---'"
        "sed -i 's@^#listen_addresses = .*@listen_addresses = '\\''*'\\''@' /var/lib/pgsql/14/data/postgresql.conf"
        "sed -i 's@^listen_addresses = .*@listen_addresses = '\\''*'\\''@' /var/lib/pgsql/14/data/postgresql.conf"
        // 如果没有 listen_addresses 行，追加
        "grep -q '^listen_addresses' /var/lib/pgsql/14/data/postgresql.conf || echo \"listen_addresses = '*'\" >> /var/lib/pgsql/14/data/postgresql.conf"
        "grep '^listen_addresses' /var/lib/pgsql/14/data/postgresql.conf"
        
        // 3. 修改 postgresql.conf - 确保端口正确
        "echo '--- Setting port to 5432 ---'"
        "sed -i 's@^#port = .*@port = 5432@' /var/lib/pgsql/14/data/postgresql.conf"
        "sed -i 's@^port = .*@port = 5432@' /var/lib/pgsql/14/data/postgresql.conf"
        "grep '^port' /var/lib/pgsql/14/data/postgresql.conf"
        
        // 4. 修改 pg_hba.conf - 允许远程连接
        "echo '--- Adding remote access to pg_hba.conf ---'"
        "grep -q 'host.*all.*all.*0.0.0.0/0.*md5' /var/lib/pgsql/14/data/pg_hba.conf || echo 'host    all             all             0.0.0.0/0            md5' >> /var/lib/pgsql/14/data/pg_hba.conf"
        
        // 5. 显示更新后的 pg_hba.conf
        "echo '--- Updated pg_hba.conf ---'"
        "cat /var/lib/pgsql/14/data/pg_hba.conf | grep -v '^#' | grep -v '^$'"
        
        // 6. 设置 postgres 用户密码
        $"echo '--- Setting postgres user password ---'"
        $"ALTER USER postgres WITH PASSWORD '{password}';" |> psql__cmd
        
        // 7. 开放防火墙端口
        "echo '--- Opening firewall port 5432 ---'"
        "systemctl is-active firewalld > /dev/null 2>&1 && firewall-cmd --permanent --add-port=5432/tcp && firewall-cmd --reload || echo 'firewalld not running'"
        "command -v iptables > /dev/null 2>&1 && iptables -C INPUT -p tcp --dport 5432 -j ACCEPT 2>/dev/null || iptables -I INPUT -p tcp --dport 5432 -j ACCEPT || true"
        
        // 8. 重启 PostgreSQL 服务（使用 pg_ctl）
        "echo '--- Restarting PostgreSQL ---'"
        "sudo -u postgres /usr/pgsql-14/bin/pg_ctl -D /var/lib/pgsql/14/data restart -w -t 30"
        
        // 9. 等待服务重启完成
        "sleep 3"
        
        // 10. 验证连接
        "echo '--- Verifying connection ---'"
        "SELECT version();" |> psql__cmd
        
        // 11. 显示当前的 listen_addresses
        "echo '--- Current listen_addresses ---'"
        "SHOW listen_addresses;" |> psql__cmd
        
        // 12. 显示监听端口状态
        "echo '--- Port listening status ---'"
        "ss -tlnp 2>/dev/null | grep 5432 || netstat -tlnp 2>/dev/null | grep 5432 || echo 'netstat/ss not available'"
        
        "echo '[OK] PostgreSQL configured for remote access'"
    |]

// ==================== 执行函数 ====================

/// 执行单个命令的辅助函数
let execCommand output credential cmd =
    $"\n--- Executing: {cmd} ---" |> cyan |> output
    let result = bash output credential cmd
    if not (String.IsNullOrEmpty result) then
        result |> output
    result

/// 验证 PostgreSQL 状态
let exeRemoteValidatePSQL 
    output
    credential =

    let porto,user,server,target,portArg = credentialExpand credential

    try
        let psqlPath = loadPathPSQL output credential
        sqls_Validate psqlPath 
        |> Array.iter (execCommand output credential >> ignore)

        $"\n>>> {server} PostgreSQL validation completed." |> cyan |> output
    with ex ->
        $"\nDeployment error: {ex.Message}" |> red |> output
        "Please check remote server status" |> yellow |> output

/// 配置 PostgreSQL 允许远程连接（仅当需要时）- 包含自动修复
let exeRemoteConfigurePSQL
    output
    (credential: Credential)
    (postgresPwd: string) =

    let porto,user,server,target,portArg = credentialExpand credential

    try
        // 1. 检查是否已配置
        let alreadyConfigured = checkPostgresRemoteConfigured output credential
        
        if alreadyConfigured then
            "✓ PostgreSQL 已配置远程访问，跳过配置步骤" |> green |> output
            
            // 2. 检查端口是否可访问
            let portAccessible = checkPostgresPortAccessible output credential
            
            if not portAccessible then
                "⚠ 端口不可访问，尝试自动修复防火墙..." |> yellow |> output
                autoFixFirewall output credential
                
                // 重新检查
                let retryAccessible = checkPostgresPortAccessible output credential
                if retryAccessible then
                    "✓ 端口已开放" |> green |> output
                else
                    "⚠ 端口仍不可访问，请手动检查防火墙设置" |> yellow |> output
            else
                "✓ 端口可访问" |> green |> output
            
            // 打印连接信息
            let conn = $"Host={server};Port=5432;Username=postgres;Password={postgresPwd};Database=postgres;SSL Mode=Disable"
            "📋 PostgreSQL 连接信息:" |> cyan |> output
            "========================================" |> cyan |> output
            $"  Host: {server}" |> green |> output
            $"  Port: 5432" |> green |> output
            $"  Username: postgres" |> green |> output
            $"  Password: {postgresPwd}" |> green |> output
            $"  Database: postgres" |> green |> output
            $"  SSL Mode: Disable" |> green |> output
            "========================================" |> cyan |> output
            conn
        else
            "⚠ PostgreSQL 未配置远程访问，开始配置..." |> yellow |> output
            
            let psqlPath = loadPathPSQL output credential

            sqls_ConfigureRemote psqlPath postgresPwd
            |> Array.iter (execCommand output credential >> ignore)

            $"\n>>> {server} PostgreSQL remote configuration completed." |> cyan |> output
            
            // 验证配置是否成功
            let verifyConfigured = checkPostgresRemoteConfigured output credential
            if verifyConfigured then
                "✓ 配置验证通过" |> green |> output
            else
                "⚠ 配置可能未生效，尝试重启 PostgreSQL..." |> yellow |> output
                let restartCmd = "sudo -u postgres /usr/pgsql-14/bin/pg_ctl -D /var/lib/pgsql/14/data restart -w -t 30"
                bash output credential restartCmd |> ignore
                // 等待重启完成
                bash output credential "sleep 3" |> ignore
                
                let retryConfigured = checkPostgresRemoteConfigured output credential
                if retryConfigured then
                    "✓ 配置验证通过" |> green |> output
                else
                    "⚠ 配置仍未生效，请手动检查" |> yellow |> output
            
            // 检查端口
            let portAccessible = checkPostgresPortAccessible output credential
            if not portAccessible then
                "⚠ 端口不可访问，尝试自动修复防火墙..." |> yellow |> output
                autoFixFirewall output credential
                
                let retryAccessible = checkPostgresPortAccessible output credential
                if retryAccessible then
                    "✓ 端口已开放" |> green |> output
                else
                    "⚠ 端口仍不可访问，请手动检查防火墙设置" |> yellow |> output
            else
                "✓ 端口可访问" |> green |> output
            
            // 打印连接信息
            let conn = $"Host={server};Port=5432;Username=postgres;Password={postgresPwd};Database=postgres;SSL Mode=Disable"
            "📋 PostgreSQL 连接信息:" |> cyan |> output
            "========================================" |> cyan |> output
            $"  Host: {server}" |> green |> output
            $"  Port: 5432" |> green |> output
            $"  Username: postgres" |> green |> output
            $"  Password: {postgresPwd}" |> green |> output
            $"  Database: postgres" |> green |> output
            $"  SSL Mode: Disable" |> green |> output
            "========================================" |> cyan |> output
            "⚠ 注意: 如果从外部连接，请确保防火墙允许端口 5432" |> yellow |> output
            conn
    with ex ->
        $"\nConfiguration error: {ex.Message}" |> red |> output
        "Please check remote server status" |> yellow |> output
        ""