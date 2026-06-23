module Util.Linux.PSQL

open System
open System.Runtime.InteropServices
open System.Diagnostics
open System.Text
open System.IO

open Util.Linux.Bash
open Util.Linux.Linux
open System.Diagnostics


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
    
    if String.IsNullOrEmpty cleanPath || cleanPath.Contains("psql") |> not then
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
        
        let tryPath = 
            defaultPaths 
            |> Array.tryFind (fun defaultPath ->
                $"尝试默认路径: {defaultPath}" |> yellow |> output
                let checkCmd = $"if [ -f {defaultPath} ]; then echo 'EXISTS'; else echo 'NOT_EXISTS'; fi"
                let checkResult = bash output credential checkCmd
                if checkResult.Contains("EXISTS") then
                    $"✅ 找到 psql: {defaultPath}" |> green |> output
                    true
                else
                    false)
        
        match tryPath with
        | Some foundPath -> foundPath
        | None -> failwith "psql not found on server. Please install PostgreSQL."
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

/// 检查 PostgreSQL 端口是否可访问（从外部）- 修复 fi 问题
let checkPostgresPortAccessible output credential =
    let porto,user,server,target,portArg = credentialExpand credential
    
    // 使用 curl 从外部测试
    let curlCmd = $"curl -v --connect-timeout 5 telnet://{server}:5432 2>&1 | grep -q 'Connected' && echo 'OPEN' || echo 'CLOSED'"
    let curlResult = bash output credential curlCmd
    
    if curlResult.Contains("OPEN") then
        "✓ PostgreSQL 端口 5432 可从外部访问" |> green |> output
        true
    else
        // 使用 bash /dev/tcp 测试（内层用单引号避免SSH引号冲突）
        let bashTestCmd = $"timeout 3 bash -c 'echo >/dev/tcp/{server}/5432' 2>/dev/null && echo 'OPEN' || echo 'CLOSED'"
        let bashResult = bash output credential bashTestCmd
        
        if bashResult.Contains("OPEN") then
            "✓ PostgreSQL 端口 5432 可从外部访问" |> green |> output
            true
        else
            // 使用本地 ss 检查
            let localCheckCmd = "ss -tlnp 2>/dev/null | grep -q ':5432' && echo 'LISTENING' || echo 'NOT_LISTENING'"
            let localResult = bash output credential localCheckCmd
            
            if localResult.Contains("LISTENING") then
                "⚠ PostgreSQL 在本地监听，但外部无法访问（防火墙/安全组问题）" |> yellow |> output
                false
            else
                "⚠ 无法确定端口状态" |> yellow |> output
                false

/// 自动修复防火墙 - 修复 fi 问题
let autoFixFirewall output credential =
    "\n--- 自动修复防火墙 ---" |> cyan |> output
    
    let cmds = [|
        // 1. 检查并启动 firewalld（如果可用）
        "if command -v firewalld > /dev/null 2>&1; then systemctl start firewalld 2>/dev/null || true; fi"
        // 2. 检查 firewalld 是否运行
        "systemctl is-active firewalld > /dev/null 2>&1 && echo 'FIREWALLD_RUNNING' || echo 'FIREWALLD_NOT_RUNNING'"
        // 3. 如果 firewalld 运行，开放端口
        "if systemctl is-active firewalld > /dev/null 2>&1; then firewall-cmd --permanent --add-port=5432/tcp 2>/dev/null && firewall-cmd --reload 2>/dev/null; fi"
        // 4. 如果 iptables 存在，添加规则
        "if command -v iptables > /dev/null 2>&1; then iptables -C INPUT -p tcp --dport 5432 -j ACCEPT 2>/dev/null || iptables -I INPUT -p tcp --dport 5432 -j ACCEPT 2>/dev/null; fi"
        // 5. 保存 iptables 规则
        "if command -v iptables-save > /dev/null 2>&1; then iptables-save > /etc/sysconfig/iptables 2>/dev/null || true; fi"
        // 6. 提示
        "echo '如果以上命令执行后仍无法连接，请检查云服务商安全组/防火墙规则'"
    |]
    
    cmds |> Array.iter (fun cmd ->
        let result = bash output credential cmd
        result |> output)
    
    "✓ 防火墙配置完成" |> green |> output
    
    "📋 手动检查命令:" |> cyan |> output
    "  ss -tlnp | grep 5432" |> yellow |> output
    "  curl -v telnet://206.119.172.186:5432" |> yellow |> output
    "  如果使用云服务器，请在安全组中开放 5432 端口" |> yellow |> output

/// 检查 PostgreSQL 实际监听地址
let checkPostgresActualListening output credential =
    let cmd = "ss -tlnp 2>/dev/null | grep 5432 || netstat -tlnp 2>/dev/null | grep 5432"
    let result = bash output credential cmd
    result |> output
    
    if result.Contains("0.0.0.0:5432") || result.Contains("*:5432") || result.Contains(":::5432") then
        "✓ PostgreSQL 正在监听所有接口 (0.0.0.0:5432)" |> green |> output
        true
    elif result.Contains("127.0.0.1:5432") then
        "⚠ PostgreSQL 只监听本地 (127.0.0.1:5432)" |> yellow |> output
        false
    else
        "⚠ 无法确定 PostgreSQL 监听地址" |> yellow |> output
        false

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
        
        // 7. 开放防火墙端口 - 多种方式
        "echo '--- Opening firewall port 5432 ---'"
        // firewalld
        "systemctl is-active firewalld > /dev/null 2>&1 && firewall-cmd --permanent --add-port=5432/tcp && firewall-cmd --reload || echo 'firewalld not running'"
        // iptables
        "command -v iptables > /dev/null 2>&1 && iptables -C INPUT -p tcp --dport 5432 -j ACCEPT 2>/dev/null || iptables -I INPUT -p tcp --dport 5432 -j ACCEPT 2>/dev/null || true"
        // 检查是否 docker 环境
        "command -v docker > /dev/null 2>&1 && echo '检测到 Docker 环境，请确保容器端口已映射' || true"
        
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
        
        // 13. 显示防火墙状态
        "echo '--- Firewall status ---'"
        "iptables -L -n | grep 5432 2>/dev/null || echo 'iptables rules not found'"
        
        "echo '[OK] PostgreSQL configured for remote access'"
    |]

// ==================== NAT 检测与 SSH 隧道 ====================

/// 检测远程服务器是否位于云 NAT 网关之后（5432 端口未转发）
/// 判断逻辑: 服务器本地监听 5432 但自身无法通过公网 IP 访问自己
let exeCheckIfBehindNAT output credential =
    let porto,user,server,target,portArg = credentialExpand credential
    
    "\n--- 检测云 NAT 环境 ---" |> cyan |> output
    
    // 1. 确认 PostgreSQL 本地监听正常
    let localListenCmd = 
        "ss -tlnp 2>/dev/null | grep -q ':5432' && echo 'LISTENING' || echo 'NOT_LISTENING'"
    let localResult = bash output credential localListenCmd
    
    if localResult.Contains("LISTENING") |> not then
        "PostgreSQL 未在本地监听 5432，跳过 NAT 检测" |> yellow |> output
        false
    else
        // 2. 从服务器自身测试公网 IP 端口可达性
        //    如果服务器自己都连不上自己的公网 IP:5432，说明是云 NAT 问题
        //    （云厂商安全组/浮动 IP 未转发该端口）
        let selfTestCmd = 
            $"timeout 3 bash -c 'echo >/dev/tcp/{server}/5432' 2>/dev/null " +
            "&& echo 'SELF_REACHABLE' || echo 'SELF_UNREACHABLE'"
        let selfResult = bash output credential selfTestCmd
        
        if selfResult.Contains("SELF_REACHABLE") then
            $"✓ 服务器公网端口 {server}:5432 可达" |> green |> output
            false
        else
            // 3. 获取内网 IP 确认 NAT
            let ipCmd = "hostname -I 2>/dev/null | awk '{print $1}'"
            let internalIp = (bash output credential ipCmd).Trim()
            
            $"🔍 检测到云 NAT 环境" |> orange |> output
            $"   公网 IP: {server}" |> yellow |> output
            $"   内网 IP: {internalIp}" |> yellow |> output
            $"   端口 5432 在 NAT 网关未转发" |> yellow |> output
            true

/// 为 PostgreSQL 建立 SSH 隧道（用于 NAT 环境绕过云防火墙）
/// 返回 (localPort, tunnelConnString) option
let exeSetupPSQLTunnel output credential postgresPwd =
    let porto,user,server,target,portArg = credentialExpand credential
    
    "\n--- 建立 PostgreSQL SSH 隧道 ---" |> cyan |> output
    
    // 从 55432 开始查找可用端口（与 5432 区分）
    let tunnelPort = findAvailablePort 55432
    
    $"尝试端口: {tunnelPort}" |> cyan |> output
    
    let success = startSSHTunnel output tunnelPort credential 5432
    
    if success then
        // 等待隧道稳定
        System.Threading.Thread.Sleep 1500
        
        let conn = 
            $"Host=localhost;Port={tunnelPort};" +
            $"Username=postgres;Password={postgresPwd};" +
            $"Database=postgres;SSL Mode=Disable"
        
        "========================================" |> cyan |> output
        "📋 SSH 隧道 PostgreSQL 连接信息:" |> green |> output
        "========================================" |> cyan |> output
        $"  Host: localhost (通过 SSH 隧道)" |> green |> output
        $"  Tunnel: localhost:{tunnelPort} -> {server}:5432" |> green |> output
        $"  Port: {tunnelPort}" |> green |> output
        $"  Username: postgres" |> green |> output
        $"  Password: {postgresPwd}" |> green |> output
        $"  Database: postgres" |> green |> output
        $"  Connection String:" |> cyan |> output
        $"  {conn}" |> yellow |> output
        "========================================" |> cyan |> output
        "💡 隧道在本次部署会话期间保持活跃" |> cyan |> output
        "   也可手动建立: ssh -L 55432:localhost:5432 -N {user}@{server}" |> cyan |> output
        
        Some (tunnelPort, conn)
    else
        $"❌ SSH 隧道建立失败" |> red |> output
        None

// ==================== 执行函数 ====================

/// 执行单个命令的辅助函数
let execCommand output credential cmd =
    $"\n--- Executing: {cmd} ---" |> cyan |> output
    let result = bash output credential cmd
    if result |> String.IsNullOrEmpty |> not then
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

/// 解析端口可达性: 防火墙修复 → NAT检测 → SSH隧道，返回最终连接字符串
let private resolvePortAndConnection output credential postgresPwd =
    let porto,user,server,target,portArg = credentialExpand credential
    
    // 检查端口是否可访问
    let mutable portAccessible = checkPostgresPortAccessible output credential
    
    if portAccessible |> not then
        "⚠ 端口不可访问，尝试自动修复防火墙..." |> yellow |> output
        autoFixFirewall output credential
        
        portAccessible <- checkPostgresPortAccessible output credential
        if portAccessible then
            "✓ 端口已开放" |> green |> output
    
    if portAccessible then
        // 端口可达 → 使用直连
        "✓ 端口可访问，使用直连模式" |> green |> output
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
        // 端口不可达 → 检查是否为 NAT 环境
        "⚠ 服务器防火墙已开放但端口仍不可达" |> yellow |> output
        
        let behindNAT = exeCheckIfBehindNAT output credential
        
        if behindNAT then
            // NAT 环境 → 自动建立 SSH 隧道
            "🔧 自动建立 SSH 隧道绕过云 NAT..." |> cyan |> output
            
            match exeSetupPSQLTunnel output credential postgresPwd with
            | Some (tunnelPort, tunnelConn) ->
                tunnelConn
            | None ->
                // 隧道失败，回退到直连信息（带警告）
                "❌ SSH 隧道建立失败，回退到直连模式" |> red |> output
                let conn = $"Host={server};Port=5432;Username=postgres;Password={postgresPwd};Database=postgres;SSL Mode=Disable"
                "📋 直连 PostgreSQL 连接信息 (⚠ 可能不可用):" |> yellow |> output
                "========================================" |> yellow |> output
                $"  Host: {server}" |> yellow |> output
                $"  Port: 5432" |> yellow |> output
                $"  Username: postgres" |> yellow |> output
                $"  Password: {postgresPwd}" |> yellow |> output
                $"  Database: postgres" |> yellow |> output
                $"  SSL Mode: Disable" |> yellow |> output
                "========================================" |> yellow |> output
                "📋 请手动检查以下内容:" |> yellow |> output
                "  1. 云服务商安全组是否开放了 5432 端口" |> yellow |> output
                "  2. 服务器防火墙是否开放了 5432 端口" |> yellow |> output
                "  3. 手动建立隧道: ssh -L 55432:localhost:5432 -N {user}@{server}" |> yellow |> output
                conn
        else
            // 非 NAT 环境 → 防火墙问题或其他
            "⚠ 非 NAT 环境但端口不可达，可能是防火墙问题" |> yellow |> output
            "📋 请手动检查以下内容:" |> yellow |> output
            "  1. 云服务商安全组是否开放了 5432 端口" |> yellow |> output
            "  2. 服务器防火墙是否开放了 5432 端口" |> yellow |> output
            "  3. PostgreSQL 是否正在运行" |> yellow |> output
            "  4. 使用命令检查: ss -tlnp | grep 5432" |> yellow |> output
            let conn = $"Host={server};Port=5432;Username=postgres;Password={postgresPwd};Database=postgres;SSL Mode=Disable"
            "📋 直连 PostgreSQL 连接信息 (⚠ 可能不可用):" |> yellow |> output
            "========================================" |> yellow |> output
            $"  Host: {server}" |> yellow |> output
            $"  Port: 5432" |> yellow |> output
            $"  Username: postgres" |> yellow |> output
            $"  Password: {postgresPwd}" |> yellow |> output
            $"  Database: postgres" |> yellow |> output
            $"  SSL Mode: Disable" |> yellow |> output
            "========================================" |> yellow |> output
            conn

/// 配置 PostgreSQL 允许远程连接（仅当需要时）- 包含自动修复 + NAT检测 + SSH隧道
let exeRemoteConfigurePSQL
    output
    credential
    postgresPwd =

    let porto,user,server,target,portArg = credentialExpand credential

    try
        // 1. 检查是否已配置
        let alreadyConfigured = checkPostgresRemoteConfigured output credential
        
        if alreadyConfigured then
            "✓ PostgreSQL 已配置远程访问，跳过配置步骤" |> green |> output
            
            // 2. 检查实际监听地址
            let listeningOk = checkPostgresActualListening output credential
            
            if listeningOk |> not then
                "⚠ PostgreSQL 未监听所有接口，尝试修复..." |> yellow |> output
                // 修改配置
                let psqlPath = loadPathPSQL output credential
                let fixCmd1 = $"sed -i 's@^listen_addresses = .*@listen_addresses = '\\''*'\\''@' /var/lib/pgsql/14/data/postgresql.conf"
                let fixCmd2 = $"grep -q '^listen_addresses' /var/lib/pgsql/14/data/postgresql.conf || echo \"listen_addresses = '*'\" >> /var/lib/pgsql/14/data/postgresql.conf"
                bash output credential fixCmd1 |> ignore
                bash output credential fixCmd2 |> ignore
                // 重启
                let restartCmd = "sudo -u postgres /usr/pgsql-14/bin/pg_ctl -D /var/lib/pgsql/14/data restart -w -t 30"
                bash output credential restartCmd |> ignore
                bash output credential "sleep 3" |> ignore
                "✓ 配置已修复" |> green |> output
            
            // 3. 检查端口可达性 → 自动检测NAT → 建立SSH隧道
            resolvePortAndConnection output credential postgresPwd
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
            
            // 检查实际监听
            checkPostgresActualListening output credential |> ignore
            
            // 检查端口可达性 → 自动检测NAT → 建立SSH隧道
            resolvePortAndConnection output credential postgresPwd
    with ex ->
        $"\nConfiguration error: {ex.Message}" |> red |> output
        "Please check remote server status" |> yellow |> output
        ""