module Util.Bash

open System
open System.Diagnostics
open System.Text

// ANSI 颜色
let cyan (s: string) = $"\u001b[36m{s}\u001b[0m"
let green (s: string) = $"\u001b[32m{s}\u001b[0m"
let red (s: string) = $"\u001b[31m{s}\u001b[0m"

/// 执行命令，同时输出到控制台并返回原始输出字符串
let exec output setDir (fileName: string) (args: string) : string =
    $"{fileName}: {args}" |> cyan |> output

    let psi = ProcessStartInfo(fileName, args)
    if not (String.IsNullOrWhiteSpace setDir) then
        psi.WorkingDirectory <- setDir
        
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    use proc = new Process(StartInfo = psi)

    let outputBuilder = StringBuilder()
    let errorBuilder = StringBuilder()

    proc.OutputDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            e.Data |> output
            outputBuilder.AppendLine(e.Data) |> ignore)

    proc.ErrorDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            e.Data |> output
            errorBuilder.AppendLine(e.Data) |> ignore)

    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    
    let timeout = 30000
    if not (proc.WaitForExit(timeout)) then
        proc.Kill()
        red $"命令执行超时 ({timeout}ms)" |> output
        ""

    elif proc.ExitCode <> 0 then
        $"ExitCode {proc.ExitCode}" |> red |> output
        errorBuilder.ToString()

    else
        outputBuilder.ToString()

/// SSH 远程执行，返回输出字符串
let bashOne output user server cmd : string =
    $"{user}@{server} " + cmd
    |> exec output "" "ssh"

/// SSH 远程执行，返回输出字符串
let bashMultiple output user server cmds : string =
    $"{user}@{server} " + (cmds |> String.concat " && ")
    |> exec output "" "ssh"


(*
PS C:\WINDOWS\system32> ssh-keygen -t rsa -b 4096 -C siduochen@hotmail.com
Generating public/private rsa key pair.
Enter file in which to save the key (C:\Users\RR/.ssh/id_rsa): 206.119.172.186
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in 206.119.172.186
Your public key has been saved in 206.119.172.186.pub
The key fingerprint is:
SHA256:R0jTmc9g302JWH1IzfCMy/Tqlb5w0VoDnFhCduowCBY siduochen@hotmail.com
The key's randomart image is:
+---[RSA 4096]----+
|     E. o..*o===.|
|    . ...oB.B.o*=|
|       ..+.B =oo+|
|         .+ +oooo|
|        S ..  oo+|
|         .     ++|
|             .oo.|
|             .+. |
|              .o.|
+----[SHA256]-----+
PS C:\WINDOWS\system32>
*)
//在本地生成密钥（如果还没有）
let email__SshKey email = 
    $"ssh-keygen -t rsa -b 4096 -C {email}"

let combine user server = 
    user + "@" + server

let remoteCopy_SshKey (user,server) file = 
    //"ssh-copy-id " + combine user server
    "cat " + file + " | ssh " + (combine user server) + " \"mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys\""

(*
let log s = printfn "%s" s

// 1. 建立持久连接
sshConnect log "root" "192.168.1.100"

// 2. 执行多个命令（复用连接）
bashWithControl log "root" "192.168.1.100" [
    "cd /var/www"
    "ls -la"
    "git status"
]

// 3. 关闭连接
sshDisconnect log "root" "192.168.1.100"

*)


/// 创建一个持久的 SSH 控制连接
let sshConnect output user server =
    "-Nf -M -S ~/.ssh/control-%r@%h:%p " + combine user server
    |> exec output "" "ssh" 
    |> ignore
    $"SSH 控制连接已建立: {user}@{server}"

/// 关闭 SSH 控制连接
let sshDisconnect output user server =
    "-O exit -S ~/.ssh/control-%r@%h:%p " + combine user server
    |> exec output "" "ssh"
    |> ignore
    $"SSH 控制连接已关闭: {user}@{server}"



