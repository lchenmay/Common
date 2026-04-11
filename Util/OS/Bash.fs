module Util.Bash

open System
open System.Diagnostics
open System.Runtime.InteropServices

// ANSI 颜色处理
let cyan (s: string) = $"\u001b[36m{s}\u001b[0m"
let green (s: string) = $"\u001b[32m{s}\u001b[0m"
let red (s: string) = $"\u001b[31m{s}\u001b[0m"

/// 改进后的即时执行函数
let exec output setDir (fileName: string) (args: string) =
    $"{fileName}: {args}" |> cyan |> output

    let psi = ProcessStartInfo(fileName, args)
    if not (String.IsNullOrWhiteSpace setDir) then
        psi.WorkingDirectory <- setDir
        
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true

    use proc = new Process(StartInfo = psi)

    // 订阅输出事件，实现即时获取
    proc.OutputDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            e.Data |> output)

    proc.ErrorDataReceived.Add(fun e -> 
        if not (String.IsNullOrEmpty e.Data) then 
            // 某些工具（如 git）会将进度信息发往 stderr
            e.Data |> output)

    proc.Start() |> ignore
        
    // 开始异步读取流
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
        
    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        $"ExitCode {proc.ExitCode}" |> red |> output

    (proc.ExitCode, "", "") // 由于是即时输出，不再返回完整的 res 字符串

let bash output user server cmds =
    $"{user}@{server} " + (cmds |> String.concat " && ")
    |> exec output "" "ssh" 
    |> ignore 

