module Util.Monitor

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices

// 跨平台系统监控工具，纯函数，无 Web 依赖
// 返回 (string * string) array 或 (string * float) array 供上层序列化为 JSON

let inline f__s (v:float) = v.ToString("0.##")
let inline i__s (v:int) = v.ToString()

// 进程性能快照
let procPerf () =
    let proc = Process.GetCurrentProcess()
    let gcMB = float(GC.GetTotalMemory(false)) / 1048576.0 |> f__s
    let wsMB = float proc.WorkingSet64 / 1048576.0 |> f__s
    let pmMB = float proc.PrivateMemorySize64 / 1048576.0 |> f__s
    let cpu = proc.TotalProcessorTime.TotalSeconds.ToString("0.0")
    let start = proc.StartTime.ToString("yyyy-MM-dd HH:mm:ss")
    let up = (DateTime.Now - proc.StartTime).ToString(@"dd\.hh\:mm\:ss")
    [| ("gcMemoryMB", gcMB); ("workingSetMB", wsMB); ("privateMemoryMB", pmMB)
       ("threadCount", proc.Threads.Count |> i__s); ("cpuTimeSec", cpu)
       ("startTime", start); ("uptime", up) |]

// 系统环境信息
let sysInfo () =
    let platform =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then "Linux"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then "Windows"
        else "Unknown"
    let uptimeDays = float Environment.TickCount64 / 86400000.0 |> f__s
    [| ("machineName", Environment.MachineName)
       ("osVersion", Environment.OSVersion.ToString())
       ("is64bit", if Environment.Is64BitOperatingSystem then "true" else "false")
       ("processorCount", Environment.ProcessorCount |> i__s)
       ("clrVersion", Environment.Version.ToString())
       ("workingDir", Environment.CurrentDirectory)
       ("platform", platform)
       ("uptimeDays", uptimeDays)
       ("userName", Environment.UserName) |]

// 详细内存统计
let memDetail () =
    let proc = Process.GetCurrentProcess()
    let inline mb (bytes:int64) = float bytes / 1048576.0 |> f__s
    [| ("gcMemoryMB", GC.GetTotalMemory(false) |> int64 |> mb)
       ("workingSetMB", proc.WorkingSet64 |> mb)
       ("privateMemoryMB", proc.PrivateMemorySize64 |> mb)
       ("virtualMemoryMB", proc.VirtualMemorySize64 |> mb)
       ("peakWorkingSetMB", proc.PeakWorkingSet64 |> mb)
       ("peakVirtualMemoryMB", proc.PeakVirtualMemorySize64 |> mb)
       ("gcGen0Count", GC.CollectionCount(0) |> i__s)
       ("gcGen1Count", GC.CollectionCount(1) |> i__s)
       ("gcGen2Count", GC.CollectionCount(2) |> i__s) |]

// 磁盘空间（返回 (driveName, (totalGB, freeGB, usedGB, usagePct)) array）
let diskUsage () =
    let drives =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then [| "/" |]
        else
            DriveInfo.GetDrives()
            |> Array.filter (fun d -> d.IsReady)
            |> Array.map (fun d -> d.Name)
    drives |> Array.choose (fun path ->
        try
            let di = DriveInfo(path)
            let total = float di.TotalSize / 1073741824.0
            let free = float di.TotalFreeSpace / 1073741824.0
            let used = total - free
            let pct = if total > 0.0 then used / total * 100.0 |> f__s else "0"
            let name = path.TrimEnd(Path.DirectorySeparatorChar).TrimEnd(':')
            Some(name, (total |> f__s, free |> f__s, used |> f__s, pct))
        with _ -> None)

// 健康检查
let healthCheck (inited:bool) =
    let proc = Process.GetCurrentProcess()
    let wsMB = float proc.WorkingSet64 / 1048576.0 |> f__s
    let up = (DateTime.Now - proc.StartTime).ToString(@"dd\.hh\:mm\:ss")
    [| ("status", if inited then "healthy" else "degraded")
       ("memoryMB", wsMB); ("uptime", up) |]
