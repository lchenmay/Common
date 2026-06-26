module Util.Monitor

open System
open System.IO
open System.Diagnostics
open System.Runtime.InteropServices

// 跨平台系统监控工具，纯函数，无 Web 依赖
// 返回 (string * string) array 或 (string * float) array 供上层序列化为 JSON

let inline f__s (v:float) = v.ToString("0.##")
let inline i__s (v:int) = v.ToString()
let inline pct__s (used:float) (total:float) = 
    if total > 0.0 then (used / total * 100.0).ToString("0") + "% " + (used |> f__s) + "/" + (total |> f__s) else "0% 0/0"

let isLinux = RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

// Windows: GlobalMemoryStatusEx P/Invoke
[<Struct; StructLayout(LayoutKind.Sequential)>]
type MEMORYSTATUSEX =
    val mutable dwLength: uint32
    val mutable dwMemoryLoad: uint32
    val mutable ullTotalPhys: uint64
    val mutable ullAvailPhys: uint64
    val mutable ullTotalPageFile: uint64
    val mutable ullAvailPageFile: uint64
    val mutable ullTotalVirtual: uint64
    val mutable ullAvailVirtual: uint64
    val mutable ullAvailExtendedVirtual: uint64

[<DllImport("kernel32.dll", SetLastError = true)>]
extern bool GlobalMemoryStatusEx(MEMORYSTATUSEX& lpBuffer)

let private getWindowsMemory () =
    try
        let mutable mex = MEMORYSTATUSEX(dwLength = uint32(sizeof<MEMORYSTATUSEX>))
        if GlobalMemoryStatusEx(&mex) then
            let totalMB = float mex.ullTotalPhys / 1048576.0
            let availMB = float mex.ullAvailPhys / 1048576.0
            Some(totalMB, availMB)
        else None
    with _ -> None

// 获取系统总内存 MB（跨平台）
let getSystemTotalMemoryMB () =
    if isLinux then
        try
            let lines = File.ReadAllLines("/proc/meminfo")
            let totalLine = lines |> Array.tryFind (fun l -> l.StartsWith("MemTotal:"))
            match totalLine with
            | Some line ->
                let parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                if parts.Length >= 2 then float parts[1] / 1024.0 else 0.0
            | None -> 0.0
        with _ -> 0.0
    else
        match getWindowsMemory() with
        | Some(total,_) -> total
        | None -> 0.0

// 获取系统可用内存 MB（跨平台）
let getSystemAvailableMemoryMB () =
    if isLinux then
        try
            let lines = File.ReadAllLines("/proc/meminfo")
            let availLine = lines |> Array.tryFind (fun l -> l.StartsWith("MemAvailable:"))
            match availLine with
            | Some line ->
                let parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                if parts.Length >= 2 then float parts[1] / 1024.0 else 0.0
            | None -> 0.0
        with _ -> 0.0
    else
        match getWindowsMemory() with
        | Some(_,avail) -> avail
        | None -> 0.0

// 获取系统 CPU 使用率（跨平台，采样 100ms）
let getSystemCpuUsage () =
    if isLinux then
        try
            let readCpu () =
                let lines = File.ReadAllLines("/proc/stat")
                let cpuLine = lines |> Array.find (fun l -> l.StartsWith("cpu "))
                let parts = cpuLine.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                let nums = parts[1..] |> Array.map int64
                let total = nums |> Array.sum
                let idle = nums[3] // idle 是第4个字段
                total, idle
            let t1, i1 = readCpu ()
            System.Threading.Thread.Sleep(100)
            let t2, i2 = readCpu ()
            let totalDiff = float(t2 - t1)
            let idleDiff = float(i2 - i1)
            if totalDiff > 0.0 then (totalDiff - idleDiff) / totalDiff * 100.0 else 0.0
        with _ -> 0.0
    else
        try
            let proc = Process.GetCurrentProcess()
            let t1 = proc.TotalProcessorTime
            System.Threading.Thread.Sleep(100)
            let t2 = proc.TotalProcessorTime
            let elapsed = (t2 - t1).TotalMilliseconds
            let cores = Environment.ProcessorCount |> float
            if elapsed > 0.0 then (elapsed / 1000.0 * 100.0 / cores) |> min 100.0 else 0.0
        with _ -> 0.0

// 进程性能快照（增强：含系统级 CPU 和内存）
let procPerf () =
    let proc = Process.GetCurrentProcess()
    let gcMB = float(GC.GetTotalMemory(false)) / 1048576.0 |> f__s
    let wsMB = float proc.WorkingSet64 / 1048576.0 |> f__s
    let pmMB = float proc.PrivateMemorySize64 / 1048576.0 |> f__s
    let cpuProc = proc.TotalProcessorTime.TotalSeconds.ToString("0.0")
    let sysCpu = getSystemCpuUsage() |> f__s
    let sysMemTotal = getSystemTotalMemoryMB()
    let sysMemAvail = getSystemAvailableMemoryMB()
    let sysMemUsed = sysMemTotal - sysMemAvail
    let start = proc.StartTime.ToString("yyyy-MM-dd HH:mm:ss")
    let up = (DateTime.Now - proc.StartTime).ToString(@"dd\.hh\:mm\:ss")
    [| ("gcMemoryMB", gcMB); ("workingSetMB", wsMB); ("privateMemoryMB", pmMB)
       ("threadCount", proc.Threads.Count |> i__s); ("cpuTimeSec", cpuProc)
       ("startTime", start); ("uptime", up)
       ("sysCpuPct", sysCpu); ("sysMemPct", pct__s sysMemUsed sysMemTotal) |]

// 系统环境信息（增强：区分平台描述）
let sysInfo () =
    let platform =
        if isLinux then "Linux"
        elif isWindows then "Windows"
        else "Unknown"
    let osDesc =
        if isLinux then
            try File.ReadAllText("/etc/os-release").Split('\n') 
                |> Array.tryFind (fun l -> l.StartsWith("PRETTY_NAME="))
                |> Option.map (fun l -> l.Substring(13).Trim('"'))
                |> Option.defaultValue "Linux"
            with _ -> "Linux"
        else Environment.OSVersion.VersionString
    let uptimeDays = float Environment.TickCount64 / 86400000.0 |> f__s
    [| ("machineName", Environment.MachineName)
       ("osVersion", osDesc)
       ("is64bit", if Environment.Is64BitOperatingSystem then "true" else "false")
       ("processorCount", Environment.ProcessorCount |> i__s)
       ("clrVersion", Environment.Version.ToString())
       ("workingDir", Environment.CurrentDirectory)
       ("platform", platform)
       ("uptimeDays", uptimeDays)
       ("userName", Environment.UserName) |]

// 详细内存统计（增强：系统级内存使用 80% 123/456 格式）
let memDetail () =
    let proc = Process.GetCurrentProcess()
    let inline mb (bytes:int64) = float bytes / 1048576.0 |> f__s
    let sysTotal = getSystemTotalMemoryMB()
    let sysAvail = getSystemAvailableMemoryMB()
    let sysUsed = sysTotal - sysAvail
    [| ("gcMemoryMB", GC.GetTotalMemory(false) |> int64 |> mb)
       ("workingSetMB", proc.WorkingSet64 |> mb)
       ("privateMemoryMB", proc.PrivateMemorySize64 |> mb)
       ("virtualMemoryMB", proc.VirtualMemorySize64 |> mb)
       ("peakWorkingSetMB", proc.PeakWorkingSet64 |> mb)
       ("peakVirtualMemoryMB", proc.PeakVirtualMemorySize64 |> mb)
       ("gcGen0Count", GC.CollectionCount(0) |> i__s)
       ("gcGen1Count", GC.CollectionCount(1) |> i__s)
       ("gcGen2Count", GC.CollectionCount(2) |> i__s)
       ("systemMemory", pct__s sysUsed sysTotal) |]

// 磁盘空间（增强：80% 123/456 GB 格式）
let diskUsage () =
    let drives =
        if isLinux then 
            try 
                File.ReadAllLines("/proc/mounts")
                |> Array.choose (fun line ->
                    let parts = line.Split(' ')
                    if parts.Length >= 2 && parts[1].StartsWith("/") && not (parts[1].StartsWith("/sys")) 
                       && not (parts[1].StartsWith("/proc")) && not (parts[1].StartsWith("/dev")) then
                        Some parts[1]
                    else None)
                |> Array.distinct
                |> Array.truncate 8
            with _ -> [| "/" |]
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
            let display = if total > 0.0 then (used / total * 100.0).ToString("0") + "% " + (used |> f__s) + "/" + (total |> f__s) + " GB" else "0% 0/0 GB"
            let name = path.TrimEnd(Path.DirectorySeparatorChar).TrimEnd(':')
            Some(name, (total |> f__s, free |> f__s, used |> f__s, pct, display))
        with _ -> None)

// 健康检查
let healthCheck (inited:bool) =
    let proc = Process.GetCurrentProcess()
    let wsMB = float proc.WorkingSet64 / 1048576.0 |> f__s
    let up = (DateTime.Now - proc.StartTime).ToString(@"dd\.hh\:mm\:ss")
    let sysCpu = getSystemCpuUsage() |> f__s
    [| ("status", if inited then "healthy" else "degraded")
       ("memoryMB", wsMB); ("uptime", up)
       ("cpuPct", sysCpu) |]

// 本地获取当前目录的 git hash（纯函数，不依赖 bash）
let gitHashLocal () =
    try
        let proc = new Process()
        proc.StartInfo.FileName <- "git"
        proc.StartInfo.Arguments <- "rev-parse --short=8 HEAD"
        proc.StartInfo.RedirectStandardOutput <- true
        proc.StartInfo.UseShellExecute <- false
        proc.StartInfo.CreateNoWindow <- true
        proc.Start() |> ignore
        let hash = proc.StandardOutput.ReadToEnd().Trim()
        proc.WaitForExit(3000) |> ignore
        if hash.Length > 0 then hash else "-"
    with _ -> "-"

// 版本/编译信息（供前端显示编译号/版本号/git hash）
let versionInfo (projectCode:string) (buildTime:System.DateTime) =
    let buildTimeStr = buildTime.ToString("yyyy-MM-dd HH:mm:ss") + " UTC"
    let compileNumber =
        // 编译号 = 从 2026-01-01 起的天数.当天分钟数（如 176.1205 表示第176天12:05）
        let origin = DateTime(2026, 1, 1, 0, 0, 0, DateTimeKind.Utc)
        let elapsed = buildTime - origin
        let days = elapsed.TotalDays |> int
        let minutes = (elapsed.TotalHours - float(days * 24)) * 60.0 |> int
        sprintf "%d.%04d" days minutes
    let gitHash = gitHashLocal()
    [| ("projectCode", projectCode)
       ("compileNumber", compileNumber)
       ("buildTime", buildTimeStr)
       ("gitHash", gitHash) |]
