module Util.OS

open LanguagePrimitives

open System
open System.IO
open System.Text
open System.Runtime.InteropServices
open System.Management
open System.Diagnostics
open System.ServiceProcess

open Microsoft.Win32

[<Literal>]
let SW_HIDE = 0
let SW_SHOW = 5

[<DllImport("kernel32.dll")>]
extern IntPtr GetConsoleWindow()

[<DllImport("user32.dll")>]
extern bool ShowWindow(IntPtr hWnd, int nCmdShow)


[<DllImport(@"kernel32.dll", CallingConvention = CallingConvention.Cdecl)>]
extern bool GetVolumeInformation(
        string rootPathName,
        StringBuilder volumeNameBuffer,
        int volumeNameSize,
        uint& volumeSerialNumber,
        uint& maximumComponentLength,
        uint& fileSystemFlags,
        StringBuilder fileSystemNameBuffer,
        int nFileSystemNameSize)

// get memory usage
let mutable total_mem_size = 0L
let free_mem_counter = new PerformanceCounter("Memory", "Available MBytes")
let getMemUsage() =
    if total_mem_size = 0 then
        let searcher = new ManagementObjectSearcher("Select * From Win32_PhysicalMemory")
        for ram in searcher.Get() do
            total_mem_size <- total_mem_size + Convert.ToInt64(ram.GetPropertyValue("Capacity"))
        total_mem_size <- total_mem_size / (1024L * 1024L)
    let free_mem_size = int64(free_mem_counter.NextValue())
    total_mem_size, free_mem_size

let getVolumeByName name = 
    let VolumeNameSize = 255
    let FileSystemNameBufferSize = 255
    let volumeNameBuffer = new StringBuilder(VolumeNameSize)
    let mutable volumeSerialNumber = 0u
    let mutable maximumComponentLength = 0u
    let mutable fileSystemFeatures = 0u
    let fileSystemNameBuffer = new StringBuilder(FileSystemNameBufferSize)
    GetVolumeInformation(
        name,
        volumeNameBuffer,
        VolumeNameSize,
        &volumeSerialNumber,
        &maximumComponentLength,
        &fileSystemFeatures,
        fileSystemNameBuffer,
        FileSystemNameBufferSize) |> ignore

    //volumeNameBuffer.ToString() |> Console.WriteLine
    //fileSystemNameBuffer.ToString() |> Console.WriteLine
    //volumeSerialNumber.ToString() |> Console.WriteLine

    volumeNameBuffer.ToString(),fileSystemNameBuffer.ToString(),volumeSerialNumber

let getRootDriverInfo() = 
    let winroot = System.Environment.GetFolderPath System.Environment.SpecialFolder.Windows
    let dir = (new System.IO.DirectoryInfo(winroot)).Root
    System.IO.DriveInfo.GetDrives() |> Array.find(fun i -> i.Name.StartsWith dir.Name)
        
let getPhysicalHDDs() = 

    let mc = new ManagementClass("Win32_DiskDrive")
    let seqs = mc.GetInstances()
    let e = seqs.GetEnumerator()
    e.Reset()
    while e.MoveNext() do
        let hdd = e.Current

        //let s = hdd.Properties
        //let ee = s.GetEnumerator()
        //ee.Reset()
        //while ee.MoveNext() do
        //    ee.Current.Name.ToString() |> Console.WriteLine

        hdd["Model"].ToString() |> Console.WriteLine
        hdd["DeviceID"].ToString() |> Console.WriteLine
        hdd["SystemName"].ToString() |> Console.WriteLine
        hdd["SerialNumber"].ToString() |> Console.WriteLine

let getMechineID () =
    let _,_,volumeSerialNumber = 
        getRootDriverInfo().Name
        |> getVolumeByName

    volumeSerialNumber
    |> BitConverter.GetBytes
    |> Crypto.bin__sha256bin
    |> Bin.bytes__hex

let startProcess os path argAdder =

    let exepath = 
        match os with
        | PlatformID.Unix 
        | PlatformID.MacOSX -> "/path/to/your/file"
        | PlatformID.Win32NT -> path
        | _ -> ""

    let pinfo = new ProcessStartInfo(exepath)
    argAdder pinfo.ArgumentList
    pinfo.WindowStyle <- ProcessWindowStyle.Normal
    pinfo.CreateNoWindow <- false
    pinfo.UseShellExecute <- true

    Process.Start pinfo

let outcmd (p:Process) (s:string) = 
    p.StandardInput.WriteLine s

let runInProcess hiden job = 

    let p = new Process()
    p.StartInfo.FileName <- "cmd.exe"
    p.StartInfo.UseShellExecute <- false        //是否使用操作系统shell启动
    p.StartInfo.RedirectStandardInput <- true   //接受来自调用程序的输入信息
    p.StartInfo.RedirectStandardOutput <- true  //由调用程序获取输出信息
    p.StartInfo.RedirectStandardError <- true   //重定向标准错误输出
    p.StartInfo.CreateNoWindow <- hiden          //不显示程序窗口
    p.Start() |> ignore   //启动程序

    job p

    p.StandardInput.AutoFlush <- true
    p.StandardInput.Close()

    let reader = p.StandardOutput
    let error = p.StandardError
    let str1 = reader.ReadToEnd()
    let str2 = error.ReadToEnd()
    p.WaitForExit()
    p.Close()
    str2

let runStartProcess hiden job = 

    let p = new Process()
    p.StartInfo.FileName <- "cmd.exe"
    p.StartInfo.UseShellExecute <- true        //是否使用操作系统shell启动
    p.StartInfo.RedirectStandardInput <- true   //接受来自调用程序的输入信息
    p.StartInfo.RedirectStandardOutput <- true  //由调用程序获取输出信息
    p.StartInfo.RedirectStandardError <- true   //重定向标准错误输出
    // p.StartInfo.CreateNoWindow <- hiden          //不显示程序窗口
    p.Start() |> ignore   //启动程序

    job p

    p.StandardInput.AutoFlush <- true
    p.StandardInput.Close()

    let reader = p.StandardOutput
    let error = p.StandardError
    let str1 = reader.ReadToEnd()
    let str2 = error.ReadToEnd()
    p.WaitForExit()
    p.Close()
    str1 + str2

let runCmdHideInProcess cmd = 
    runInProcess true (fun p -> cmd |> outcmd p)

let runCmdInProcess cmd = 
    runInProcess false (fun p -> cmd |> outcmd p)

let runCmdsInProcess (cmds: string array) = 
    runInProcess false (fun p -> 
                    for cmd in cmds do
                        outcmd p cmd )

let InvokeExcute cmd =
    runInProcess false (fun p -> cmd + " & exit" |> outcmd p)

let InvokeExcuteStartProcess cmd =
    runInProcess false (fun p ->
        [|
            "powershell"
            "Start-Process"
            $"-FilePath %s{cmd}"
            "-WindowStyle Normal"
        |]
        |> String.concat " "
        |> outcmd p
    )

let runScript bat args =
    try
        let pinfo = new ProcessStartInfo(bat)
        args |> Array.iter( fun arg -> pinfo.ArgumentList.Add arg)
        pinfo.WindowStyle <- ProcessWindowStyle.Normal
        pinfo.CreateNoWindow <- false
        pinfo.UseShellExecute <- true

        Process.Start pinfo
        |> Some 
    with
    _ -> None

let runCMDScript cmd =
    try
        let pinfo = new ProcessStartInfo("CMD.exe")
        pinfo.Arguments <- ("/K " + cmd)
        //pinfo.ArgumentList.Add "/K"
        //pinfo.ArgumentList.Add "start"
        //pinfo.ArgumentList.Add bat
        //args |> Array.iter( fun arg -> pinfo.ArgumentList.Add arg)
        //pinfo.WindowStyle <- ProcessWindowStyle.Normal
        pinfo.CreateNoWindow <- false
        pinfo.UseShellExecute <- true
        // Console.WriteLine pinfo.FileName
        // Console.WriteLine pinfo.Arguments
        Process.Start pinfo
        |> Some 
    with
    _ -> None

let createShortcut srcfile dstlnk =
    [|
        "echo Set oWS = WScript.CreateObject(\"WScript.Shell\") >> CreateShortcut.vbs"
        "echo sLinkFile = \"" + dstlnk + "\" >> CreateShortcut.vbs"
        "echo Set oLink = oWS.CreateShortcut(sLinkFile) >> CreateShortcut.vbs"
        "echo oLink.TargetPath = \"" + srcfile + "\" >> CreateShortcut.vbs"
        "echo oLink.Save >> CreateShortcut.vbs"
        "cscript CreateShortcut.vbs"
        "del CreateShortcut.vbs"
    |]
    |> runCmdsInProcess

let createUrllnk url (path:string) =

    let writer = new IO.StreamWriter(path)                
    writer.WriteLine("[InternetShortcut]")
    writer.WriteLine("URL=" + url)
    writer.WriteLine("ShowCommand=3")
    writer.Flush()
    writer.Close()

let getInstallDir svrname =
    let mutable path = ""
    runCmdInProcess(sprintf "sc.exe qc \"%s\"" svrname).Split("\r\n")
    |> Array.iter(fun line -> 
        if line.Contains("BINARY_PATH_NAME") then
            path <- line.Split(" : ")[1])
    if path <> "" then
        path <- IO.Path.GetDirectoryName path
    path

let rec copyDirectory src dst recursive =

    if not (Directory.Exists(src)) then
        raise (DirectoryNotFoundException("source path doesn't exist"))

    if not (Directory.Exists(dst)) then
        System.IO.Directory.CreateDirectory(dst) |> ignore

    let di = DirectoryInfo(src)
    di.GetFiles()
    |> Array.iter(fun fi ->
                      let dstpath = Path.Combine(dst, fi.Name)
                      fi.CopyTo(dstpath, true) |> ignore)

    if recursive then
       di.GetDirectories()
       |> Array.iter(fun subdi -> 
                         let dstSubdir = Path.Combine(dst, subdi.Name)
                         copyDirectory subdi.FullName dstSubdir recursive)

type WinService(func) =
    inherit System.ServiceProcess.ServiceBase()
    override this.OnStart arg = func arg
    override this.OnStop() = ()

let launchWinSrv srvName dump func= 
    let svr = new WinService(func)
    svr.ServiceName <- srvName
    try
        ServiceBase.Run svr
    with ex ->
        dump()

let getLocalIPs () =
    let hostname = Net.Dns.GetHostName()
    Net.Dns.GetHostAddresses hostname
        |> Array.map(fun a -> a.ToString())
        |> Array.insertAt 0 "127.0.0.1"