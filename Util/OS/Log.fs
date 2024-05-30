module Util.Log

open System

type LogLevelEnum =
    | Debug = 0
    | Info = 1
    | Warning = 2 
    | Error = 3
    | Fatal = 4

let print_ (msg: string, enable: bool) =
    if(enable) then
        let text = "[" + Text.now_time() + "] " + msg
        Console.WriteLine text

let print msg =
    let text = "[" + Text.now_time() + "] " + msg
    print_(text, true)

let print_start msg =
    let text = "[" + Text.now_time() + "] " + msg + " ... "
    Console.Write text

let print_end() =
    let text = "Done. [" + Text.now_time() + "]"
    Console.WriteLine text

type LoggingBuilder(logger:string -> unit) =

    member this.Bind(x, f) =
        logger x
        f x

    member this.Return(x) = x

let get_log_path(file) =
    let log_directory = "C:\DebugLogs"
    let local_log_directory = "D:\DebugLogs"
        
    if (System.Environment.MachineName = "DESKTOP-8VAEQPJ") then 
        [|local_log_directory;file|] |> System.IO.Path.Combine
    else
        [|log_directory;file|] |> System.IO.Path.Combine

let log text = 
    async {
        let file = "DebugLog"+DateTime.UtcNow.ToString("yyyyMMddHH")+".txt"
        let path = get_log_path(file)
        let date = [|DateTime.UtcNow.ToString("yyyyMMddHHmmss")|]
        let content = Array.append date text
        content |> Util.FileSys.try_write_text_add(path) |> ignore
    } |> Async.Ignore |> Async.Start

let debugLog (file: string) (lines: string array) =
    lock file (fun _ ->
        if not (IO.File.Exists file) then
            IO.Directory.CreateDirectory(IO.Path.GetDirectoryName file)
            |> ignore
        use sw = new IO.StreamWriter(file, true)
        lines
        |> Array.insertAt 0 (DateTime.UtcNow.ToString("yyyyMMddHHmmss"))
        |> Array.insertAt 0 "["
        |> Array.iter(fun line -> sw.WriteLine line)
        sw.WriteLine "]"
    )

let debugLogLine (file: string) (line: string) =
    lock file (fun _ ->
        if not (IO.File.Exists file) then
            IO.Directory.CreateDirectory(IO.Path.GetDirectoryName file) |> ignore
            
        use sw = new IO.StreamWriter(file, true)
        sw.WriteLine $"{DateTime.UtcNow: ``u``} %s{line}"
    )

