module Util.FileSys

open System
open System.IO
open System.Collections.Generic
open System.Linq.Expressions
open System.Diagnostics
open System.Threading

open Util.Collection
open Util.CollectionModDict

open Util.Perf
open Util.Bin


let lockee = new Object()

let checkpath path = 
    if Directory.Exists path = false then
        Directory.CreateDirectory path |> ignore
    path

let check_file (filename:string) =
    try
        System.IO.File.OpenWrite(filename).Close()
        ""
    with
    | ex ->
        filename + " => " + ex.ToString()

let path__parent path = 
    let dir = new DirectoryInfo(path)
    dir.Parent.FullName

let shared_read_bin filename =
    if File.Exists filename then
        use ms = new MemoryStream()
            
        use f = File.Open(filename,FileMode.Open,FileAccess.Read,FileShare.ReadWrite)
        f.CopyTo ms
        ms.ToArray()
    else
        [||]

let try_read_bin filename =
    try
        "", shared_read_bin filename
    with
    | ex -> filename + " => " + ex.ToString(), [||]

let try_read_string filename =
    try
        "", File.ReadAllText filename
    with
    | ex -> filename + " => " + ex.ToString(), ""

let try_read_lines filename =
    try
        let lines = 
            shared_read_bin filename 
            |> System.Text.Encoding.UTF8.GetString
            |> (fun s -> s.Replace(Util.Text.crlf,Util.Text.lf))
            |> Util.Text.str__lines Util.Text.lf

        "", lines
    with
    | ex -> filename + " => " + ex.ToString(), [| |]

let filename__lines = try_read_lines >> snd

let try_write_lines filename lines =
    try
        File.WriteAllLines(filename,lines)
        ""
    with
    | ex -> filename + " => " + ex.ToString()

let try_write_text filename text =
    try
        File.WriteAllText(filename,text)
        ""
    with
    | ex -> filename + " => " + ex.ToString()

let try_write_bytes filename (bs:byte[]) =
    let info = new FileInfo(filename)
    try
        use stream = File.Open(filename, FileMode.OpenOrCreate,FileAccess.Write)
        use bw = new BinaryWriter(stream)
        bw.Write bs
        // File.WriteAllBytes(filename, bs)
        ""
    with
    | ex -> filename + " => " + ex.ToString()

let try_wipe_write_bytes filename (bs:byte[]) =
    let info = new FileInfo(filename)
    try 
        if File.Exists filename then
            File.Delete filename
    
        filename
        |> Path.GetDirectoryName
        |> checkpath
        |> ignore
            
        use fsr = new FileStream(filename, FileMode.OpenOrCreate, FileAccess.Write)
        fsr.Write(bs, 0, bs.Length) |> ignore
        fsr.Close()
             
        ""
    with
    | ex -> filename + " => " + ex.ToString()
        
let try_write_text_repeat (filename:string, text:string, n:int, interval:int) =

    let mutable success = false
    let mutable nn = n
    while nn > 0 do
        try
            File.WriteAllText(filename, text)
            success <- true
            nn <- 0
        with
        | _ ->
            nn <- nn - 1
            System.Threading.Thread.Sleep interval

    success

type FswExt = {
    mutable desc:string
    mutable lastop:DateTime
    key:string
    fsw:FileSystemWatcher }

let log (o:FswExt option) desc = 
    if o.IsSome then
        o.Value.desc <- desc
        o.Value.lastop <- DateTime.UtcNow

let fswexts = new Dictionary<string,FswExt>()

let empty__FswExt path fsw =
    let res = {
        desc = "Init"
        lastop = DateTime.UtcNow
        key = path
        fsw = fsw }
    fswexts.Add(path,res)
    res

let fsw_multiline (code:string) file = 
    shared_read_bin file 
    |> System.Text.Encoding.GetEncoding(code).GetString
    |> Util.Text.str__lines Util.Text.crlf

let create_fsw_textlines(path, filter, f:(string * FswExt option) -> string[] -> unit, encoding:string) =

    let fsw = new FileSystemWatcher()
    fsw.Path <- path
    fsw.IncludeSubdirectories <- false
    fsw.EnableRaisingEvents <- true
    fsw.NotifyFilter <- NotifyFilters.LastWrite
    fsw.Changed
        |> Event.filter filter
        |> Event.add (fun a ->
            fsw.EnableRaisingEvents <- false
                
            let o = 
                if fswexts.ContainsKey a.FullPath then
                    Some fswexts.[a.FullPath]
                else    
                    None

            try
                use cw = new CodeWrapper("Util/FileSys.fs/create_fsw_textlines()/Proceed")
                use cw = new CodeWrapper("Util/FileSys.fs/create_fsw_textlines()/Proceed="+a.FullPath)

                a.FullPath
                |> fsw_multiline(if encoding.Length = 0 then "UTF-8" else encoding)
                |> f(a.FullPath,o)
            with
            | ex -> 
                ex.ToString() 
                |> log o

            System.Threading.Thread.Sleep 100
            fsw.EnableRaisingEvents <- true)
    fsw

let create_fsw_textlines_exact(path, fullname, f:(string * FswExt option) -> string[] -> unit, encoding:string) =
    let filter(a:FileSystemEventArgs) =
        a.FullPath = fullname

    let fswext = 
        create_fsw_textlines(path, filter, f, encoding)
        |> empty__FswExt fullname

    try
        fullname
        |> fsw_multiline(if encoding.Length = 0 then "UTF-8" else encoding)
        |> f(fullname, Some fswext)
    with
    | ex -> ()

    fswext.fsw

let compare_write_time(f1, f2) =

    let fi1 = System.IO.FileInfo f1
    let fi2 = System.IO.FileInfo f2

    let t1 = if fi1.Exists then fi1.LastWriteTimeUtc else DateTime.MinValue
    let t2 = if fi2.Exists then fi2.LastWriteTimeUtc else DateTime.MinValue

    if t1 > t2 then
        1
    else if t1 < t2 then
        -1
    else
        0

let batchfile(path, filter:string->bool, handler:string * string * string -> unit) =
    Directory.GetFiles path
    |> Seq.iter(fun f ->
        if filter f then
            handler(path, f, f.Substring(path.Length + 1)))

let dir_parent s = (Directory.GetParent s).FullName

let rec pack bb (errors:List<string * string>) path = 
    if Directory.Exists path then

        Directory.GetFiles path
        |> Array.map(fun i -> 
            let finfo = new FileInfo(i)
            let msg,bin = try_read_bin i
            if msg.Length > 0 then
                errors.Add(i,msg)
            finfo.Name,bin)
        |> array__bin (fun (bb:BytesBuilder) item -> 
            item |> fst |> str__bin bb
            item |> snd |> bytes__bin bb) bb
            
        let folders = Directory.GetDirectories path

        folders.Length |> int32__bin bb

        folders
        |> Array.iter(fun folder ->
            (new DirectoryInfo(folder)).Name |> str__bin bb
            pack bb errors folder)

let path__bin path = 
    let bb = new BytesBuilder()
    pack bb (new List<string * string>()) path
    bb.bytes()

let rec unpack (errors:List<string * string>) bi folder = 

    if Directory.Exists folder = false then
        Directory.CreateDirectory folder |> ignore

    let fs = (fun bi -> bin__str bi, bin__bytes bi) |> bin__array <| bi

    fs
    |> Array.iter(fun (f,bin) -> 
        let filename = folder + "/" + f
        let msg = try_write_bytes filename bin
        if msg.Length > 0 then
            errors.Add(filename,msg))
            
    let length = bin__int32 bi

    [| 0 .. length - 1 |]
    |> Array.iter(fun i -> unpack errors bi (folder + "/" + (bin__str bi)))

let bin__path bin = unpack (new List<string * string>()) (bin,ref 0)
let bin__pathwitherr errs bin = unpack errs (bin,ref 0)

let try_write_text_add path (text:string[]) =
    try
            
        if System.IO.File.Exists path = false then
            let f = System.IO.File.Create path
                
            f.Dispose()
            f.Close()

        let file = new System.IO.StreamWriter(path,true, System.Text.Encoding.UTF8);
            
        text
        |> Array.map file.WriteLine
        |> ignore

        file.Dispose()
        file.Close()
                
        ""
    with
    | ex -> ex.ToString() 

