module UtilWebServer.FileSys

open System
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Net.Http
open System.Net.Http.Headers

open Util.CollectionModDict
open Util.Perf
open Util.Text
open Util.Db
open Util.DbQuery
open Util.DbTx
open Util.Orm
open Util.Cat
open Util.VideoDecorder


let checkDir f =
    if Directory.Exists f = false then
        Directory.CreateDirectory f |> ignore

    f

let check root folders =

    checkDir root |> ignore

    folders
    |> Array.iter (fun i ->
        let s = System.IO.Path.Join(root, i)
        checkDir s |> ignore)

let modulo__f (root: string) folder modulo (id: int64) =
    let residual = id % modulo

    let s = System.IO.Path.Combine([| root; folder; residual.ToString() |])
    let f = System.IO.Path.Combine([| s; id.ToString() |])
    residual, s, f

let hash__f (root: string) folder (hash: string) =
    let residual = hash.Substring(hash.Length - 2, 2)

    let s =
        System.IO.Path
            .Join(root, folder.ToString(), residual.ToString())
            .Replace("\\", "/")
            .Replace("//", "/")

    let f = s + hash
    residual, s, f

// ██████████████████████████████████████████████████████████████████████████████

let occupied = new ConcurrentDictionary<string, bool>()

let lockee = new Object()

let tryFileLocked (fullname: string) remove h =
    use cw = new CodeWrapper("*SrOrm.FileSys.tryFileLocked")

    let tryLock key =
        use cw = new CodeWrapper("SrOrm.FileSys.tryLock")

        let mutable locked = true
        occupied.AddOrUpdate(key, true, fun k v ->
                                            if v then
                                                locked <- false
                                            true) |> ignore
        locked

    let unLock (occupied:ConcurrentDictionary<string, bool>) (key:string) remove =
        if remove then
            occupied.TryRemove key |> ignore
        else
            occupied[key] <- false

    let key = fullname.Trim().Replace("\\", "/").ToLower() |> Util.Crypto.str__sha256

    SpinWait.SpinUntil(fun _ -> tryLock key)

    let res = h fullname

    unLock occupied key remove

    res

//let trySaveBinLocked fullname (bin: byte[]) =
//    use cw = new CodeWrapper("SrOrm.FileSys.trySaveBinLocked")

//    tryFileLocked fullname false (fun fullname ->
//        try
//            if File.Exists fullname then
//                let df name =
//                    use cw =
//                        new CodeWrapper("SrOrm.FileSys.trySaveBinLocked.tryFileLocked.h.deleteFile")

//                    File.Delete name

//                df fullname

//            fullname |> Path.GetDirectoryName |> checkDir |> ignore

//            use stream = File.Open(fullname, FileMode.OpenOrCreate, FileAccess.Write)
//            use bw = new BinaryWriter(stream)
//            bw.Write bin
//            bw.Close()
//            stream.Close()
//            // File.WriteAllBytes(fullname, bin)
//            true
//        with ex ->
//            [| "Fullname = " + fullname
//               ", Bin = " + bin.Length.ToString()
//               ", Ex = " + ex.ToString() |]
//            |> linesConcat
//            |> exn__logNoSql "SrOrm.FileSys.trySaveBin"

//            false)

//let tryLoadBinLocked fullname =
//    tryFileLocked fullname false (fun fullname ->
//        try
//            if IO.File.Exists fullname then
//                use stream = File.Open(fullname, FileMode.Open, FileAccess.Read)
//                use br = new BinaryReader(stream)
//                let bin = br.ReadBytes(stream.Length |> int)
//                br.Close()
//                stream.Close()
//                Some bin
//            // Some(File.ReadAllBytes fullname)
//            else
//                None
//        with ex ->
//            [| "Fullname = " + fullname; ", Ex = " + ex.ToString() |]
//            |> linesConcat
//            |> exn__logNoSql "SrOrm.FileSys.tryLoadBinLocked"

//            None)

//let trySaveTextLocked fullname text =
//    tryFileLocked fullname false (fun fullname ->

//        try
//            File.WriteAllText(fullname, text)
//            true
//        with ex ->
//            [| "Fullname = " + fullname
//               ", Text = " + text.Length.ToString()
//               ", Ex = " + ex.ToString() |]
//            |> linesConcat
//            |> exn__logNoSql "SrOrm.FileSys.trySaveBin"

//            false)

//let tryDeleteFile fullname =
//    try
//        File.Delete(fullname)
//        true
//    with ex ->
//        [| "Fullname = " + fullname; ", Text = Delete" + ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryDeleteBin"

//        false

//let tryLoadTextLocked fullname =
//    tryFileLocked fullname false (fun fullname ->
//        try
//            if IO.File.Exists fullname then
//                Some(File.ReadAllText fullname)
//            else
//                None
//        with ex ->
//            [| "Fullname = " + fullname; ", Ex = " + ex.ToString() |]
//            |> linesConcat
//            |> exn__logNoSql "SrOrm.FileSys.tryLoadBinLocked"

//            None)

//let tryDeleteLocked fullname =
//    tryFileLocked fullname true (fun fullname ->
//        try
//            File.Delete fullname
//            true
//        with ex ->
//            [| "Fullname = " + fullname; ", Ex = " + ex.ToString() |]
//            |> linesConcat
//            |> exn__logNoSql "SrOrm.FileSys.tryDeleteLocked"

//            false)

//// ██████████████████████████████████████████████████████████████████████████████

//let tryModuloLoadTxt root folder modulo (id: int64) =

//    use cw = new CodeWrapper("SrOrm.FileSys.tryModuloLoadTxt")

//    let residual, s, f = modulo__f root folder modulo id

//    try
//        checkDir s |> ignore
//        tryLoadTextLocked f
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Modulo = " + modulo.ToString()
//           ", ID = " + id.ToString()
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryModuloLoadTxt"

//        None

//let tryModuloSaveTxt root folder modulo (id: int64) txt =
//    use cw = new CodeWrapper("SrOrm.FileSys.tryModuloSaveTxt")

//    let residual, s, f = modulo__f root folder modulo id

//    try
//        checkDir s |> ignore
//        trySaveTextLocked f txt
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Modulo = " + modulo.ToString()
//           ", ID = " + id.ToString()
//           ", Txt = " + txt
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryModuloSaveTxt"

//        false

//let tryModuloSaveBin root folder modulo (id: int64) bin =

//    use cw = new CodeWrapper("SrOrm.FileSys.tryModuloSaveBin")

//    let residual, s, f = modulo__f root folder modulo id

//    try
//        checkDir s |> ignore
//        trySaveBinLocked f bin
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Modulo = " + modulo.ToString()
//           ", ID = " + id.ToString()
//           ", Bin = " + bin.Length.ToString()
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryModuloSaveTxt"

//        false

//let tryModuloLoadBin root folder modulo (id: int64) =

//    use cw = new CodeWrapper("SrOrm.FileSys.tryModuloLoadBin")

//    let residual, s, f = modulo__f root folder modulo id

//    try
//        checkDir s |> ignore
//        tryLoadBinLocked f
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Modulo = " + modulo.ToString()
//           ", ID = " + id.ToString()
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryHashLoadBin"

//        None

//let tryHashSaveTxt root (folder: FolderNamed) (hash: string) txt =

//    use cw = new CodeWrapper("SrOrm.FileSys.tryHashSaveTxt")

//    let residual, s, f = hash__f root folder hash

//    try
//        checkDir s |> ignore
//        trySaveTextLocked f txt
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Residual = " + residual
//           ", Hash = " + hash
//           ", Txt = " + txt
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryHashSaveTxt"

//        false

//let tryHashLoadTxt root (folder: FolderNamed) (hash: string) =

//    use cw = new CodeWrapper("SrOrm.FileSys.tryHashLoadTxt")

//    let residual, s, f = hash__f root folder hash

//    try
//        checkDir s |> ignore
//        tryLoadTextLocked f
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Residual = " + residual
//           ", Hash = " + hash
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryHashLoadTxt"

//        None

//let tryHashSaveBin root (folder: FolderNamed) (hash: string) bin =

//    use cw = new CodeWrapper("SrOrm.FileSys.tryHashSaveBin")

//    let residual, s, f = hash__f root folder hash

//    try
//        checkDir s |> ignore
//        trySaveBinLocked f bin
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Residual = " + residual
//           ", Hash = " + hash
//           ", Bin = " + bin.Length.ToString()
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryHashSaveBin"

//        false

//let tryHashLoadBin root (folder: FolderNamed) (hash: string) =

//    use cw = new CodeWrapper("SrOrm.FileSys.tryHashLoadBin")

//    let residual, s, f = hash__f root folder hash

//    try
//        checkDir s |> ignore
//        tryLoadBinLocked f
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Residual = " + residual
//           ", Hash = " + hash
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryHashLoadBin"

//        None

//let tryHashDelete root (folder: FolderNamed) (hash: string) =

//    use cw = new CodeWrapper("SrOrm.FileSys.tryHashLoadBin")

//    let residual, s, f = hash__f root folder hash

//    try
//        if Directory.Exists s then
//            if File.Exists f then tryDeleteLocked f else true
//        else
//            true
//    with ex ->
//        [| "Folder = " + folder.ToString()
//           ", Residual = " + residual
//           ", Hash = " + hash
//           ", Ex = " + ex.ToString() |]
//        |> linesConcat
//        |> exn__logNoSql "SrOrm.FileSys.tryHashDelete"

//        false

//// ██████████████████████████████████████████████████████████████████████████████

//let modulo = 1024L
//let mutable mmFileMap =
//    new SortedDictionary<int64, MemoryMappedFiles.MemoryMappedFile>()

//let mutable backendFileQueue =
//    new ConcurrentQueue<int64>()

//type IPFSResponse =
//    { Name: string
//      Hash: string
//      Size: string }

//let ipfsByteDataUpload (ipfsURL: string) (fileName: string) (byteData: byte[]) : string =
//    let httpClient = new HttpClient()
//    let content = new MultipartFormDataContent()

//    let byteArrayContent = new ByteArrayContent(byteData)
//    byteArrayContent.Headers.ContentType <- MediaTypeHeaderValue.Parse("application/octet-stream")
//    content.Add(byteArrayContent, "file", fileName)

//    async {
//        let! response = httpClient.PostAsync(ipfsURL, content) |> Async.AwaitTask
//        return response
//    }
//    |> Async.RunSynchronously
//    |> (fun r ->
//        if r.StatusCode = System.Net.HttpStatusCode.OK then
//            use reader = new StreamReader(r.Content.ReadAsStream())

//            let content = reader.ReadToEnd()            
//            let iHash = content.IndexOf("\"Hash\":\"")
//            let iEnd = content.IndexOf("\",\"", iHash)
//            if iEnd - iHash = 54 then
//                content.Substring(iHash + 8, iEnd - iHash - 8)
//            else
//                ""            
//        else
//            "")


//let getFileFullByFolderType root (modv: int64) (euId: int64) (tid: int64) (foldertype: FolderNamed) =
//    let usr = FolderNamed.Usr.ToString()
//    let chat = FolderNamed.Chat.ToString()
//    let mods = modv.ToString()
//    let euIds = euId.ToString()
//    let tids = tid.ToString()
//    let chatFolder = [| root; usr; mods; euIds; chat |] |> System.IO.Path.Combine

//    if not (IO.Directory.Exists chatFolder) then
//        chatFolder |> IO.Directory.CreateDirectory |> ignore

//    let filefull =
//        match foldertype with
//        | FolderNamed.Usr -> Ok([| root; usr; mods; euIds; tids |] |> System.IO.Path.Combine)
//        | FolderNamed.Chat -> Ok([| root; usr; mods; euIds; chat; tids |] |> System.IO.Path.Combine)
//        | _ -> Error "Error:FolderNamed Enum Error"

//    filefull

//let (++) lx ly = Array.append lx ly

//let logErrNoSql name msg errcode =
//    msg |> linesConcat |> exn__logNoSql name
//    errcode

//let eq_bin_sha256 exnMsg sha256 bin =
//    match Util.Crypto.bin__sha256 bin = sha256 with
//    | true -> Ok bin
//    | false ->
//        Error(
//            logErrNoSql
//                "FileSys.createChatEuFile"
//                (exnMsg
//                 ++ [| ", Length = " + bin.Length.ToString(); ", Exception = InvalideHash" |])
//                Error.InvalideHash
//        )

//let suc_trySaveBinLocked exnMsg filefull bin =
//    match trySaveBinLocked filefull bin with
//    | true -> Ok true
//    | false ->
//        Error(
//            logErrNoSql
//                "FileSys.createChatEuFile"
//                (exnMsg ++ [| ", Exception = createChatEuFile.trySaveBinLocked" |])
//                Error.Internal
//        )

//type mediaInfo = {
//    shortname: string
//    longname : string
//    destname : string
//}

//let validateMediaExtension (m:mediaInfo) =
//    match m.shortname.ToString() |> System.IO.Path.GetExtension with
//    | ".mp4"
//    | ".wmv"
//    | ".mov" // mobile
//    | ".mkv"
//    | ".mpeg"
//    | ".webm" 
//    | ".m4v" 
//    | ".avi" -> Ok m
//    | _      -> Error(Error.MediaNotValidateExtension)

//let validateMediaInternalFormat exnMsg (m:mediaInfo) =
//    match isMediaReady m.longname with
//    | Ok _ -> Ok m
//    | Error v ->
//        Error(
//                logErrNoSql
//                    "FileSys.validateMedia"
//                    (exnMsg ++ [| v |])
//                    Error.MediaInvalidateFormat
//        )

//let validateMedia exnMsg  (m:mediaInfo) =
//    m |> validateMediaExtension 
//      >>= validateMediaInternalFormat exnMsg

//let readNewMediaBin exnMsg (m:mediaInfo) =
//    try
//        let stream = File.Open(m.destname, FileMode.Open, FileAccess.Read)
//        let mem = new MemoryStream()
//        stream.CopyTo mem
//        let data = mem.ToArray()
//        stream.Close()
//        Ok data
//    with 
//        | ex -> 
//            Error (
//                    logErrNoSql
//                        "FileSys.readNewMediaBin"
//                        (exnMsg ++ [| ex.ToString() |])
//                        Error.MediaReadError
//            )

//let mediaEncodeProcess exnMsg (m:mediaInfo) =
//    match handleStream m.longname m.destname with
//    | Ok _ -> Ok m
//    | Error v -> 
//        Error(
//                logErrNoSql
//                    "FileSys.getMeidaBin"
//                    (exnMsg ++ [| v |])
//                    Error.MediaReEncodInternalError
//        )
//let doMediaConvert exnMsg (m:mediaInfo) =
//    m |> validateMediaExtension 
//      >>= validateMediaInternalFormat exnMsg
//      >>= mediaEncodeProcess exnMsg
//      >>= readNewMediaBin exnMsg

//let canStartMediaAsync exnMsg shortname longname = 
//    let m = {
//        shortname = shortname
//        longname  = longname
//        destname  = longname + ".mp4"
//    }
//    match validateMedia exnMsg m with
//    | Ok v -> true
//    | Error _ -> false

//let getMediaBin exnMsg shortname longname=
//    let m = {
//        shortname = shortname
//        longname  = longname
//        destname  = longname + ".mp4"
//    }
//    match doMediaConvert exnMsg m with
//    | Ok bin -> Some bin
//    | Error _ -> None

//let mediaBin mediaFilename =
//    let stream = File.Open(mediaFilename, FileMode.Open, FileAccess.Read)
//    let mem = new MemoryStream()
//    stream.CopyTo mem
//    let data = mem.ToArray()
//    stream.Close()
//    data

//let deleteFile(f:FILE, files: ModDict<int64, FILE>) =
//    match txOne conn output ($"update [{FILE_table}] set State = 5 where ID = {f.ID.ToString()}") with
//    | Suc ctx ->
//        f.p.State <- fileStateEnum.Deleted
//        files.remove f.ID
//        Error.OK
//    | Fail (dte, ctx) ->
//        Error.Internal

//// backend file task, transcode and ipfs upload
//let backendFileTask (host: Host, files: ModDict<int64, FILE>) =
//        async {
//            let tasks = List<Task>()
//            while true do
//                tasks.RemoveAll(fun t -> t.IsCompleted) |> ignore
//                if backendFileQueue.IsEmpty || tasks.Count >= host.MaxBackendFileTask then
//                    Thread.Sleep(3000)
//                else
//                    Task.Run( fun _ ->
//                        let fileid = ref 0L
//                        if backendFileQueue.TryDequeue(fileid) then
//                            let fo = fileid.Value |> id__FILEo
//                            if fo.IsSome then
//                                let userFolder =
//                                    System.IO.Path.Combine(
//                                        [| host.fsRoot
//                                           FolderNamed.Usr.ToString()
//                                           (fo.Value.p.Bind % modulo).ToString()
//                                           fo.Value.p.Bind.ToString()|])
//                                let mutable rc = false
//                                let oldState = fo.Value.p.State
//                                let mutable f = System.IO.Path.Combine(userFolder, fileid.Value.ToString())
//                                if not (File.Exists f) then
//                                    f <- System.IO.Path.Combine(userFolder, FolderNamed.Chat.ToString(), fileid.Value.ToString())
//                                if File.Exists f then
//                                    if fo.Value.p.SHA256 <> "" then
//                                        match oldState with
//                                        | fileStateEnum.PendingTranscode ->
//                                            let mutable exnMsg = [| "FileName = " + fo.Value.p.Caption|]
//                                            let m = {
//                                                shortname = fo.Value.p.Caption
//                                                longname  = f
//                                                destname  = f + ".mp4"
//                                            }
//                                            match validateMedia exnMsg m with
//                                            | Ok v -> 
//                                                let mbin =
//                                                    use cw = new CodeWrapper("SrOrm.FileSys.backendFileTask.get_MediaBin")
//                                                    getMediaBin exnMsg fo.Value.p.Caption f
//                                                if mbin.IsSome then
//                                                    fo.Value.p.State <- fileStateEnum.PendingBlockchain
//                                                    fo.Value.p.Caption <-
//                                                        (fo.Value.p.Caption |> System.IO.Path.GetFileNameWithoutExtension) + ".mp4"
//                                                    fo.Value.p.Size <- mbin.Value.Length
//                                                    rc <- true                                                
//                                            | Error _ -> 
//                                                fo.Value.p.State <- fileStateEnum.PendingBlockchain
//                                                rc <- true
//                                        | fileStateEnum.PendingBlockchain ->
//                                            use cw = new CodeWrapper("SrOrm.FileSys.backendFileTask.ipfsByteDataUpload")
//                                            if fo.Value.p.Caption.EndsWith(".mp4") then
//                                                f <- f + ".mp4"
//                                            let hash = ipfsByteDataUpload host.ipfsGateway fo.Value.p.Caption (File.ReadAllBytes(f))
//                                            if hash.Length = 46 then
//                                                fo.Value.p.SHA256 <- hash
//                                                fo.Value.p.State <- fileStateEnum.Normal
//                                                rc <- true                                            
//                                        | fileStateEnum.Uploading ->
//                                            fo.Value.p.State <- fileStateEnum.Deleted
//                                            rc <- true
//                                        | _ -> ()
//                                        if rc then
//                                            // update database
//                                            let pretx = None |> opctx__pretx
//                                            (fun (a, b) -> fo.Value.p.State <- oldState) |> pretx.fails.Add

//                                            (fileid.Value, fo.Value.Updatedat, fo.Value.p)
//                                            |> build_update_sql FILE_metadata
//                                            |> pretx.sqls.Add

//                                            let rcd = ((fileid.Value, pretx.dt, pretx.dt, fileid.Value), fo.Value.p) |> FILE_wrapper

//                                            (fun _ -> files[rcd.ID] <- rcd) |> pretx.sucs.Add
//                                            let op =
//                                                match oldState with
//                                                | fileStateEnum.Uploading -> "clean_remained_file"
//                                                | fileStateEnum.PendingTranscode -> "get_MediaBin"
//                                                | fileStateEnum.PendingBlockchain -> "ipfsByteDataUpload"
//                                                | _ -> ""
//                                            match pretx |> logged_pipeline ("FileSys.backendFileTask." + op) with
//                                            | Error.OK ->
//                                                if fo.Value.p.State = fileStateEnum.PendingBlockchain then
//                                                    backendFileQueue.Enqueue(fileid.Value)
//                                            | _ -> exn__logNoSql "FileSys.backendFileTask.updateDatabse" ("FileId=" + fo.Value.ID.ToString())
//                                    else
//                                        deleteFile(fo.Value, files) |> ignore
//                                else
//                                    deleteFile(fo.Value, files) |> ignore) |> tasks.Add
//        }
//        |> Async.Start

//let quotaMap = new Dictionary<int64, uint64*uint64>()
//let getOrAddQuotaItem(files: ModDict<int64, FILE>, isGet: bool, host: Host, userid: int64, quota: uint64) =
//    use cw = new CodeWrapper("ApiEndUser.getOrAddQuotaItem")
//    lock quotaMap (fun _ ->
//        let mutable (quotaFull, occupied) = 
//            match quotaMap.ContainsKey userid with
//            | true -> quotaMap[userid]
//            | false -> 0UL, 0UL
//        if isGet then
//            if quotaFull > 0UL then
//                match quotaFull - occupied with
//                | remain when remain > 0UL -> quotaFull, remain
//                | _ -> quotaFull, 0UL
//            else
//                occupied <-
//                    files.filter(fun i -> i.p.BindType = fileBindTypeEnum.EndUser && i.p.Bind = userid) 
//                    |> Array.sumBy(fun i -> i.p.Size) 
//                    |> uint64
//                let rcds =
//                    match 
//                        [|"SELECT TOP 1 " + RESOURCE_fieldorders;
//                        " FROM [" + RESOURCE_table + "]";
//                        " WHERE State=0" + " and Owner=" + userid.ToString()|]
//                        |> linesConcat
//                        |> str__sql
//                        |> multiline_query( conn ) 
//                    with
//                        | Suc(ctx) -> 
//                            ctx.lines.ToArray() |> Array.map(fun line -> line |> db__RESOURCE)
//                        | Fail(exn,ctx) -> 
//                            (exn,ctx) |> dbquery_exn__log("ApiEndUser.getOrAddQuotaItem") |> ignore
//                            [||]
//                quotaFull <- 
//                    match rcds.Length = 0 with
//                    | true ->
//                        host.QuotaForUnpaidUser                    
//                    | false -> 
//                        match rcds[0].p.QuotaUnit with
//                        | resourceQuotaUnitEnum.M -> uint64(rcds[0].p.Quota) * (1024UL * 1024UL)
//                        | resourceQuotaUnitEnum.G -> uint64(rcds[0].p.Quota) * (1024UL * 1024UL * 1024UL)
//                        | _ -> 0UL
//                quotaMap[userid] <- quotaFull, occupied
//                match quotaFull - occupied with
//                | remain when remain > 0UL -> quotaFull, remain
//                | _ -> quotaFull, 0UL
//        else
//            occupied <- occupied + quota
//            quotaMap[userid] <- quotaFull, occupied
//            match quotaFull - occupied with
//            | remain when remain > 0UL -> quotaFull, remain
//            | _ -> quotaFull, 0UL
//    )

//let resetQuotaFull(host: Host, userid: int64, quota: uint64) =
//    use cw = new CodeWrapper("ApiEndUser.resetQuotaFull")
//    lock quotaMap (fun _ ->         
//        if quotaMap.ContainsKey userid then
//            let occupied = snd(quotaMap[userid])
//            let quotaFullNew =
//                match quota = 0UL with
//                | true -> host.QuotaForUnpaidUser
//                | false -> quota
//            quotaMap[userid] <- quotaFullNew, occupied
//        )

//let getFileMetaData (f:string) =
//    let sb = System.Text.StringBuilder()
//    sb.Append("{") |> ignore
//    //ImageMetadataReader.ReadMetadata f
//    if f.ToLower().EndsWith(".mp3") then
//        new FileStream(f, FileMode.OpenOrCreate, FileAccess.Read)
//        |> Mp3MetadataReader.ReadMetadata
//    else
//        ImageMetadataReader.ReadMetadata f
//    |> Seq.iter(
//        fun d ->
//            d.Tags
//            |> Seq.iter(
//                fun t ->
//                    $"\"{d.Name} - {t.Name}\": \"{t.Description}\","
//                    |> sb.Append
//                    |> ignore
//            )
//    )
//    if (sb.Chars(sb.Length - 1) = ',') then
//        sb.Remove(sb.Length - 1, 1) |> ignore
//    sb.Append("}") |> ignore
//    sb.ToString()

//let writeFile
//    (host: Host)
//    (files: ModDict<int64, FILE>)
//    ec
//    (folderID, filename, fid, filesize, bin: byte array, sha256, offset, isLastChunk)
//    folderType
//    =
//    use cw = new CodeWrapper("SrOrm.FileSys.writeFile")
//    let (fo: FILE option), (po: pFILE option), fileid =
//        if fid = 0L then
//            let p = pFILE_empty ()
//            p.BindType <- fileBindTypeEnum.EndUser
//            p.Bind <- ec.eu.ID
//            p.Folder <- folderID
//            p.Caption <- filename
//            p.SHA256 <- sha256
//            p.Size <- filesize
//            p.JSON <- "{}"
//            None, Some p, System.Threading.Interlocked.Increment FILE_id
//        else
//            let fo = fid |> id__FILEo
//            match fo with
//            | Some file when (file.p.BindType = fileBindTypeEnum.EndUser && file.p.Bind = ec.eu.ID) ->
//                fo, Some file.p, file.ID
//            | _ -> None, None, 0
//    if fileid = 0 then
//        Error.Internal, "{\"Error\":\"Record missed\"}"
//    else
//        let userFolder =
//            System.IO.Path.Combine(
//                [| host.fsRoot
//                   FolderNamed.Usr.ToString()
//                   (ec.eu.ID % modulo).ToString()
//                   ec.eu.ID.ToString() |])
//        // 每次都要创建目录,如果有 userFolder 则不创建
//        // 尤其是当第一次上传文件时,并且没有创建目录时,防止出错
//        let di = System.IO.Directory.CreateDirectory(userFolder)
//        let chatFolder = System.IO.Path.Combine(userFolder, FolderNamed.Chat.ToString())
//        let diChat = System.IO.Directory.CreateDirectory(chatFolder)

//        let f =
//            match folderType with
//            | FolderNamed.Chat -> System.IO.Path.Combine(chatFolder, fileid.ToString())
//            | FolderNamed.Usr
//            | _ -> System.IO.Path.Combine(userFolder, fileid.ToString())

//        let mutable exnMsg =
//            [|
//                "FileName = " + filename
//                ", SHA256 = " + sha256
//                ", File = " + f
//                ", Modulo = " + modulo.ToString()
//                ", FolderID = " + folderID.ToString()
//             |]
//        let isSingleFile = offset = 0 && isLastChunk
//        let isChunkStart = fid = 0 && bin.Length = 0
//        let m = {
//            shortname = po.Value.Caption
//            longname  = f
//            destname  = f + ".mp4"
//        }
//        let rc, error =
//            if isSingleFile then
//                if Util.Crypto.bin__sha256 bin = sha256 then
//                    exnMsg <- Array.append exnMsg [| ", Length = " + bin.Length.ToString() |]
//                    if trySaveBinLocked f bin then
//                        match validateMedia exnMsg m with
//                        | Ok v -> po.Value.State <- fileStateEnum.PendingTranscode
//                        | Error _ -> po.Value.State <- fileStateEnum.PendingBlockchain
//                        getOrAddQuotaItem(files, false, host, ec.eu.ID, uint64(filesize)) |> ignore
//                        Error.OK, "{\"Error\":\"OK\", \"FileID\":" + fileid.ToString() + "}"
//                    else
//                        (Array.append exnMsg [| ", Exception = writeFile.trySaveBinLocked" |])
//                        |> linesConcat
//                        |> exn__logNoSql "FileSys.writeFile"
//                        Error.Internal, "{\"Error\":\"Failed to save file\"}"
//                else
//                    (Array.append exnMsg [| ", Length = " + bin.Length.ToString(); ", Exception = InvalideHash" |])
//                    |> linesConcat
//                    |> exn__logNoSql "FileSys.writeFile"
//                    Error.InvalideHash, "{\"Error\":\"Hash invalid\"}"
//            else
//                if isChunkStart then
//                    if IO.File.Exists f then
//                        IO.File.Delete(f)
//                    IO.File.Create(f).Close()
//                    po.Value.State <- fileStateEnum.Uploading
//                    getOrAddQuotaItem(files, false, host, ec.eu.ID, uint64(filesize)) |> ignore
//                    Error.OK, "{\"Error\":\"OK\", \"FileID\":" + fileid.ToString() + "}"
//                else
//                    exnMsg <-
//                        Array.append exnMsg
//                            [| ", ID = " + fileid.ToString()
//                               ", Length = " + bin.Length.ToString()
//                               ", Offset = " + offset.ToString()
//                               ", isLastChunk = " + isLastChunk.ToString()
//                            |]
//                    if File.Exists f then
//                        try
//                            let tryGetMMFile(fileid, path, filesize) =
//                                use cw = new CodeWrapper("SrOrm.FileSys.uploadFileChunk.tryGetMMFile")

//                                lock mmFileMap (fun _ ->
//                                    if not (mmFileMap.ContainsKey(fileid)) then
//                                        if mmFileMap.Count >= host.MaxCachedMMFileObj then
//                                            let mutable er = mmFileMap.GetEnumerator()
//                                            er.MoveNext() |> ignore
//                                            er.Current.Value.Dispose()
//                                            mmFileMap.Remove(er.Current.Key) |> ignore
//                                        let mf = MemoryMappedFiles.MemoryMappedFile.CreateFromFile(path, FileMode.Create, null, filesize)
//                                        mmFileMap[fileid] <- mf
//                                        mf
//                                    else
//                                        mmFileMap[fileid])
                            
//                            let vs = tryGetMMFile(fileid, f, po.Value.Size).CreateViewStream(offset, bin.Length)
//                            let bw = new BinaryWriter(vs)
//                            bw.Write(bin)
//                            bw.Close()
//                            vs.Close()
//                            if isLastChunk then
//                                if mmFileMap.ContainsKey fileid then
//                                    mmFileMap[fileid].Dispose()
//                                    mmFileMap.Remove(fileid) |> ignore
                                
//                                match validateMedia exnMsg m with
//                                | Ok v -> po.Value.State <- fileStateEnum.PendingTranscode
//                                | Error _ -> po.Value.State <- fileStateEnum.PendingBlockchain
                            
//                                // update database
//                                let pretx = None |> opctx__pretx
//                                (fun (a, b) -> po.Value.State <- fileStateEnum.Uploading) |> pretx.fails.Add
                            
//                                (fileid, fo.Value.Updatedat, po.Value)
//                                |> build_update_sql FILE_metadata
//                                |> pretx.sqls.Add
                            
//                                let rcd = ((fileid, pretx.dt, pretx.dt, fileid), po.Value) |> FILE_wrapper
//                                (fun _ -> files.[rcd.ID] <- rcd) |> pretx.sucs.Add
//                                match pretx |> logged_pipeline "FileSys.writeFile" with
//                                | Error.OK -> 
//                                    backendFileQueue.Enqueue fileid
//                                    Error.OK, "{\"Error\":\"OK\", \"FileID\":" + fileid.ToString() + "}"
//                                | _ -> 
//                                    exn__logNoSql "FileSys.writeFile.updateDatabase" ("FileId=" + fileid.ToString())
//                                    Error.Internal, "{\"Error\":\"Internal\", \"FileID\":" + fileid.ToString() + "}"
//                            else
//                                Error.OK, "{\"Error\":\"OK\", \"FileID\":" + fileid.ToString() + "}"
//                        with ex ->
//                            Array.append exnMsg [| ", Exception = " + ex.ToString() |]
//                                |> linesConcat
//                                |> exn__logNoSql "FileSys.writeFile"
//                            Error.Internal, sprintf "{\"Error\":\"[%s] \"}" (ex.ToString())
//                    else
//                        Array.append exnMsg [| ", Error = File missed" |]
//                        |> linesConcat
//                        |> exn__logNoSql "FileSys.writeFile"
//                        Error.Internal, "{\"Error\":\"File missed\"}"

//        if rc = Error.OK && (isSingleFile || isChunkStart) then
//            if isLastChunk then
//                po.Value.JSON <- getFileMetaData f
//            let pretx = None |> opctx__pretx
//            (fileid, pretx.dt, pretx.dt, fileid, po.Value)
//            |> build_create_sql FILE_metadata
//            |> pretx.sqls.Add

//            let rcd = ((fileid, pretx.dt, pretx.dt, fileid), po.Value) |> FILE_wrapper
//            (fun _ -> files.[rcd.ID] <- rcd) |> pretx.sucs.Add
//            if isSingleFile then
//                backendFileQueue.Enqueue fileid
//            //pretx |> logged_pipeline "FileSys.writeFile", error
//            match pretx |> logged_pipeline "FileSys.writeFile" with
//            | Error.OK -> Error.OK, error
//            | e -> 
//                exn__logNoSql "FileSys.writeFile.create" ("FileId=" + fileid.ToString())
//                e, error
//        else
//            rc, error

//let findAllImageLinkInContent (content: string) : (string * string)[] =
//    let pattern = @"https://i.imgur.com/\S+(.jpg|.svg|.png|.gif)"

//    let matches = Regex.Matches(content, pattern)

//    matches
//    |> Seq.cast
//    |> Seq.toArray
//    |> Array.map (fun (m: Match) -> m.Value.Replace("https://i.imgur.com/", ""), m.Value)

//let downloadImageAndUploadToIPFS (x:X) (endUserID: int64) ((fileName: string), (imageLink: string)) : string =
//    let pretx = None |> opctx__pretx
//    let tid = System.Threading.Interlocked.Increment FILE_id
//    let httpClient = new HttpClient()

//    async {
//        let! imageContent = httpClient.GetByteArrayAsync(imageLink) |> Async.AwaitTask

//        let byteArrayContent = new ByteArrayContent(imageContent)
//        return byteArrayContent.ReadAsByteArrayAsync().Result
//    }
//    |> Async.RunSynchronously
//    |> (ipfsByteDataUpload x.runtime.host.ipfsGateway fileName)
//    |> (fun hash ->

//        let p = pFILE_empty ()
//        p.BindType <- fileBindTypeEnum.EndUser
//        p.Bind <- endUserID
//        p.Folder <- 0L
//        p.Caption <- fileName
//        p.SHA256 <- hash

//        (tid, pretx.dt, pretx.dt, tid, p)
//        |> build_create_sql FILE_metadata
//        |> pretx.sqls.Add

//        let rcd = ((tid, pretx.dt, pretx.dt, tid), p) |> FILE_wrapper

//        (fun _ -> x.runtime.files.[rcd.ID] <- rcd) |> pretx.sucs.Add

//        let err = tid, pretx |> logged_pipeline ("FileSys.downloadImageAndUploadToIPFS") 

//        hash)

//let rec directorySize (dirPath: string) : int64 =
//    try
//        let sizeFilePath = System.IO.Path.Combine(dirPath, "download_size.txt")
//        let content = File.ReadAllText(sizeFilePath)
//        content |> Int64.Parse
//    with ex ->
//        0L

//let getExpiredFiles (x:X) =
//    let oneWeekAgo = DateTime.UtcNow.AddDays(-7.0)
//    x.runtime.files.filter (fun file -> file.Createdat < oneWeekAgo)

//type IPFSSummary =
//    { TotalFilesCount: int64
//      ExpiredCount: int64
//      TotalSize: int64
//      DownloadedSize: int64 }

//let ipfsSummary (x:X) : IPFSSummary =
//    use cw = new CodeWrapper("FileSys.ipfsSummary")

//    let expiredFiles = x |> getExpiredFiles
//    let totalSize = x.runtime.files.array () |> Array.sumBy (fun f -> f.p.Size)
//    let downloadedSize = x.runtime.host.ipfsBackupFolderPath |> directorySize

//    { TotalFilesCount = x.runtime.files.count
//      ExpiredCount = expiredFiles.Length
//      TotalSize = totalSize
//      DownloadedSize = downloadedSize }

//let ipfsRenewFile (x:X) hash dataSize =

//    let url = x.runtime.host.ipfsPrefix + "/" + hash

//    let httpClient = new HttpClient()
//    let response = httpClient.GetAsync(url).Result

//    if response.IsSuccessStatusCode then
//        let content = response.Content

//        let contentLength = content.Headers.ContentLength

//        if contentLength.HasValue then
//            contentLength.Value = dataSize
//        else
//            false
//    else
//        false

//let tryAppendBinLocked fullname (bin: byte[]) =
//    use cw = new CodeWrapper("SrOrm.FileSys.tryAppendBinLocked")

//    tryFileLocked fullname false (fun fullname ->
//        try
//            use stream = 
//                if File.Exists fullname = false then
//                    File.Open(fullname, FileMode.OpenOrCreate, FileAccess.Write)
//                else 
//                    new FileStream(fullname, FileMode.Append, FileAccess.Write)
//            use bw = new BinaryWriter(stream)
//            bw.Write bin
//            bw.Close()
//            stream.Close()
//            // File.WriteAllBytes(fullname, bin)
//            true
//        with ex ->
//            [| "Fullname = " + fullname
//               ", Bin = " + bin.Length.ToString()
//               ", Ex = " + ex.ToString() |]
//            |> linesConcat
//            |> exn__logNoSql "SrOrm.FileSys.trySaveBin"

//            false
//    )


