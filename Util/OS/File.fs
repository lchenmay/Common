module Util.File

open System
open System.IO
open System.Text

/// 文件类别
type FileCategory =
    | TextCode      // 文本/代码文件
    | Document      // 文档文件（PDF, DOCX, XLSX 等）
    | Image         // 图片文件
    | Binary        // 二进制文件
    | Directory     // 目录

/// 检查路径是否存在，不存在则创建
let checkpath path = 
    if Directory.Exists path = false then
        Directory.CreateDirectory path |> ignore
    path

/// 检查文件是否可写
let checkFile filename =
    try
        File.OpenWrite(filename).Close()
        ""
    with ex ->
        $"{filename} => {ex.Message}"

/// 获取父目录路径
let parentPath path = 
    let dir = DirectoryInfo(path)
    dir.Parent.FullName

/// 读取二进制文件（共享读写）
let readBin filename =
    if File.Exists filename then
        use ms = new MemoryStream()
        use f = File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        f.CopyTo ms
        ms.ToArray()
    else
        [||]

/// 尝试读取二进制文件
let tryReadBin filename =
    try
        "", readBin filename
    with ex ->
        $"{filename} => {ex.Message}", [||]

/// 尝试读取文本文件
let tryReadText filename =
    try
        "", File.ReadAllText filename
    with ex ->
        $"{filename} => {ex.Message}", ""

/// 尝试按行读取文件
let tryReadLines filename =
    try
        let lines = 
            readBin filename 
            |> Encoding.UTF8.GetString
            |> (fun s -> s.Replace(Util.Text.crlf, Util.Text.lf))
            |> Util.Text.str__lines Util.Text.lf
        "", lines
    with ex ->
        $"{filename} => {ex.Message}", [||]

/// 获取文件行数组
let filename__lines = tryReadLines >> snd

/// 尝试写入行到文件
let tryWriteLines filename lines =
    try
        File.WriteAllLines(filename, lines)
        ""
    with ex ->
        $"{filename} => {ex.Message}"

/// 尝试写入文本到文件
let tryWriteText (filename:string) (text:string) =
    try
        File.WriteAllText(filename, text)
        ""
    with ex ->
        $"{filename} => {ex.Message}"

/// 尝试写入字节到文件
let tryWriteBytes filename (bs: byte[]) =
    try
        File.WriteAllBytes(filename, bs)
        ""
    with ex ->
        $"{filename} => {ex.Message}"

/// 尝试覆盖写入字节到文件
let tryWipeWriteBytes filename (bs: byte[]) =
    try 
        if File.Exists filename then
            File.Delete filename
        filename
        |> Path.GetDirectoryName
        |> checkpath
        |> ignore
        File.WriteAllBytes(filename, bs)
        ""
    with ex ->
        $"{filename} => {ex.Message}"

/// 尝试重复写入文本（带重试）
let tryWriteTextRepeat (filename:string, text:string, n, interval:int) =
    let mutable success = false
    let mutable nn = n
    while nn > 0 do
        try
            File.WriteAllText(filename, text)
            success <- true
            nn <- 0
        with _ ->
            nn <- nn - 1
            Threading.Thread.Sleep interval
    success

/// 追加文本到文件末尾
let tryWriteTextAdd filename (text: string[]) =
    try
        if File.Exists filename = false then
            use f = File.Create(filename)
            f.Dispose()
        use file = new StreamWriter(filename, true, Encoding.UTF8)
        text |> Array.iter file.WriteLine
        ""
    with ex ->
        ex.Message

/// 比较两个文件的修改时间
let compareWriteTime (f1, f2) =
    let fi1 = FileInfo f1
    let fi2 = FileInfo f2
    let t1 = if fi1.Exists then fi1.LastWriteTimeUtc else DateTime.MinValue
    let t2 = if fi2.Exists then fi2.LastWriteTimeUtc else DateTime.MinValue
    if t1 > t2 then 1
    elif t1 < t2 then -1
    else 0

/// 批量处理目录中的文件
let batchFiles (path, filter: string -> bool, handler: string * string * string -> unit) =
    Directory.GetFiles path
    |> Seq.iter (fun f ->
        if filter f then
            handler(path, f, f.Substring(path.Length + 1)))

/// 获取目录父路径
let dirParent s = Directory.GetParent(s).FullName

// ========== 文件类型判断和内容提取 ==========

/// 判断文件类别
let getFileCategory (path: string) =
    let ext = Path.GetExtension(path).ToLower()
    match ext with
    // 文本/代码文件
    | ".txt" | ".fs" | ".fsx" | ".fsproj" | ".cs" | ".csproj" | ".vb"
    | ".json" | ".xml" | ".md" | ".yml" | ".yaml" | ".js" | ".ts" | ".tsx" | ".jsx"
    | ".html" | ".css" | ".scss" | ".sass" | ".less" | ".sql" | ".ps1" | ".bat" | ".sh"
    | ".py" | ".rb" | ".go" | ".rs" | ".java" | ".c" | ".cpp" | ".h" | ".hpp"
    | ".ini" | ".cfg" | ".config" | ".log" | ".csv" | ".toml" -> TextCode
    
    // 文档文件
    | ".pdf" | ".doc" | ".docx" | ".xls" | ".xlsx" | ".ppt" | ".pptx"
    | ".odt" | ".ods" | ".odp" | ".rtf" -> Document
    
    // 图片文件
    | ".jpg" | ".jpeg" | ".png" | ".gif" | ".bmp" | ".svg" | ".webp" | ".ico" -> Image
    
    | _ -> Binary

/// 尝试读取 PDF 文本
let tryReadPdfText output (path:string) =
    try
        $"PDF 文件: {Path.GetFileName(path)} (完整文本提取需要安装 PdfPig)" |> output
        $"[PDF 文件: {Path.GetFileName(path)}，需要安装 PdfPig 以提取文本内容]"
    with ex ->
        $"PDF 读取失败: {ex.Message}"

/// 尝试读取 Word 文档文本
let tryReadDocxText output (path:string) =
    try
        $"Word 文档: {Path.GetFileName(path)} (完整文本提取需要安装 DocumentFormat.OpenXml)" |> output
        $"[Word 文档: {Path.GetFileName(path)}]"
    with ex ->
        $"Word 读取失败: {ex.Message}"

/// 获取图片信息
let getImageInfo path =
    try
        let info = FileInfo(path)
        $"图片: {Path.GetFileName(path)}, 大小: {info.Length} 字节"
    with ex ->
        $"图片信息读取失败: {ex.Message}"

/// 读取文本/代码文件内容
let readTextFileContent output maxLength (path:string) =
    try
        let encoding = 
            try 
                use reader = new StreamReader(path, Encoding.UTF8, true)
                reader.CurrentEncoding
            with _ -> Encoding.UTF8
        
        use reader = new StreamReader(path, encoding)
        let content = reader.ReadToEnd()
        let fileName = Path.GetFileName(path)
        
        if content.Length > maxLength then
            let truncated = content.Substring(0, maxLength)
            $"文件 {fileName} 内容已截断 ({content.Length} -> {maxLength} 字符)" |> output
            $"""--- 文件: {fileName} 开始 (共 {content.Length} 字符，已截断) ---
{truncated}
--- 文件: {fileName} 结束 ---"""
        else
            $"""--- 文件: {fileName} 开始 (共 {content.Length} 字符) ---
{content}
--- 文件: {fileName} 结束 ---"""
    with ex ->
        $"读取文本文件失败: {path}, 错误: {ex.Message}" |> output
        $"""--- 文件: {Path.GetFileName(path)} 读取失败: {ex.Message} ---"""

/// 异步读取文本/代码文件内容
let readTextFileContentAsync output maxLength (path:string) =
    async {
        try
            let encoding = 
                try 
                    use reader = new StreamReader(path, Encoding.UTF8, true)
                    reader.CurrentEncoding
                with _ -> Encoding.UTF8
            
            use reader = new StreamReader(path, encoding)
            let! content = reader.ReadToEndAsync() |> Async.AwaitTask
            let fileName = Path.GetFileName(path)
            
            if content.Length > maxLength then
                let truncated = content.Substring(0, maxLength)
                $"文件 {fileName} 内容已截断 ({content.Length} -> {maxLength} 字符)" |> output
                return $"""--- 文件: {fileName} 开始 (共 {content.Length} 字符，已截断) ---
{truncated}
--- 文件: {fileName} 结束 ---"""
            else
                return $"""--- 文件: {fileName} 开始 (共 {content.Length} 字符) ---
{content}
--- 文件: {fileName} 结束 ---"""
        with ex ->
            $"读取文本文件失败: {path}, 错误: {ex.Message}" |> output
            return $"""--- 文件: {Path.GetFileName(path)} 读取失败: {ex.Message} ---"""
    }

/// 构建文件附件内容（同步）
let buildFileAttachmentContent output maxTextLength path =
    if not (File.Exists path) then
        $"""--- 目录: {path} ---"""
    else
        match getFileCategory path with
        | TextCode ->
            readTextFileContent output maxTextLength path
        
        | Document ->
            let ext = Path.GetExtension(path).ToLower()
            match ext with
            | ".pdf" -> tryReadPdfText output path
            | ".docx" | ".doc" -> tryReadDocxText output path
            | ".xlsx" | ".xls" -> 
                $"Excel 文件: {Path.GetFileName(path)} (完整提取需要安装 EPPlus)" |> output
                $"[Excel 文件: {Path.GetFileName(path)}]"
            | ".pptx" | ".ppt" ->
                $"PowerPoint 文件: {Path.GetFileName(path)}" |> output
                $"[PowerPoint 文件: {Path.GetFileName(path)}]"
            | _ ->
                $"文档文件: {Path.GetFileName(path)}" |> output
                $"[文档文件: {Path.GetFileName(path)}]"
        
        | Image ->
            getImageInfo path
        
        | Binary ->
            let info = FileInfo(path)
            $"""--- 二进制文件: {Path.GetFileName(path)} (大小: {info.Length} 字节) ---
[二进制文件内容无法直接显示]"""

/// 构建文件附件内容（异步）
let buildFileAttachmentContentAsync output maxTextLength path =
    async {
        if not (File.Exists path) then
            return $"""--- 目录: {path} ---"""
        else
            match getFileCategory path with
            | TextCode ->
                return! readTextFileContentAsync output maxTextLength path
            
            | Document ->
                let ext = Path.GetExtension(path).ToLower()
                match ext with
                | ".pdf" -> return tryReadPdfText output path
                | ".docx" | ".doc" -> return tryReadDocxText output path
                | ".xlsx" | ".xls" -> 
                    $"Excel 文件: {Path.GetFileName(path)} (完整提取需要安装 EPPlus)" |> output
                    return $"[Excel 文件: {Path.GetFileName(path)}]"
                | ".pptx" | ".ppt" ->
                    $"PowerPoint 文件: {Path.GetFileName(path)}" |> output
                    return $"[PowerPoint 文件: {Path.GetFileName(path)}]"
                | _ ->
                    $"文档文件: {Path.GetFileName(path)}" |> output
                    return $"[文档文件: {Path.GetFileName(path)}]"
            
            | Image ->
                return getImageInfo path
            
            | Binary ->
                let info = FileInfo(path)
                return $"""--- 二进制文件: {Path.GetFileName(path)} (大小: {info.Length} 字节) ---
[二进制文件内容无法直接显示]"""
    }

/// 批量构建附件内容（同步）
let buildAttachmentsContent output maxTextLength (paths:string[]) =
    if paths.Length = 0 then
        ""
    else
        paths
        |> Array.map (buildFileAttachmentContent output maxTextLength)
        |> Array.filter (not << String.IsNullOrWhiteSpace)
        |> String.concat "\n\n"

/// 批量构建附件内容（异步）
let buildAttachmentsContentAsync output maxTextLength (paths:string[]) =
    async {
        if paths.Length = 0 then
            return ""
        else
            let! contents = 
                paths
                |> Array.map (buildFileAttachmentContentAsync output maxTextLength)
                |> Async.Sequential
            return contents |> Array.filter (not << String.IsNullOrWhiteSpace) |> String.concat "\n\n"
    }