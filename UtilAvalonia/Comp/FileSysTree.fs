module UtilAvalonia.Comp.FileSysTree

open System
open System.IO
open System.Threading.Tasks
open System.Diagnostics
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Threading
open Avalonia.Platform.Storage
open Avalonia.Media
open Avalonia.Layout

// ========== 内部辅助函数 ==========
let getSortedDirectories dirPath =
    try Directory.GetDirectories(dirPath) |> Array.map Path.GetFileName |> Array.sortBy (fun n -> n.ToLower())
    with _ -> [||]

let getSortedFiles dirPath =
    try Directory.GetFiles(dirPath) |> Array.map Path.GetFileName |> Array.sortBy (fun n -> n.ToLower())
    with _ -> [||]

/// 创建带复选框的树节点
let createTreeNode header path isDirectory =
    let panel = StackPanel(Orientation = Orientation.Horizontal)
    let checkBox = CheckBox(VerticalAlignment = VerticalAlignment.Center, Margin = Thickness(2.0, 0.0, 8.0, 0.0))
    let textBlock = TextBlock(Text = header, VerticalAlignment = VerticalAlignment.Center)
    panel.Children.Add checkBox
    panel.Children.Add textBlock
    let node = TreeViewItem(Header = panel, Tag = path)
    let ctx = ContextMenu()
    let openItem = MenuItem(Header = (if isDirectory then "从文件管理器打开" else "打开所在文件夹"))
    openItem.Click.Add(fun _ ->
        try
            if Directory.Exists path then Process.Start("explorer.exe", path) |> ignore
            elif File.Exists path then Process.Start("explorer.exe", $"/select, \"{path}\"") |> ignore
        with _ -> ())
    ctx.Items.Add openItem
    node.ContextMenu <- ctx
    node

/// 创建简单树节点（不带复选框）
let createSimpleTreeNode header path isDirectory =
    let node = TreeViewItem(Header = header, Tag = path)
    let ctx = ContextMenu()
    let openItem = MenuItem(Header = (if isDirectory then "从文件管理器打开" else "打开所在文件夹"))
    openItem.Click.Add(fun _ ->
        try
            if Directory.Exists path then Process.Start("explorer.exe", path) |> ignore
            elif File.Exists path then Process.Start("explorer.exe", $"/select, \"{path}\"") |> ignore
        with _ -> ())
    ctx.Items.Add openItem
    node.ContextMenu <- ctx
    node

/// 递归加载目录内容到树节点（使用带复选框的节点）
let rec loadDirectory (node: TreeViewItem) (dirPath: string) (log: string -> unit) =
    if node.Tag <> null && node.Tag.ToString() = "loaded" then
        log $"loadDirectory 跳过: 节点已加载"
    else
        log $"loadDirectory 开始加载目录: {dirPath}"
        Task.Run(fun () ->
            try
                let subDirs = getSortedDirectories dirPath
                let files = getSortedFiles dirPath
                log $"找到 {subDirs.Length} 个子目录, {files.Length} 个文件"
                Dispatcher.UIThread.Post(fun () ->
                    node.Items.Clear()
                    for dirName in subDirs do
                        let fullPath = Path.Combine(dirPath, dirName)
                        let dirNode = createTreeNode dirName fullPath true
                        let hasContent = Directory.GetDirectories(fullPath).Length > 0 || Directory.GetFiles(fullPath).Length > 0
                        if hasContent then
                            dirNode.Items.Add(createTreeNode "加载中..." fullPath true) |> ignore
                            dirNode.Expanded.Add(fun _ -> loadDirectory dirNode fullPath log)
                        node.Items.Add(dirNode) |> ignore
                    for fileName in files do
                        let fullPath = Path.Combine(dirPath, fileName)
                        node.Items.Add(createTreeNode fileName fullPath false) |> ignore
                    node.Tag <- box "loaded"
                    log $"目录加载完成: {dirPath}"
                )
            with ex ->
                log $"loadDirectory 异常: {ex.Message}"
                Dispatcher.UIThread.Post(fun () ->
                    node.Items.Clear()
                    node.Items.Add(createTreeNode $"错误: {ex.Message}" dirPath true) |> ignore
                    node.Tag <- box "error"
                )
        ) |> ignore

// ========== 公开的组件 ==========

/// 通用文件树视图，根节点可动态更换目录，支持自定义右键菜单
type FileTreeView(getMainWindow: unit -> Window, onRootPathChanged: string -> unit, ?log: string -> unit) as this =
    inherit UserControl()

    let log = defaultArg log ignore
    let mutable currentRootPath = ""
    let treeView = TreeView()
    let rootNode = createSimpleTreeNode "根目录" "" true

    // 刷新树：清除旧内容，加载新路径
    let refreshTree () =
        if String.IsNullOrEmpty currentRootPath then
            rootNode.Header <- "根目录 (未选择)"
            rootNode.Tag <- box "placeholder"
            rootNode.Items.Clear()
        else
            rootNode.Header <- sprintf "根目录 (%s)" currentRootPath
            rootNode.Tag <- box "placeholder"
            rootNode.Items.Clear()
            let placeholder = createSimpleTreeNode "加载中..." "" true
            rootNode.Items.Add placeholder |> ignore
            // 展开时加载实际目录
            rootNode.Expanded.Add(fun _ ->
                if rootNode.Tag <> null && rootNode.Tag.ToString() = "loaded" then ()
                elif Directory.Exists currentRootPath then
                    log $"开始加载目录: {currentRootPath}"
                    rootNode.Items.Clear()
                    loadDirectory rootNode currentRootPath log
                    rootNode.Tag <- box "loaded"
                else
                    log $"目录不存在: {currentRootPath}"
                    rootNode.Header <- sprintf "根目录 (不存在: %s)" currentRootPath
            )

    // 更换根目录：弹出文件夹选择器
    let changeRoot () =
        task {
            let win = getMainWindow()
            if win <> null then
                let storage = win.StorageProvider
                let options = FolderPickerOpenOptions(Title = "选择根目录", AllowMultiple = false)
                if Directory.Exists currentRootPath then
                    let! start = storage.TryGetFolderFromPathAsync(currentRootPath)
                    options.SuggestedStartLocation <- start
                let! folders = storage.OpenFolderPickerAsync(options)
                if folders.Count > 0 then
                    let newPath = folders[0].Path.LocalPath
                    currentRootPath <- newPath
                    log $"根目录已更换为: {currentRootPath}"
                    onRootPathChanged newPath
                    refreshTree()
        } |> ignore

    do
        // 构建根节点的右键菜单
        let ctxMenu = ContextMenu()
        let changeItem = MenuItem(Header = "更换文件夹")
        changeItem.Click.Add(fun _ -> changeRoot())
        ctxMenu.Items.Add changeItem

        rootNode.ContextMenu <- ctxMenu
        rootNode.IsExpanded <- false   // 初始不展开
        treeView.Items.Add rootNode
        this.Content <- treeView

    /// 设置根目录路径（由外部调用，例如从配置加载后）
    member _.SetRootPath(path: string) =
        currentRootPath <- path
        refreshTree()

    /// 添加自定义右键菜单项
    member _.AddMenuItem(header: string, onClick: unit -> unit) =
        let item = MenuItem(Header = header)
        item.Click.Add(fun _ -> onClick())
        rootNode.ContextMenu.Items.Add item

    /// 获取当前根节点
    member _.RootNode = rootNode