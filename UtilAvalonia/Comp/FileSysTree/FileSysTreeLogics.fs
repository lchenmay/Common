module UtilAvalonia.Comp.FileSysTree.Logics

open System
open System.IO
open System.Threading.Tasks
open System.Collections.ObjectModel
open System.Diagnostics

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Threading

open UtilAvalonia.UiUtil

// ========== 内部辅助函数 ==========
let getSortedDirectories dirPath =
    try Directory.GetDirectories(dirPath) |> Array.map Path.GetFileName |> Array.sortBy (fun n -> n.ToLower())
    with _ -> [||]

let getSortedFiles dirPath =
    try Directory.GetFiles(dirPath) |> Array.map Path.GetFileName |> Array.sortBy (fun n -> n.ToLower())
    with _ -> [||]

// 创建带复选框的树节点，返回 (节点, 复选框option)
let createTreeNode header path isDirectory checkboxSize =
    let panel = StackPanel(Orientation = Orientation.Horizontal)

    let textBlock = txt__TextBlock header
    textBlock.VerticalAlignment <- VerticalAlignment.Center
    textBlock.HorizontalAlignment <- HorizontalAlignment.Left

    let checkBox = 
        if checkboxSize > 0.0 then
            let cb = CheckBox(
                VerticalAlignment = VerticalAlignment.Center,
                HorizontalAlignment = HorizontalAlignment.Left,
                Margin = Thickness(2.0, 0.0, 8.0, 0.0),
                Width = checkboxSize,
                Height = checkboxSize,
                MinWidth = checkboxSize,
                MinHeight = checkboxSize)
            Some cb
        else
            None

    match checkBox with
    | Some cb -> panel.Children.Add cb
    | None -> ()
    
    panel.Children.Add textBlock
    
    let node = TreeViewItem(Header = panel, Tag = path)
    
    // 右键菜单
    let ctx = ContextMenu()
    let openItem = MenuItem(Header = (if isDirectory then "从文件管理器打开" else "打开所在文件夹"))
    openItem.Click.Add(fun _ ->
        try
            if Directory.Exists path then Process.Start("explorer.exe", path) |> ignore
            elif File.Exists path then Process.Start("explorer.exe", $"/select, \"{path}\"") |> ignore
        with _ -> ())

    ctx.Items.Add openItem |> ignore
    node.ContextMenu <- ctx
    
    node, checkBox

/// 递归加载目录内容到树节点（带复选框，并同步 selectedPaths）
let rec loadDirectoryWithCheckbox 
    (node: TreeViewItem)
    (dirPath: string) 
    (log: string -> unit)
    (checkboxSize: float)
    (selectedPaths: ObservableCollection<string>) =

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
                        let fullPath = Path.GetFullPath(Path.Combine(dirPath, dirName))
                        log $"创建目录节点: {dirName} -> {fullPath}"
                        let dirNode, cbOpt = createTreeNode dirName fullPath true checkboxSize
                        // 绑定复选框事件
                        match cbOpt with
                        | Some cb ->
                            cb.IsCheckedChanged.Add(fun _ ->
                                if cb.IsChecked = true then
                                    if not (selectedPaths.Contains(fullPath)) then
                                        selectedPaths.Add(fullPath)
                                        log $"复选框选中添加: {fullPath}"
                                else
                                    selectedPaths.Remove(fullPath) |> ignore
                                    log $"复选框取消移除: {fullPath}"
                            )
                        | None -> ()
                        
                        let hasContent = Directory.GetDirectories(fullPath).Length > 0 || Directory.GetFiles(fullPath).Length > 0
                        if hasContent then
                            let placeholder, _ = createTreeNode "加载中..." fullPath true checkboxSize
                            dirNode.Items.Add placeholder |> ignore
                            dirNode.Expanded.Add(fun _ -> 
                                loadDirectoryWithCheckbox dirNode fullPath log checkboxSize selectedPaths)
                        node.Items.Add(dirNode) |> ignore
                    for fileName in files do
                        let fullPath = Path.Combine(dirPath, fileName)
                        let fileNode, cbOpt = createTreeNode fileName fullPath false checkboxSize
                        match cbOpt with
                        | Some cb ->
                            cb.IsCheckedChanged.Add(fun _ ->
                                if cb.IsChecked = true then
                                    if not (selectedPaths.Contains(fullPath)) then
                                        selectedPaths.Add(fullPath)
                                        log $"复选框选中添加: {fullPath}"
                                else
                                    selectedPaths.Remove(fullPath) |> ignore
                                    log $"复选框取消移除: {fullPath}"
                            )
                        | None -> ()
                        node.Items.Add(fileNode) |> ignore
                    node.Tag <- box "loaded"
                    log $"目录加载完成: {dirPath}"
                )
            with ex ->
                log $"loadDirectory 异常: {ex.Message}"
                Dispatcher.UIThread.Post(fun () ->
                    node.Items.Clear()
                    let errNode, _ = createTreeNode $"错误: {ex.Message}" dirPath true checkboxSize
                    node.Items.Add errNode |> ignore
                    node.Tag <- box "error"
                )
        ) |> ignore

/// 递归加载目录内容到树节点（不带复选框同步，用于开发/文档项目）
let rec loadDirectory 
    (node: TreeViewItem)
    (dirPath: string) 
    (log: string -> unit)
    (checkboxSize: float) =

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
                        let fullPath = Path.GetFullPath(Path.Combine(dirPath, dirName))
                        log $"创建目录节点: {dirName} -> {fullPath}"
                        let dirNode, _ = createTreeNode dirName fullPath true checkboxSize
                        let hasContent = Directory.GetDirectories(fullPath).Length > 0 || Directory.GetFiles(fullPath).Length > 0
                        if hasContent then
                            let placeholder, _ = createTreeNode "加载中..." fullPath true checkboxSize
                            dirNode.Items.Add placeholder |> ignore
                            dirNode.Expanded.Add(fun _ -> 
                                loadDirectory dirNode fullPath log checkboxSize)
                        node.Items.Add(dirNode) |> ignore
                    for fileName in files do
                        let fullPath = Path.Combine(dirPath, fileName)
                        let fileNode, _ = createTreeNode fileName fullPath false checkboxSize
                        node.Items.Add(fileNode) |> ignore
                    node.Tag <- box "loaded"
                    log $"目录加载完成: {dirPath}"
                )
            with ex ->
                log $"loadDirectory 异常: {ex.Message}"
                Dispatcher.UIThread.Post(fun () ->
                    node.Items.Clear()
                    let errNode, _ = createTreeNode $"错误: {ex.Message}" dirPath true checkboxSize
                    node.Items.Add errNode |> ignore
                    node.Tag <- box "error"
                )
        ) |> ignore