namespace UtilAvalonia.Comp.FileSysTree

open System
open System.IO
open System.Collections.ObjectModel
open System.Collections.Specialized

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Threading
open Avalonia.Platform.Storage
open Avalonia.Media

open UtilAvalonia.UiUtil

open UtilAvalonia.Comp.FileSysTree.Logics
open UtilAvalonia.Comp.FileSysTree.Layout

/// 文件树视图，包含复选框和已选列表
type FileTreeView(getMainWindow: unit -> Window, onRootPathChanged: string -> unit, ?log: string -> unit, ?checkboxSize: float) as this =
    inherit UserControl()

    let log = defaultArg log ignore
    let checkboxSize = defaultArg checkboxSize 12.0
    let mutable currentRootPath = ""
    let mutable isExpandedHandlerAttached = false
    let treeView = TreeView()
    let selectedPaths = ObservableCollection<string>()
    let selectedPathsChanged = Event<NotifyCollectionChangedEventArgs>()

    let rootNode, _ = createTreeNode "文件服务" "" true 0.0

    // 刷新树：清除旧内容，加载新路径
    let refreshTree () =
        if String.IsNullOrEmpty currentRootPath then
            rootNode.Header <- "文件服务 (未选择)"
            rootNode.Tag <- box "placeholder"
            rootNode.Items.Clear()
        else
            rootNode.Header <- sprintf "文件服务 (%s)" currentRootPath
            rootNode.Tag <- box "placeholder"
            rootNode.Items.Clear()
            let placeholder, _ = createTreeNode "加载中..." "" true 0.0
            rootNode.Items.Add placeholder |> ignore
            
            if not isExpandedHandlerAttached then
                isExpandedHandlerAttached <- true
                rootNode.Expanded.Add(fun _ ->
                    if rootNode.Tag <> null && rootNode.Tag.ToString() = "loaded" then ()
                    elif Directory.Exists currentRootPath then
                        log $"开始加载目录: {currentRootPath}"
                        rootNode.Items.Clear()
                        loadDirectoryWithCheckbox rootNode currentRootPath log checkboxSize selectedPaths
                        rootNode.Tag <- box "loaded"
                    else
                        log $"目录不存在: {currentRootPath}"
                        rootNode.Header <- sprintf "文件服务 (不存在: %s)" currentRootPath
                )

    // 更换根目录
    let changeRoot () =
        task {
            let win = getMainWindow()
            if win <> null then
                let storage = win.StorageProvider
                let options = FolderPickerOpenOptions(Title = "选择文件服务根目录", AllowMultiple = false)
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

    // 清空已选列表
    let clearSelectedPaths () = selectedPaths.Clear()

    do
        selectedPaths.CollectionChanged.Add(fun args -> 
            selectedPathsChanged.Trigger(args)
            log $"selectedPaths 变化: {args.Action}, 当前数量: {selectedPaths.Count}"
        )

        // 构建根节点的右键菜单
        let ctxMenu = ContextMenu()
        let changeItem = MenuItem(Header = "更换文件夹")
        changeItem.Click.Add(fun _ -> changeRoot())
        ctxMenu.Items.Add changeItem
        rootNode.ContextMenu <- ctxMenu

        // 将文件服务根节点添加到树
        treeView.Items.Add(rootNode)
        
        // 使用布局函数创建界面
        let layout = createFileTreeLayout treeView selectedPaths clearSelectedPaths log
        this.Content <- layout

    /// 添加根节点（用于开发项目和文档项目）
    member this.AddRootNode(header: string, tag: obj) =
        let node = TreeViewItem(Header = txt__TextBlock header, Tag = tag)
        treeView.Items.Add(node) |> ignore
        node

    /// 添加带右键菜单的根节点
    member this.AddRootNode(header: string, tag: obj, contextMenu: ContextMenu) =
        let node = TreeViewItem(Header = txt__TextBlock header, Tag = tag, ContextMenu = contextMenu)
        treeView.Items.Add(node) |> ignore
        node

    /// 设置根目录路径
    member _.SetRootPath(path: string) =
        currentRootPath <- path
        refreshTree()

    /// 添加自定义右键菜单项到文件服务节点
    member _.AddMenuItem(header: string, onClick: unit -> unit) =
        let item = MenuItem(Header = header)
        item.Click.Add(fun _ -> onClick())
        rootNode.ContextMenu.Items.Add item

    /// 获取文件服务根节点
    member _.RootNode = rootNode
    
    /// 获取已选路径集合
    member _.SelectedPaths = selectedPaths
    
    /// 已选路径变更事件
    member _.SelectedPathsChanged = selectedPathsChanged.Publish
    
    /// 刷新树（重新加载）
    member _.Refresh() = 
        refreshTree()