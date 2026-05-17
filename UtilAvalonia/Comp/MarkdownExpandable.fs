module UtilAvalonia.Comp.MarkdownExpandable

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media

open UtilAvalonia.Markdown

/// 可折叠 Markdown 控件的配置选项
type MarkdownExpandableOptions = {
    /// 背景色
    BackgroundColor: Color
    /// 圆角半径
    CornerRadius: CornerRadius
    /// 外边距
    Margin: Thickness
    /// 内边距
    Padding: Thickness
    /// 按钮宽度
    ButtonWidth: float
    /// 按钮高度
    ButtonHeight: float
    /// 按钮右边距
    ButtonMargin: Thickness
    /// 预览行数（展开前显示的行数）
    PreviewLines: int
    /// Markdown 渲染选项
    MarkdownOptions: MarkdownRenderOptions
}

/// 默认配置
let defaultOptions = {
    BackgroundColor = Color.Parse("#2D2D30")
    CornerRadius = CornerRadius(8.0)
    Margin = Thickness(2.0, 2.0, 2.0, 4.0)
    Padding = Thickness(5.0)
    ButtonWidth = 30.0
    ButtonHeight = 20.0
    ButtonMargin = Thickness(0.0, 0.0, 5.0, 0.0)
    PreviewLines = 2
    MarkdownOptions = defaultOptions
}

/// 可折叠 Markdown 内容控件
type MarkdownExpandable(content: string, isExpanded: bool, ?options: MarkdownExpandableOptions) as this =
    inherit Border()

    let opts = defaultArg options defaultOptions
    let mutable expanded = isExpanded
    let mutable fullContent = content
    
    let getPreviewText (text: string) = getPreviewText text opts.PreviewLines
    
    let createContentView text =
        let displayText = if expanded then text else getPreviewText text
        renderMarkdown displayText opts.MarkdownOptions :> Control

    let expandCollapseBtn = 
        Button(Content = (if expanded then "▲" else "▼"), 
               Width = opts.ButtonWidth,
               Height = opts.ButtonHeight,
               HorizontalAlignment = HorizontalAlignment.Right,
               Margin = opts.ButtonMargin)
        
    let mutable contentViewer = createContentView fullContent
    
    let toggleExpand () =
        expanded <- not expanded
        expandCollapseBtn.Content <- if expanded then "▲" else "▼"
        let newViewer = createContentView fullContent
        let parent = contentViewer.Parent
        if parent <> null then
            let panel = parent :?> Panel
            let index = panel.Children.IndexOf(contentViewer)
            panel.Children.Remove(contentViewer) |> ignore
            panel.Children.Insert(index, newViewer)
        contentViewer <- newViewer

    do
        expandCollapseBtn.Click.Add(fun _ -> toggleExpand())

        this.Background <- SolidColorBrush(opts.BackgroundColor)
        this.CornerRadius <- opts.CornerRadius
        this.Margin <- opts.Margin
        this.Padding <- opts.Padding
        
        let rootPanel = DockPanel()
        let buttonPanel = StackPanel(Orientation = Orientation.Horizontal)
        buttonPanel.Children.Add(expandCollapseBtn) |> ignore
        DockPanel.SetDock(buttonPanel, Dock.Right)
        
        rootPanel.Children.Add(buttonPanel) |> ignore
        rootPanel.Children.Add(contentViewer) |> ignore
        
        this.Child <- rootPanel

    /// 获取或设置展开状态
    member _.IsExpanded
        with get() = expanded
        and set(v) = if v <> expanded then toggleExpand()
    
    /// 获取完整内容
    member _.Content = fullContent
    
    /// 设置新内容
    member _.SetContent(newContent: string) =
        fullContent <- newContent
        let newViewer = createContentView fullContent
        let parent = contentViewer.Parent
        if parent <> null then
            let panel = parent :?> Panel
            let index = panel.Children.IndexOf(contentViewer)
            panel.Children.Remove(contentViewer) |> ignore
            panel.Children.Insert(index, newViewer)
        contentViewer <- newViewer
    
    /// 手动切换展开/折叠
    member _.ToggleExpand() = toggleExpand()