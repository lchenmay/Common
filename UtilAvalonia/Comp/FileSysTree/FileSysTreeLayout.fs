module UtilAvalonia.Comp.FileSysTree.Layout

open System
open System.Collections.ObjectModel
open System.Collections.Specialized
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open UtilAvalonia.UiUtil

/// 创建文件树的布局
let createFileTreeLayout 
    (treeView: TreeView)
    (selectedPaths: ObservableCollection<string>)
    (clearSelectedPaths: unit -> unit)
    (log: string -> unit) =

    log "=== createFileTreeLayout 开始 ==="
    
    // ========== 已选列表区域 ==========
    let listBox = ListBox()
    listBox.ItemsSource <- selectedPaths
    listBox.MinHeight <- 100
    listBox.MaxHeight <- 150
    listBox.HorizontalAlignment <- HorizontalAlignment.Stretch
    listBox.Background <- SolidColorBrush(Colors.DarkBlue)  // 调试用蓝色背景
    log $"ListBox 已创建, MinHeight={listBox.MinHeight}, MaxHeight={listBox.MaxHeight}"
    
    let clearBtn = Button(Content = "清空列表", Width = 80.0)
    clearBtn.Click.Add(fun _ -> 
        log "清空列表按钮被点击"
        clearSelectedPaths()
    )
    
    let topButtonPanel = StackPanel(Orientation = Orientation.Horizontal, Spacing = 10.0, Margin = Thickness(5.0))
    topButtonPanel.HorizontalAlignment <- HorizontalAlignment.Right
    topButtonPanel.Children.Add clearBtn
    
    let listHeader = TextBlock(Text = "已选路径:", FontWeight = FontWeight.Bold, Margin = Thickness(5.0, 5.0, 5.0, 0.0))
    listHeader.VerticalAlignment <- VerticalAlignment.Center
    listHeader.Foreground <- Brushes.White
    
    let headerPanel = StackPanel(Orientation = Orientation.Horizontal)
    headerPanel.Children.Add listHeader
    headerPanel.Children.Add topButtonPanel
    headerPanel.Background <- SolidColorBrush(Colors.DarkGreen)  // 调试用绿色背景
    headerPanel.Margin <- Thickness(0.0, 0.0, 0.0, 2.0)
    
    let topPanel = StackPanel()
    topPanel.Children.Add headerPanel
    topPanel.Children.Add listBox
    topPanel.Background <- SolidColorBrush(Colors.DarkSlateGray)  // 调试用深灰色背景
    
    let topBorder = Border(
        Background = SolidColorBrush(Color.Parse("#2D2D30")),
        BorderBrush = SolidColorBrush(Color.Parse("#3E3E42")),
        BorderThickness = Thickness(0.0, 0.0, 0.0, 1.0),
        Child = topPanel
    )
    
    // 调试：设置 treeView 的背景色
    treeView.Background <- SolidColorBrush(Colors.DarkRed)  // 调试用红色背景
    log "treeView 背景已设置为红色"
    
    // ========== 使用 Grid 实现上下布局 ==========
    let grid = Grid()
    grid.Background <- SolidColorBrush(Colors.Purple)  // 调试用紫色背景
    
    // 定义两行：第一行自动高度，第二行占满剩余空间
    grid.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))
    grid.RowDefinitions.Add(RowDefinition(Height = GridLength(1.0, GridUnitType.Star)))
    
    log $"Grid 行数: {grid.RowDefinitions.Count}"
    log $"第0行高度: {grid.RowDefinitions.[0].Height}"
    log $"第1行高度: {grid.RowDefinitions.[1].Height}"
    
    // 添加顶部区域到第一行
    Grid.SetRow(topBorder, 0)
    grid.Children.Add(topBorder)
    
    // 添加树视图到第二行
    Grid.SetRow(treeView, 1)
    grid.Children.Add(treeView)
    
    log $"Grid 子元素数量: {grid.Children.Count}"
    log "=== createFileTreeLayout 完成 ==="
    
    grid