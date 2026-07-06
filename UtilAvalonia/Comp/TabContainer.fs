module UtilAvalonia.Comp.TabContainer

open System.Collections.ObjectModel
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media

open UtilAvalonia.UiUtil
open UtilAvalonia.Style

type TabContainer() as this =
    inherit UserControl()
    let tabControl = TabControl()

    do
        tabControl.Background <- BuildInColor.TabBackground |> color__Brush
        tabControl.HorizontalAlignment <- HorizontalAlignment.Stretch
        tabControl.VerticalAlignment <- VerticalAlignment.Stretch
        this.Content <- tabControl

    member private this.DetachFromParent(content: obj) =
        match content with
        | :? Control as ctrl when ctrl.Parent <> null ->
            match ctrl.Parent with
            | :? Panel as p -> p.Children.Remove(ctrl) |> ignore
            | :? ContentControl as cc -> cc.Content <- null
            | :? Decorator as d -> d.Child <- null
            | _ -> ()
        | _ -> ()

    member this.AddTab header content =
        let tabItem = new TabItem()
        
        // 自定义 Header：文本 + ✕ 关闭按钮
        let headerPanel = StackPanel(Orientation = Orientation.Horizontal)
        let titleText = header |> txt__TextBlockTab
        titleText.VerticalAlignment <- VerticalAlignment.Center
        
        let closeBtn = Button(Content = "✕")
        closeBtn.Width <- 18.0
        closeBtn.Height <- 18.0
        closeBtn.Margin <- Thickness(6.0, 0.0, 0.0, 0.0)
        closeBtn.Background <- Brushes.Transparent
        closeBtn.Foreground <- BuildInColor.DefaultForeground |> color__Brush
        closeBtn.BorderThickness <- Thickness(0.0)
        closeBtn.Padding <- Thickness(0.0)
        closeBtn.VerticalAlignment <- VerticalAlignment.Center
        closeBtn.Click.Add(fun _ ->
            let idx = tabControl.Items.IndexOf(tabItem)
            tabItem.Content <- null  // 先解绑，防止视觉树残留
            tabControl.Items.Remove(tabItem) |> ignore
            if tabControl.Items.Count > 0 then
                let newIdx = min idx (tabControl.Items.Count - 1)
                tabControl.SelectedIndex <- newIdx
        )
        
        headerPanel.Children.Add(titleText) |> ignore
        headerPanel.Children.Add(closeBtn) |> ignore
        tabItem.Header <- headerPanel
        tabItem.Tag <- header  // 用于 OpenOrActivateTab 查找
        
        this.DetachFromParent(content)
        tabItem.Content <- content
        tabControl.Items.Add tabItem |> ignore
        tabControl.SelectedItem <- tabItem
        tabItem

    member this.OpenOrActivateTab header content =
        let existing = 
            tabControl.Items 
            |> Seq.cast<TabItem>
            |> Seq.tryFind (fun ti -> 
                match ti.Tag with
                | :? string as s -> s = header
                | _ -> false)
        match existing with
        | Some ti ->
            if not (obj.ReferenceEquals(ti.Content, content)) then
                this.DetachFromParent(content)
                ti.Content <- content
            tabControl.SelectedItem <- ti
        | None ->
            this.AddTab header content |> ignore
