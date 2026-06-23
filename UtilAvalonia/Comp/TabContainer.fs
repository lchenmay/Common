module UtilAvalonia.Comp.TabContainer

open System.Collections.ObjectModel
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

    member this.AddTab header content =
        let tabItem = new TabItem()
        tabItem.Header <- header |> txt__TextBlockTab
        tabItem.Content <- content
        tabControl.Items.Add tabItem |> ignore
        tabControl.SelectedItem <- tabItem
        tabItem

    member this.OpenOrActivateTab header content =
        let existing = 
            tabControl.Items 
            |> Seq.cast<TabItem>
            |> Seq.tryFind (fun ti -> 
                match ti.Header with
                | :? TextBlock as tb -> tb.Text = header
                | :? string as s -> s = header
                | _ -> false)
        match existing with
        | Some ti ->
            ti.Content <- content
            tabControl.SelectedItem <- ti
        | None ->
            this.AddTab header content |> ignore
