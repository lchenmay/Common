namespace UtilAvalonia.Comp.TabContainer

open System.Collections.ObjectModel
open Avalonia.Controls
open Avalonia.Media

open UtilAvalonia.UiUtil

type TabContainer() as this =
    inherit UserControl()
    let tabControl = TabControl()

    do
        tabControl.Background <- BuildInColor.TabBackground |> color__Brush
        this.Content <- tabControl

    member this.AddTab header isClosable content =
        let tabItem = TabItem()
        tabItem.Header <- header
        tabItem.Content <- content
        tabControl.Items.Add tabItem |> ignore
        tabControl.SelectedItem <- tabItem
        tabItem

    member this.OpenOrActivateTab header isClosable content =
        let existing = 
            tabControl.Items 
            |> Seq.cast<TabItem>
            |> Seq.tryFind (fun ti -> ti.Header = header)
        match existing with
        | Some ti ->
            ti.Content <- content
            tabControl.SelectedItem <- ti
        | None ->
            this.AddTab header isClosable content |> ignore