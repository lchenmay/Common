module UtilAvalonia.UiUtil

open LanguagePrimitives

open System
open System.IO
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Threading.Tasks

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Media
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Threading
open Avalonia.Platform.Storage

type BuildInColor =
    | DefaultForeground = 0
    | DefaultBackground = 1 
    | Green = 2
    | Red = 3
    | Yellow = 4
    | Cyan = 5
    | White = 6
    | ToolbarBackground = 101
    | StatusPanelBackground = 201
    | StatusPanelForeground = 202
    | TabBackground = 301

let color__Brush ccolor =
    match ccolor with
    | BuildInColor.DefaultBackground -> "#0D2B1F"
    | BuildInColor.DefaultForeground -> "#E6E6A3"
    | BuildInColor.Green   -> "#4ECB71"
    | BuildInColor.Red     -> "#E66A5C"
    | BuildInColor.Yellow  -> "#F5D033"
    | BuildInColor.Cyan    -> "#45E3E3"
    | BuildInColor.White   -> "#FFFFFF"
    | BuildInColor.ToolbarBackground -> "#2D2D30"
    | BuildInColor.StatusPanelBackground -> "#2D2D30"
    | BuildInColor.StatusPanelForeground -> "#E6E6E3"
    | BuildInColor.TabBackground -> "#1E1E1E"
    | _                    -> "#E6E6A3"
    |> Color.Parse
    |> SolidColorBrush


//let defaultFontFamily = 
//    new FontFamily("Consolas, 'Courier New', monospace")

// 推荐使用 'Fira Code' 或你喜欢的其他字体，并在其后添加回退字体
let defaultFontFamily = 
    new FontFamily("Fira Code, 'JetBrains Mono', 'Cascadia Code', 'Consolas', monospace")

let defaultFontSize = 14.0

let defaultFontSizeSmall = 12.0

let defaultCheckboxSize = 12.0

let txtFontSize__TextBlock fontsize text = 
    new TextBlock(
        Text = text,
        Foreground = color__Brush BuildInColor.DefaultForeground,
        FontFamily = defaultFontFamily,
        FontSize = fontsize)

let txt__TextBlock text = 
    new TextBlock(
        Text = text,
        Foreground = color__Brush BuildInColor.DefaultForeground,
        FontFamily = defaultFontFamily,
        FontSize = defaultFontSize)

let txt__Button text = 
    new Button(Content = text, Width = 60.0)

let control__withBorder c = 
    new Border(
        Background = color__Brush BuildInColor.DefaultBackground,
        CornerRadius = CornerRadius(4.0),
        Child = c)

let copyToClipboard
    output
    this text =

    task {
        match TopLevel.GetTopLevel this with
        | null -> ()
        | tl ->
            match tl.Clipboard with
            | null -> ()
            | cb -> 
                do! cb.SetTextAsync text
                "已复制到剪贴板" |> output
    } |> ignore



