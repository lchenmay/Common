module UtilMaui.SPA

open System
open System.Collections.Generic

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Layouts

let pages = new Stack<string>()

let route (layout:AbsoluteLayout) page =

    pages.Push page

    layout.Children.Clear()

    match page with
    | "/" -> layout.Children.Add(new Label(Text = "OK"))
    | "abc" -> ()
    | "def" -> ()
    | _ -> pages.Pop() |> ignore


