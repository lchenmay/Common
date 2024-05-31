module MauiFsLogics.Pages

open System
open System.Collections.Generic

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Layouts

let renderPage1 (layout:AbsoluteLayout) = 

    layout.Children.Add(new Label(Text = "1"))

    let b1 = new Button(Text = "Next")
    b1.Clicked.Add(fun e -> ())

    layout.Children.Add b1
    
let renderPage2 (layout:AbsoluteLayout) = 
    layout.Children.Add(new Label(Text = "Page 2"))

let renderPage3 (layout:AbsoluteLayout) = 
    layout.Children.Add(new Label(Text = "Page 3"))

let pages = new Stack<string>()

let router (layout:AbsoluteLayout) page = 

    pages.Push page

    layout.Children.Clear()

    match page with
    | "/" -> renderPage1 layout
    | "/2" -> renderPage2 layout
    | "/3" -> renderPage3 layout
    | _ -> pages.Pop() |> ignore
    