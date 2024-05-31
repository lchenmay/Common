module MauiFsLogics.Pages

open System
open System.Collections.Generic

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Layouts

let renders = new Dictionary<string,(string * AbsoluteLayout) -> unit>()

let pages = new Stack<string>()

let routing (layout:AbsoluteLayout) page = 

    pages.Push page

    layout.Children.Clear()

    if renders.ContainsKey page then
        (page, layout)
        |> renders[page]
    else
        pages.Pop()
        |> ignore

renders.Add("1",fun (page,layout:AbsoluteLayout) ->
    layout.Children.Add(new Label(Text = "1"))

    let b = new Button(Text = "Next")
    AbsoluteLayout.SetLayoutBounds(b, Rect(300.0,0.0,100.0,30.0))
    b.Clicked.Add(fun e -> routing layout "2")
    layout.Children.Add b)
    
renders.Add("2",fun (page,layout:AbsoluteLayout) ->
    layout.Children.Add(new Label(Text = "Page 2"))

    let b1 = new Button(Text = "Prev")
    AbsoluteLayout.SetLayoutBounds(b1, Rect(100.0,0.0,100.0,30.0))
    b1.Clicked.Add(fun e -> routing layout "1")
    layout.Children.Add b1

    let b2 = new Button(Text = "Next")
    AbsoluteLayout.SetLayoutBounds(b2, Rect(300.0,0.0,100.0,30.0))
    b2.Clicked.Add(fun e -> routing layout "3")
    layout.Children.Add b2)

renders.Add("3",fun (page,layout:AbsoluteLayout) ->
    layout.Children.Add(new Label(Text = "Page 3"))

    let b = new Button(Text = "Prev")
    AbsoluteLayout.SetLayoutBounds(b, Rect(100.0,0.0,100.0,30.0))
    b.Clicked.Add(fun e -> routing layout "2")
    layout.Children.Add b)


    