module UtilMaui.Layout

open System
open System.Collections.Generic

open Util.Json

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Layouts

let absLayout item spec = 
    let l,t,w,h = spec
    AbsoluteLayout.SetLayoutFlags(item, AbsoluteLayoutFlags.All)
    AbsoluteLayout.SetLayoutBounds(item, Rect(l,t,w,h))

let desc = 
    """
{
    "dockSeq": [
        {   "header": { "direction": "Top", "qty": "20%" } },
        {
            "side": {
                "direction": "Right",
                "qty": "123",
                "dockSeq": [
                    { "server": { "direction": "Top" } },
                    { "palette": { "direction": "Top" } },
                    { "chatSession": { "direction": "Fill" } }
                ]}
        },
        {   "canvas": { "direction": "Fill" } }
    ]
}
    """

let test = 
    """
    {
        "rows":"",
        "cols":[],
        "cells":[],
    }
    """

let tes = 
    [|  ("600,400","100")
        ("","50")
        ("","50") |]

let jsonstr__layout t = 

    let json = str__root t
    
    let grid = new Grid()

    ((json__tryFindByName json "rows").Value |> json__aryo).Value
    |> Array.map(fun json -> new RowDefinition())
    |> Array.iter grid.RowDefinitions.Add

    ((json__tryFindByName json "cols").Value |> json__aryo).Value
    |> Array.map(fun json -> new ColumnDefinition())
    |> Array.iter grid.ColumnDefinitions.Add

    let cells = ((json__tryFindByName json "cells").Value |> json__aryo).Value


    // Layout
    [|  new RowDefinition(Height = new GridLength(2, GridUnitType.Star))
        new RowDefinition()
        new RowDefinition(Height = new GridLength(100)) |]
    |> Array.iter grid.RowDefinitions.Add
    
    [|  new ColumnDefinition()
        new ColumnDefinition() |]
    |> Array.iter grid.ColumnDefinitions.Add


    grid