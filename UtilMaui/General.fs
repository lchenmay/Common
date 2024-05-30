module UtilMaui.General

open System
open System.Collections.Generic

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls

let float32__int32: float32 -> int = 
    decimal >> Math.Round >> int

let float32__int64: float32 -> int64 = 
    decimal >> Math.Round >> int64


let tea__points (e:TouchEventArgs) = 
    e.Touches
    |> Array.map(fun p -> p.X,p.Y)

let tea__desc (e:TouchEventArgs) = 
    e
    |> tea__points
    |> Array.map(fun (x,y) -> "(" + x.ToString("0") + "," + y.ToString("0") + ")")
    |> String.concat " -> "

let appendControl (children:IList<IView>) c =
    children.Add c
    c

