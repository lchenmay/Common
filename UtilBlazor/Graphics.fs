module UtilBlazor.Graphics

open System
open System.Threading.Tasks

open Blazor.Extensions
open Blazor.Extensions.Canvas
open Blazor.Extensions.Canvas.Canvas2D

let essential = 
    """
   
    function renderJS(timeStamp) {
        theInstance.invokeMethodAsync('RenderInBlazor', timeStamp);
        window.requestAnimationFrame(renderJS);
    }

    function resizeCanvasToFitWindow() {
        var holder = document.getElementById('canvasHolder');
        var canvas = holder.querySelector('canvas');
        if (canvas) {
            canvas.width = window.innerWidth;
            canvas.height = window.innerHeight;
            theInstance.invokeMethodAsync('ResizeInBlazor', canvas.width, canvas.height);
        }
    }

    window.initRenderJS = (instance) => {
        window.theInstance = instance;
        window.addEventListener("resize", resizeCanvasToFitWindow);
        resizeCanvasToFitWindow();
        window.requestAnimationFrame(renderJS);
    };

    """

type Rect = {
l: float
t: float
w: float
h: float }

let empty__Rect() = {
    l = 0.0
    t = 0.0
    w = 0.0
    h = 0.0 }

let palette = 
    [|  "Red"
        "Green"
        "Blue"
        "Yellow"
        "Orange"
        "Purple"
        "Pink"
        "Cyan"
        "Crimson"
        "MediumVioletRed"
        "DarkMagenta"
        "Indigo"
        "DarkSlateBlue"
        "MediumBlue"
        "MidnightBlue"
        "Navy"
        "DarkSlateGray"
        "DarkGreen"
        "DarkOliveGreen"
        "SaddleBrown"
        "Sienna"
        "Maroon"
        "Gray"
        "DimGray"
        "Black" |]


let uint32__color (u:UInt32) = 
    let s = 
        u
        |> BitConverter.GetBytes
        |> Array.map(fun b -> 
            b.ToString("X").PadLeft(2,'0'))
        |> String.Concat

    "#" + s

let rand__color() = 

    let r = new Random()

    let bin = Array.zeroCreate 3
    r.NextBytes bin
    
    let s = 
        bin
        |> Array.map(fun b -> 
            b.ToString("X").PadLeft(2,'0'))
        |> String.Concat

    "#" + s

let drawText (ctx:Canvas2DContext) (x,y) s = 
    ctx.FillTextAsync(s, x, y)

let drawLine (ctx:Canvas2DContext) (l,t,r,b) = 
    task{
        do! ctx.BeginPathAsync()
        do! ctx.MoveToAsync(l,t)
        do! ctx.LineToAsync(r,b)
        do! ctx.ClosePathAsync()
        do! ctx.StrokeAsync()
    }

let drawPath (ctx:Canvas2DContext) (points:(float * float)[]) = 
    task{
        if points.Length > 0 then
            do! ctx.BeginPathAsync()

            let mutable p = points[0]
            do! ctx.MoveToAsync(fst p,snd p)
            let! tasks = 
                [| 1 .. points.Length - 1|]
                |> Array.map(fun i ->
                    let next = points[i]
                    let t = ctx.LineToAsync(fst next,snd next)
                    p <- next
                    t)
                |> Task.WhenAll
            do! ctx.ClosePathAsync()
            do! ctx.StrokeAsync()
    }

let circle = 2.0 * System.Math.PI    

let drawCircle (ctx:Canvas2DContext) color r (x,y) = 
    task{
        do! ctx.BeginPathAsync()
        do! ctx.ArcAsync(x, y, r, 0, circle,false)
        do! ctx.SetFillStyleAsync color
        do! ctx.FillAsync()
        do! ctx.StrokeAsync()
    }
