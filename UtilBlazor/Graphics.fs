module UtilBlazor.Graphics

open System

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

let circle = 2.0 * System.Math.PI    

let drawCircle (ctx:Canvas2DContext) color r (x,y) = 
    task{
        do! ctx.BeginPathAsync()
        do! ctx.ArcAsync(x, y, r, 0, circle,false)
        do! ctx.SetFillStyleAsync color
        do! ctx.FillAsync()
        do! ctx.StrokeAsync()
    }
