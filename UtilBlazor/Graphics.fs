module UtilBlazor.Graphics

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
    