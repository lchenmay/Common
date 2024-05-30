module UtilMaui.Skia

open System
open System.Collections.Generic

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls

open SkiaSharp
open SkiaSharp.Views.Maui
open SkiaSharp.Views.Maui.Controls


let skPoint__xyFloat32 (p:SKPoint) = 
    p.X,p.Y

let skPoint__xyFloat (p:SKPoint) = 
    float p.X,float p.Y

let skPoint__Point (p:SKPoint) = 
    new Point(float p.X, float p.Y)

let Rect__skRect (rect:Rect) = 
    new SKRect(float32 rect.Left,float32 rect.Top,float32 rect.Right,float32 rect.Bottom)

let Rect__skRectRound (rect:Rect,radius) = 
    let r = new SKRect(float32 rect.Left,float32 rect.Top,float32 rect.Right,float32 rect.Bottom)
    new SKRoundRect(r,radius)

let skPaintSurfaceE__info (e:SKPaintSurfaceEventArgs) = 
    let canvas = e.Surface.Canvas
    let w = e.Info.Width
    let h = e.Info.Height
    canvas,w,h
