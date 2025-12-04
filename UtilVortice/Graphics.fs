module UtilVortice.Graphics

open System
open System.Windows.Forms
open System.Numerics

open Vortice
open Vortice.DXGI
open Vortice.DCommon
open Vortice.Direct2D1
open Vortice.DirectWrite
open Vortice.Mathematics
open Vortice.WIC

open Util.Perf
open Util.GraphicsGeo

let drawText 
    (rt:ID2D1RenderTarget) 
    (fontsize,back:Color4,fore:Color4)
    s (vct2:Vector2) = 

    let factory = Vortice.DirectWrite.DWrite.DWriteCreateFactory()
    let format = factory.CreateTextFormat("Callibri",fontsize)

    let metrics = factory.CreateTextLayout(s,format, 1000.f, 1000.f).Metrics
    let rect = RawRectF(vct2.X,vct2.Y,vct2.X + metrics.Width,vct2.Y + metrics.Height)

    rt.FillRectangle(rect,rt.CreateSolidColorBrush back)
    rt.DrawText(s,format,rect,rt.CreateSolidColorBrush fore)

let drawTextOnChart 
    (rt:ID2D1RenderTarget) 
    padding
    (fontsize,back:Color4,fore:Color4)
    (l,t,r,b)
    (s,corner) = 

    let factory = Vortice.DirectWrite.DWrite.DWriteCreateFactory()
    let format = factory.CreateTextFormat("Courier",fontsize)

    let metrics = factory.CreateTextLayout(s,format, 1000.f, 1000.f).Metrics
    let rect = 
        let w,h = metrics.Width + padding * 2f,metrics.Height + padding * 2f
        match corner with
        | ChartCorner.LeftTop -> RawRectF(l,t,l + w,t + h)
        | ChartCorner.LeftBottom -> RawRectF(l,b - h,l + w,b)
        | ChartCorner.RightTop -> RawRectF(r - w,t,r,t + h)
        | ChartCorner.RightBottom -> RawRectF(r - w,b - h,r,b)
    rt.FillRectangle(rect,rt.CreateSolidColorBrush back)

    let r = RawRectF(rect.Left + padding,rect.Top + padding,rect.Right - padding,rect.Bottom - padding)
    rt.DrawText(s,format,r,rt.CreateSolidColorBrush fore)

let drawTextVerOnChart 
    (rt:ID2D1RenderTarget) 
    padding
    (fontsize,back:Color4,fore:Color4)
    (l,t,r,b)
    (s,y,align) = 

    let factory = Vortice.DirectWrite.DWrite.DWriteCreateFactory()
    let format = factory.CreateTextFormat("Courier",fontsize)

    let metrics = factory.CreateTextLayout(s,format, 1000.f, 1000.f).Metrics
    let rect = 
        let w,h = metrics.Width + padding * 2f,metrics.Height + padding * 2f
        match align with
        | VerAlign.LeftAbove -> RawRectF(l,y - h,l + w,y)
        | VerAlign.LeftCenter -> RawRectF(l,y - 0.5f * h,l + w,y + 0.5f * h)
        | VerAlign.LeftBelow -> RawRectF(l,y,l + w,y + h)
        | VerAlign.RightAbove -> RawRectF(r - w,y - h,r,y)
        | VerAlign.RightCenter -> RawRectF(r - w,y - 0.5f * h,r,y + 0.5f * h)
        | VerAlign.RightBelow -> RawRectF(r - w,y,r,y + h)
    rt.FillRectangle(rect,rt.CreateSolidColorBrush back)

    let r = RawRectF(rect.Left + padding,rect.Top + padding,rect.Right - padding,rect.Bottom - padding)
    rt.DrawText(s,format,r,rt.CreateSolidColorBrush fore)

let drawTextHorOnChart 
    (rt:ID2D1RenderTarget) 
    padding
    (fontsize,back:Color4,fore:Color4)
    (l,t,r,b)
    (s,x,align) = 

    let factory = Vortice.DirectWrite.DWrite.DWriteCreateFactory()
    let format = factory.CreateTextFormat("Courier",fontsize)

    let metrics = factory.CreateTextLayout(s,format, 1000.f, 1000.f).Metrics
    let rect = 
        let w,h = metrics.Width + padding * 2f,metrics.Height + padding * 2f
        match align with
        | HorAlign.TopLeft -> RawRectF(x - w,t,x,t + h)
        | HorAlign.TopCenter -> RawRectF(x - 0.5f * w,t,x + 0.5f * w,t + h)
        | HorAlign.TopRight -> RawRectF(x,t,x + w,t + h)
        | HorAlign.BottomLeft -> RawRectF(x - w,b - h,x,b)
        | HorAlign.BottomCenter -> RawRectF(x - 0.5f * w,b - h,x + 0.5f * w,b)
        | HorAlign.BottomRight -> RawRectF(x,b - h,x + w,b)
    rt.FillRectangle(rect,rt.CreateSolidColorBrush back)

    let r = RawRectF(rect.Left + padding,rect.Top + padding,rect.Right - padding,rect.Bottom - padding)
    rt.DrawText(s,format,r,rt.CreateSolidColorBrush fore)

let drawLineHor
    (rt:ID2D1RenderTarget) 
    cxy
    (p,color:Color4) = 

    let y = p__d cxy.y p
    if y <= cxy.y.dinf && y >= cxy.y.dsup then
        use brush = rt.CreateSolidColorBrush color
        rt.DrawLine(Vector2(cxy.x.dinf,y),Vector2(cxy.x.dsup,y),brush)

let drawLineVer
    (rt:ID2D1RenderTarget) 
    cxy
    (p,color:Color4) = 

    let x = p__d cxy.x p
    if x >= cxy.x.dinf && x <= cxy.x.dsup then
        use brush = rt.CreateSolidColorBrush color
        rt.DrawLine(Vector2(x,cxy.y.dinf),Vector2(x,cxy.y.dsup),brush)

let drawLines
    (rt:ID2D1RenderTarget) 
    (color:Color4)
    lines = 

    use brush = rt.CreateSolidColorBrush color

    lines
    |> Array.iter(fun (x1,y1,x2,y2) ->
        rt.DrawLine(Vector2(x1,y1),Vector2(x2,y2),brush))

let drawLine
    (factory:ID2D1Factory,rt:ID2D1RenderTarget) 
    (lineWidth,color:Color4,dashes:float32[])
    (src,dst) = 

    use brush = rt.CreateSolidColorBrush color

    use ss = 

        if dashes.Length = 0 then
            let stroke = new StrokeStyleProperties(
                StartCap = CapStyle.Flat,
                EndCap = CapStyle.Flat,
                DashCap = CapStyle.Flat,
                LineJoin = LineJoin.Miter)

            factory.CreateStrokeStyle(stroke)        
        else
            let stroke = new StrokeStyleProperties(
                StartCap = CapStyle.Flat,
                EndCap = CapStyle.Flat,
                DashCap = CapStyle.Flat,
                LineJoin = LineJoin.Miter,
                DashStyle = DashStyle.Custom)

            factory.CreateStrokeStyle(stroke,dashes)        

    rt.DrawLine(src,dst,brush,lineWidth,ss)

let drawPath
    (factory:ID2D1Factory,rt:ID2D1RenderTarget) 
    (lineWidth,color:Color4,dashes:float32[])
    (pts:Vector2[]) = 

    use cw = new CodeWrapper("Workstation.Graphics.drawPath")

    use brush = rt.CreateSolidColorBrush color

    use ss = 

        if dashes.Length = 0 then
            let stroke = new StrokeStyleProperties(
                StartCap = CapStyle.Flat,
                EndCap = CapStyle.Flat,
                DashCap = CapStyle.Flat,
                LineJoin = LineJoin.Miter)

            factory.CreateStrokeStyle(stroke)        
        else
            let stroke = new StrokeStyleProperties(
                StartCap = CapStyle.Flat,
                EndCap = CapStyle.Flat,
                DashCap = CapStyle.Flat,
                LineJoin = LineJoin.Miter,
                DashStyle = DashStyle.Custom)

            factory.CreateStrokeStyle(stroke,dashes)        

    [| 1 .. pts.Length - 1 |]
    |> Array.iter(fun i -> 
        let src = pts[i - 1]
        let dst = pts[i]
        rt.DrawLine(src,dst,brush,lineWidth,ss))

let drawPathGeometry
    (factory:ID2D1Factory,rt:ID2D1RenderTarget) 
    (lineWidth,color:Color4,dashes:float32[])
    (pts:Vector2[]) = 

    use cw = new CodeWrapper("Workstation.Graphics.drawPathGeometry")

    use path = factory.CreatePathGeometry()

    use sink = path.Open()
    sink.BeginFigure(pts[0],FigureBegin.Filled)
    [| 1 .. pts.Length - 1 |]
    |> Array.iter(fun i -> sink.AddLine pts[i])
    sink.EndFigure(FigureEnd.Open)
    sink.Close()

    use brush = rt.CreateSolidColorBrush color

    use ss = 

        if dashes.Length = 0 then
            let stroke = new StrokeStyleProperties(
                StartCap = CapStyle.Flat,
                EndCap = CapStyle.Flat,
                DashCap = CapStyle.Flat,
                LineJoin = LineJoin.Miter)

            factory.CreateStrokeStyle(stroke)        
        else
            let stroke = new StrokeStyleProperties(
                StartCap = CapStyle.Flat,
                EndCap = CapStyle.Flat,
                DashCap = CapStyle.Flat,
                LineJoin = LineJoin.Miter,
                DashStyle = DashStyle.Custom)

            factory.CreateStrokeStyle(stroke,dashes)        

    rt.DrawGeometry(path,brush,lineWidth,ss)


