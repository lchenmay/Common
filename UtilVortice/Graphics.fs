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

let measureText 
    (rt:ID2D1RenderTarget) 
    fontsize s = 

    let factory = Vortice.DirectWrite.DWrite.DWriteCreateFactory()
    let format = factory.CreateTextFormat("Callibri",fontsize)

    factory.CreateTextLayout(s,format, 1000.f, 1000.f).Metrics

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

// ============================================================================
// 坐标轴刻度绘制（像素驱动 + 横轴自适应防重叠 + 最少有效数字）
// ============================================================================

/// 在图表矩形 (l,t,r,b) 上绘制 scale 对应的坐标轴刻度线 + 文字标注。
/// scale.attach 决定轴在哪条边（Left/Right/Top/Bottom），
/// 横轴(Top/Bottom)会按实测文字宽度自适应倍增文字步进以防重叠。
let drawScale
    (rt: ID2D1RenderTarget)
    (l: float32) (t: float32) (r: float32) (b: float32)
    (scale: Scale)
    (fontSize: float32)
    (tickColor: Color4)
    (textBack: Color4)
    (textFore: Color4) =

    let ticks = genTicks scale
    let small = scale.pincrementScaleMin
    let maj   = scale.pincrementScaleMaj
    let txt   = scale.pincrementText
    let c     = scale.coord

    if small <= 0.0 || ticks.minors.IsEmpty then () else

    let isX = scale.attach = Top || scale.attach = Bottom

    // ---- 横轴自适应文字步进 ----
    let textStep =
        let defaultEvery = int (txt / small)
        if isX && List.length ticks.texts > 1 then
            let factory = Vortice.DirectWrite.DWrite.DWriteCreateFactory()
            let format = factory.CreateTextFormat("Callibri", fontSize)
            let measure s =
                factory.CreateTextLayout(s, format, 1000.f, 1000.f).Metrics.Width
            let maxW = ticks.texts |> List.map (fun (_,s) -> measure s) |> List.max
            let axisPixels =
                match scale.attach with
                | Left | Right -> abs(c.dinf - c.dsup)  // vertical → pixel height
                | Top  | Bottom -> abs(c.dsup - c.dinf)  // horizontal → pixel width
            let pixPerSmall = float axisPixels / ((c.psup - c.pinf) / small)
            let mutable te = defaultEvery
            while float32(float te) * float32(pixPerSmall) < maxW * 1.2f do
                te <- te * 2
            te
        else defaultEvery

    let drawnTextStep = small * float textStep

    // ---- 绘制刻度线 ----
    use brush = rt.CreateSolidColorBrush tickColor

    for v in ticks.minors do
        let pix = p__d c v
        let isDrawnText = isMultipleOf v drawnTextStep && isMultipleOf v txt
        let tickLen =
            if isDrawnText then 10.0f
            elif isMultipleOf v maj then 7.0f
            else 4.0f
        match scale.attach with
        | Left   -> rt.DrawLine(Vector2(l - tickLen, pix), Vector2(l, pix), brush)
        | Right  -> rt.DrawLine(Vector2(r, pix), Vector2(r + tickLen, pix), brush)
        | Top    -> rt.DrawLine(Vector2(pix, t - tickLen), Vector2(pix, t), brush)
        | Bottom -> rt.DrawLine(Vector2(pix, b), Vector2(pix, b + tickLen), brush)

    // ---- 绘制文字标注 ----
    let drFactory = Vortice.DirectWrite.DWrite.DWriteCreateFactory()
    let drFormat = drFactory.CreateTextFormat("Calibri", fontSize)

    for v, s in ticks.texts do
        if isMultipleOf v drawnTextStep then
            let pix = p__d c v
            let metrics = drFactory.CreateTextLayout(s, drFormat, 1000.f, 1000.f).Metrics
            let tw, th = metrics.Width, metrics.Height
            let pad = 3.0f
            match scale.attach with
            | Left ->
                let rect = RawRectF(l - tw - pad * 2f - 12f, pix - th * 0.5f - pad, l - 2f, pix + th * 0.5f + pad)
                rt.FillRectangle(rect, rt.CreateSolidColorBrush textBack)
                rt.DrawText(s, drFormat, RawRectF(rect.Left + pad, rect.Top + pad, rect.Right - pad, rect.Bottom - pad), rt.CreateSolidColorBrush textFore)
            | Right ->
                let rect = RawRectF(r + 2f, pix - th * 0.5f - pad, r + tw + pad * 2f + 12f, pix + th * 0.5f + pad)
                rt.FillRectangle(rect, rt.CreateSolidColorBrush textBack)
                rt.DrawText(s, drFormat, RawRectF(rect.Left + pad, rect.Top + pad, rect.Right - pad, rect.Bottom - pad), rt.CreateSolidColorBrush textFore)
            | Top ->
                let rect = RawRectF(pix - tw * 0.5f - pad, t - th - pad * 2f - 10f, pix + tw * 0.5f + pad, t - 2f)
                rt.FillRectangle(rect, rt.CreateSolidColorBrush textBack)
                rt.DrawText(s, drFormat, RawRectF(rect.Left + pad, rect.Top + pad, rect.Right - pad, rect.Bottom - pad), rt.CreateSolidColorBrush textFore)
            | Bottom ->
                let rect = RawRectF(pix - tw * 0.5f - pad, b + 2f, pix + tw * 0.5f + pad, b + th + pad * 2f + 10f)
                rt.FillRectangle(rect, rt.CreateSolidColorBrush textBack)
                rt.DrawText(s, drFormat, RawRectF(rect.Left + pad, rect.Top + pad, rect.Right - pad, rect.Bottom - pad), rt.CreateSolidColorBrush textFore)

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


