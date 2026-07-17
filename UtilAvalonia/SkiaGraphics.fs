module UtilAvalonia.SkiaGraphics

open System
open SkiaSharp

open Util.Perf
open Util.GraphicsGeo
open UtilAvalonia.SkiaCommon

// ============================================================================
// 文本绘制（SkiaSharp 3.x 使用 SKFont 替代 SKPaint 上的字体属性）
// ============================================================================

/// 创建 Calibri SKFont
let private calibriFont (fontSize: float32) =
    new SKFont(SKTypeface.FromFamilyName("Calibri"), fontSize)

/// 创建 Courier SKFont
let private courierFont (fontSize: float32) =
    new SKFont(SKTypeface.FromFamilyName("Courier"), fontSize)

/// 测量文本尺寸——替代 UtilVortice.Graphics.measureText
let measureText (fontSize: float32) (s: string) =
    use font = calibriFont fontSize
    let bounds = font.MeasureText(s)
    {| Width = bounds; Height = fontSize |}

/// 绘制文本（带背景）——替代 UtilVortice.Graphics.drawText
let drawText
    (canvas: SKCanvas)
    (fontSize: float32, back: SKColor, fore: SKColor)
    (s: string)
    (x: float32, y: float32) =

    use font = calibriFont fontSize
    let textWidth = font.MeasureText(s)
    let rect = SKRect(x, y, x + textWidth, y + fontSize)

    use paint = new SKPaint(Color = back, Style = SKPaintStyle.Fill, IsAntialias = true)
    canvas.DrawRect(rect, paint)

    paint.Color <- fore
    canvas.DrawText(s, x, y + fontSize * 0.8f, SKTextAlign.Left, font, paint)

/// 在图表内绘制文本（支持四角定位）——替代 UtilVortice.Graphics.drawTextOnChart
let drawTextOnChart
    (canvas: SKCanvas)
    (padding: float32)
    (fontSize: float32, back: SKColor, fore: SKColor)
    (l: float32, t: float32, r: float32, b: float32)
    (s: string, corner: ChartCorner) =

    use font = courierFont fontSize
    let textWidth = font.MeasureText(s)
    let w = textWidth + padding * 2.f
    let h = fontSize + padding * 2.f

    let rect =
        match corner with
        | LeftTop -> SKRect(l, t, l + w, t + h)
        | LeftBottom -> SKRect(l, b - h, l + w, b)
        | RightTop -> SKRect(r - w, t, r, t + h)
        | RightBottom -> SKRect(r - w, b - h, r, b)

    use paint = new SKPaint(Color = back, Style = SKPaintStyle.Fill, IsAntialias = true)
    canvas.DrawRect(rect, paint)

    paint.Color <- fore
    canvas.DrawText(s, rect.Left + padding, rect.Top + padding + fontSize * 0.8f, SKTextAlign.Left, font, paint)

/// 在图表内绘制垂直对齐文本——替代 UtilVortice.Graphics.drawTextVerOnChart
let drawTextVerOnChart
    (canvas: SKCanvas)
    (padding: float32)
    (fontSize: float32, back: SKColor, fore: SKColor)
    (l: float32, t: float32, r: float32, b: float32)
    (s: string, y: float32, align: VerAlign) =

    use font = courierFont fontSize
    let textWidth = font.MeasureText(s)
    let w = textWidth + padding * 2.f
    let h = fontSize + padding * 2.f

    let rect =
        match align with
        | LeftAbove -> SKRect(l, y - h, l + w, y)
        | LeftCenter -> SKRect(l, y - 0.5f * h, l + w, y + 0.5f * h)
        | LeftBelow -> SKRect(l, y, l + w, y + h)
        | RightAbove -> SKRect(r - w, y - h, r, y)
        | RightCenter -> SKRect(r - w, y - 0.5f * h, r, y + 0.5f * h)
        | RightBelow -> SKRect(r - w, y, r, y + h)

    use paint = new SKPaint(Color = back, Style = SKPaintStyle.Fill, IsAntialias = true)
    canvas.DrawRect(rect, paint)

    paint.Color <- fore
    canvas.DrawText(s, rect.Left + padding, rect.Top + padding + fontSize * 0.8f, SKTextAlign.Left, font, paint)

/// 在图表内绘制水平对齐文本——替代 UtilVortice.Graphics.drawTextHorOnChart
let drawTextHorOnChart
    (canvas: SKCanvas)
    (padding: float32)
    (fontSize: float32, back: SKColor, fore: SKColor)
    (l: float32, t: float32, r: float32, b: float32)
    (s: string, x: float32, align: HorAlign) =

    use font = courierFont fontSize
    let textWidth = font.MeasureText(s)
    let w = textWidth + padding * 2.f
    let h = fontSize + padding * 2.f

    let rect =
        match align with
        | TopLeft -> SKRect(x - w, t, x, t + h)
        | TopCenter -> SKRect(x - 0.5f * w, t, x + 0.5f * w, t + h)
        | TopRight -> SKRect(x, t, x + w, t + h)
        | BottomLeft -> SKRect(x - w, b - h, x, b)
        | BottomCenter -> SKRect(x - 0.5f * w, b - h, x + 0.5f * w, b)
        | BottomRight -> SKRect(x, b - h, x + w, b)

    use paint = new SKPaint(Color = back, Style = SKPaintStyle.Fill, IsAntialias = true)
    canvas.DrawRect(rect, paint)

    paint.Color <- fore
    canvas.DrawText(s, rect.Left + padding, rect.Top + padding + fontSize * 0.8f, SKTextAlign.Left, font, paint)

// ============================================================================
// 线条绘制
// ============================================================================

/// 水平线（自动裁剪到 Coord 范围）——替代 UtilVortice.Graphics.drawLineHor
let drawLineHor
    (canvas: SKCanvas)
    (cxy: CoordXY)
    (p: float, color: SKColor) =

    let y = p__d cxy.y p
    if y <= cxy.y.dinf && y >= cxy.y.dsup then
        use paint = new SKPaint(Color = color, StrokeWidth = 1f, IsAntialias = true)
        canvas.DrawLine(cxy.x.dinf, y, cxy.x.dsup, y, paint)

/// 垂直线（自动裁剪到 Coord 范围）——替代 UtilVortice.Graphics.drawLineVer
let drawLineVer
    (canvas: SKCanvas)
    (cxy: CoordXY)
    (p: float, color: SKColor) =

    let x = p__d cxy.x p
    if x >= cxy.x.dinf && x <= cxy.x.dsup then
        use paint = new SKPaint(Color = color, StrokeWidth = 1f, IsAntialias = true)
        canvas.DrawLine(x, cxy.y.dinf, x, cxy.y.dsup, paint)

/// 批量线段——替代 UtilVortice.Graphics.drawLines
let drawLines
    (canvas: SKCanvas)
    (color: SKColor)
    (lines: (float32 * float32 * float32 * float32)[]) =

    use paint = new SKPaint(Color = color, StrokeWidth = 1f, IsAntialias = true)
    lines
    |> Array.iter (fun (x1, y1, x2, y2) ->
        canvas.DrawLine(x1, y1, x2, y2, paint))

/// 单线段（支持线宽+虚线）——替代 UtilVortice.Graphics.drawLine
let drawLine
    (canvas: SKCanvas)
    (lineWidth: float32, color: SKColor, dashes: float32[])
    (src: SKPoint, dst: SKPoint) =

    use paint = new SKPaint(
        Color = color,
        StrokeWidth = lineWidth,
        IsAntialias = true,
        Style = SKPaintStyle.Stroke,
        StrokeCap = SKStrokeCap.Butt,
        StrokeJoin = SKStrokeJoin.Miter)

    if dashes.Length > 0 then
        paint.PathEffect <- SKPathEffect.CreateDash(dashes, 0f)

    canvas.DrawLine(src, dst, paint)

/// 折线路径——替代 UtilVortice.Graphics.drawPath
let drawPath
    (canvas: SKCanvas)
    (lineWidth: float32, color: SKColor, dashes: float32[])
    (pts: SKPoint[]) =

    use cw = new CodeWrapper("UtilAvalonia.Skia.drawPath")

    use paint = new SKPaint(
        Color = color,
        StrokeWidth = lineWidth,
        IsAntialias = true,
        Style = SKPaintStyle.Stroke,
        StrokeCap = SKStrokeCap.Butt,
        StrokeJoin = SKStrokeJoin.Miter)

    if dashes.Length > 0 then
        paint.PathEffect <- SKPathEffect.CreateDash(dashes, 0f)

    for i in 1 .. pts.Length - 1 do
        canvas.DrawLine(pts[i - 1], pts[i], paint)

/// 路径几何（使用 SKPath）——替代 UtilVortice.Graphics.drawPathGeometry
let drawPathGeometry
    (canvas: SKCanvas)
    (lineWidth: float32, color: SKColor, dashes: float32[])
    (pts: SKPoint[]) =

    use cw = new CodeWrapper("UtilAvalonia.Skia.drawPathGeometry")

    use path = new SKPath()
    path.MoveTo(pts[0])
    for i in 1 .. pts.Length - 1 do
        path.LineTo(pts[i])

    use paint = new SKPaint(
        Color = color,
        StrokeWidth = lineWidth,
        IsAntialias = true,
        Style = SKPaintStyle.Stroke,
        StrokeCap = SKStrokeCap.Butt,
        StrokeJoin = SKStrokeJoin.Miter)

    if dashes.Length > 0 then
        paint.PathEffect <- SKPathEffect.CreateDash(dashes, 0f)

    canvas.DrawPath(path, paint)

// ============================================================================
// 实用函数
// ============================================================================

/// 创建纯色 Fill paint
let fillPaint (color: SKColor) =
    new SKPaint(Color = color, Style = SKPaintStyle.Fill, IsAntialias = true)

/// 创建纯色 Stroke paint
let strokePaint (color: SKColor) (strokeWidth: float32) =
    new SKPaint(
        Color = color,
        Style = SKPaintStyle.Stroke,
        StrokeWidth = strokeWidth,
        IsAntialias = true,
        StrokeCap = SKStrokeCap.Butt)

/// 创建带虚线的 Stroke paint
let dashedPaint (color: SKColor) (strokeWidth: float32) (dashes: float32[]) =
    let p = strokePaint color strokeWidth
    if dashes.Length > 0 then
        p.PathEffect <- SKPathEffect.CreateDash(dashes, 0f)
    p

// ============================================================================
// 坐标轴刻度绘制（像素驱动 + 横轴自适应防重叠 + 最少有效数字）
// ============================================================================

/// 在图表矩形 (l,t,r,b) 上绘制 scale 对应的坐标轴刻度线 + 文字标注。
/// scale.attach 决定轴在哪条边（Left/Right/Top/Bottom），
/// 横轴(Top/Bottom)会按实测文字宽度自适应倍增文字步进以防重叠。
let drawScale
    (canvas: SKCanvas)
    (l: float32) (t: float32) (r: float32) (b: float32)
    (scale: Scale)
    (fontSize: float32)
    (tickColor: SKColor)
    (textBack: SKColor)
    (textFore: SKColor) =

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
            use font = calibriFont fontSize
            let measure (s: string) = font.MeasureText(s)
            let maxW = ticks.texts |> List.map (fun (_,s) -> measure s) |> List.max
            let axisPixels =
                match scale.attach with
                | Left | Right -> abs(c.dinf - c.dsup)
                | Top  | Bottom -> abs(c.dsup - c.dinf)
            let pixPerSmall = float axisPixels / ((c.psup - c.pinf) / small)
            let mutable te = defaultEvery
            while float32(float te) * float32(pixPerSmall) < maxW * 1.2f do
                te <- te * 2
            te
        else defaultEvery

    let drawnTextStep = small * float textStep

    // ---- 绘制刻度线 ----
    use linePaint = new SKPaint(
        Color = tickColor,
        StrokeWidth = 1f,
        IsAntialias = true,
        Style = SKPaintStyle.Stroke)

    for v in ticks.minors do
        let pix = p__d c v
        let isDrawnText = isMultipleOf v drawnTextStep && isMultipleOf v txt
        let tickLen =
            if isDrawnText then 10.0f
            elif isMultipleOf v maj then 7.0f
            else 4.0f
        match scale.attach with
        | Left   -> canvas.DrawLine(l - tickLen, pix, l, pix, linePaint)
        | Right  -> canvas.DrawLine(r, pix, r + tickLen, pix, linePaint)
        | Top    -> canvas.DrawLine(pix, t - tickLen, pix, t, linePaint)
        | Bottom -> canvas.DrawLine(pix, b, pix, b + tickLen, linePaint)

    // ---- 绘制文字标注 ----
    use font = calibriFont fontSize

    for v, s in ticks.texts do
        if isMultipleOf v drawnTextStep then
            let pix = p__d c v
            let tw = font.MeasureText(s)
            let th = fontSize
            let pad = 3.0f
            let rect =
                match scale.attach with
                | Left   -> SKRect(l - tw - pad * 2f - 12f, pix - th * 0.5f - pad, l - 2f, pix + th * 0.5f + pad)
                | Right  -> SKRect(r + 2f, pix - th * 0.5f - pad, r + tw + pad * 2f + 12f, pix + th * 0.5f + pad)
                | Top    -> SKRect(pix - tw * 0.5f - pad, t - th - pad * 2f - 10f, pix + tw * 0.5f + pad, t - 2f)
                | Bottom -> SKRect(pix - tw * 0.5f - pad, b + 2f, pix + tw * 0.5f + pad, b + th + pad * 2f + 10f)
            use bgPaint = new SKPaint(Color = textBack, Style = SKPaintStyle.Fill, IsAntialias = true)
            canvas.DrawRect(rect, bgPaint)
            use fgPaint = new SKPaint(Color = textFore, IsAntialias = true)
            canvas.DrawText(s, rect.Left + pad, rect.Top + pad + th * 0.8f, SKTextAlign.Left, font, fgPaint)

/// SKPoint 数组 → (float32*float32*float32*float32) 线段数组（用于 drawLines）
let pts__lines (pts: SKPoint[]) =
    [| for i in 1 .. pts.Length - 1 ->
        pts[i - 1].X, pts[i - 1].Y, pts[i].X, pts[i].Y |]
