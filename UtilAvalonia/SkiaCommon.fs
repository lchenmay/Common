module UtilAvalonia.SkiaCommon

open System
open SkiaSharp

open Util.Perf
open Util.GraphicsGeo

/// 渲染上下文——替代 UtilVortice.Common.Ctx
/// 用 SKCanvas + SKSurface 替代 ID2D1RenderTarget + ID2D1Factory
type Ctx = {
    w: float32
    h: float32
    surface: SKSurface
    canvas: SKCanvas }

/// HTML 颜色字符串 → SKColor（替代 htmlcolor__color4）
let htmlcolor__SKColor (s: string) =
    let c = System.Drawing.ColorTranslator.FromHtml s
    SKColor(c.R, c.G, c.B, c.A)

/// Vortice.Mathematics.Color4 (r,g,b) → SKColor（用于 Palette 迁移）
let color4__SKColor (r: float32, g: float32, b: float32) =
    SKColor(
        byte (r * 255.f |> min 255.f |> max 0.f),
        byte (g * 255.f |> min 255.f |> max 0.f),
        byte (b * 255.f |> min 255.f |> max 0.f))

/// Vortice.Mathematics.Color4 (r,g,b,a) → SKColor
let color4a__SKColor (r: float32, g: float32, b: float32, a: float32) =
    SKColor(
        byte (r * 255.f |> min 255.f |> max 0.f),
        byte (g * 255.f |> min 255.f |> max 0.f),
        byte (b * 255.f |> min 255.f |> max 0.f),
        byte (a * 255.f |> min 255.f |> max 0.f))

/// 离屏渲染到 SKBitmap——替代 drawToWICBitmap
let drawToSKBitmap (w: uint32) (h: uint32) (drawer: Ctx -> unit) =
    use cw = new CodeWrapper("UtilAvalonia.Skia.drawToSKBitmap")

    let info = SKImageInfo(int w, int h)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas

    let ctx = {
        w = float32 w
        h = float32 h
        surface = surface
        canvas = canvas }

    drawer ctx

    canvas.Flush()

    use img = surface.Snapshot()
    SKBitmap.FromImage(img)

/// 将 SKBitmap 像素拷贝到目标 IntPtr（用于 WriteableBitmap Lock）
let private copyPixelsToPtr (skbmp: SKBitmap) (dstPtr: nativeint) =
    let pixels = skbmp.GetPixels()
    let byteCount = skbmp.ByteCount
    let src = System.ReadOnlySpan<byte>(pixels.ToPointer(), byteCount)
    let dst = System.Span<byte>(dstPtr.ToPointer(), byteCount)
    src.CopyTo(dst)

/// SKBitmap → Avalonia WriteableBitmap（用于 Image 控件显示）
let SKBitmap__WriteableBitmap (skbmp: SKBitmap) =
    let avbmp = new Avalonia.Media.Imaging.WriteableBitmap(
        Avalonia.PixelSize(skbmp.Width, skbmp.Height),
        Avalonia.Vector(96.0, 96.0),
        Avalonia.Platform.PixelFormat.Bgra8888,
        Avalonia.Platform.AlphaFormat.Premul)

    use lck = avbmp.Lock()
    copyPixelsToPtr skbmp lck.Address

    avbmp

/// 直接在已存在的 WriteableBitmap 像素缓冲上绘制。
/// 与 drawToSKBitmap + SKBitmap__WriteableBitmap 的组合等价，但：
///   零新建 SKSurface 后备位图、零像素拷贝、零新建 WriteableBitmap。
/// 高频帧（鼠标移动合成前景）必须走这条路径，否则每帧都会分配一张全尺寸位图，
/// 原生内存靠 GC 回收，持续移动时 Gen2 停顿越来越频繁 → 表现为"用一会就越来越卡"。
let drawToWriteableBitmap (wbmp: Avalonia.Media.Imaging.WriteableBitmap) (drawer: Ctx -> unit) =
    use cw = new CodeWrapper("UtilAvalonia.Skia.drawToWriteableBitmap")

    use lck = wbmp.Lock()
    let info = SKImageInfo(lck.Size.Width, lck.Size.Height, SKColorType.Bgra8888, SKAlphaType.Premul)
    // raster-direct：surface 直接以 WriteableBitmap 的像素为后备存储
    use surface = SKSurface.Create(info, lck.Address, lck.RowBytes)
    if isNull (box surface) then
        failwithf "SKSurface.Create(raster-direct) failed: %dx%d rowBytes=%d" info.Width info.Height lck.RowBytes
    let canvas = surface.Canvas

    let ctx = {
        w = float32 info.Width
        h = float32 info.Height
        surface = surface
        canvas = canvas }

    drawer ctx

    canvas.Flush()

/// 从 Avalonia WriteableBitmap 创建渲染上下文
let writeableBitmap__Ctx (wbmp: Avalonia.Media.Imaging.WriteableBitmap) =
    let info = SKImageInfo(wbmp.PixelSize.Width, wbmp.PixelSize.Height)
    let surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    {
        w = float32 wbmp.PixelSize.Width
        h = float32 wbmp.PixelSize.Height
        surface = surface
        canvas = canvas }
