module UtilAvalonia.Comp.Skia.SkiaCommon

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
