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

/// Vortice.Mathematics.Color4 → SKColor（用于 Palette 迁移）
let color4__SKColor (r, g, b) =
    SKColor(
        byte (r * 255.f |> min 255.f |> max 0.f),
        byte (g * 255.f |> min 255.f |> max 0.f),
        byte (b * 255.f |> min 255.f |> max 0.f))

let color4a__SKColor (r, g, b, a) =
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

/// SKBitmap → Avalonia WriteableBitmap（用于 Image 控件显示）
let SKBitmap__WriteableBitmap (skbmp: SKBitmap) =
    let avbmp = new Avalonia.Media.Imaging.WriteableBitmap(
        Avalonia.PixelSize(skbmp.Width, skbmp.Height),
        Avalonia.Vector(96.0, 96.0),
        Avalonia.Platform.PixelFormat.Bgra8888,
        Avalonia.Platform.AlphaFormat.Premul)

    use lck = avbmp.Lock()
    let ptr = skbmp.GetPixels()
    let len = skbmp.ByteCount
    System.Runtime.InteropServices.Marshal.Copy(ptr, lck.Address, len)

    avbmp

/// SKBitmap → GDI+ Bitmap（用于兼容旧代码）
let SKBitmap__GdiBmp (skbmp: SKBitmap) =
    use cw = new CodeWrapper("UtilAvalonia.Skia.SKBitmap__GdiBmp")

    let gdibmp = new System.Drawing.Bitmap(skbmp.Width, skbmp.Height)
    let data = gdibmp.LockBits(
        System.Drawing.Rectangle(0, 0, skbmp.Width, skbmp.Height),
        System.Drawing.Imaging.ImageLockMode.WriteOnly,
        System.Drawing.Imaging.PixelFormat.Format32bppPArgb)
    System.Runtime.InteropServices.Marshal.Copy(skbmp.GetPixels(), data.Scan0, skbmp.ByteCount)
    gdibmp.UnlockBits(data)

    gdibmp

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
