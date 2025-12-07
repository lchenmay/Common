module UtilVortice.Common

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

type Ctx = {
w: float32
h: float32
factory: ID2D1Factory
rt: ID2D1RenderTarget }

let pixelformat = Vortice.DCommon.PixelFormat(Format.Unknown, AlphaMode.Ignore)
//let pixelformat_ = PixelFormat.Format32bppPBGRA
let renderProps = 
    RenderTargetProperties(RenderTargetType.Default, PixelFormat.Unknown, 96.0f, 96.0f, RenderTargetUsage.None, FeatureLevel.Default)

let createRenderTarget (control: Control) =
    let factory = D2D1.D2D1CreateFactory<ID2D1Factory1>()
    let hwndProps = HwndRenderTargetProperties(
        Hwnd = control.Handle,
        PresentOptions = PresentOptions.None)
    hwndProps.PixelSize.Width <- control.ClientSize.Width
    hwndProps.PixelSize.Height <- control.ClientSize.Height
    
    factory,factory.CreateHwndRenderTarget(renderProps, hwndProps)

/// Load an image file and convert it into an ID2D1Bitmap
let loadID2D1Bitmap (rt: ID2D1RenderTarget) (path: string) =
    use wic = new Vortice.WIC.IWICImagingFactory()

    use decoder = wic.CreateDecoderFromFileName(path)
    use frame = decoder.GetFrame(uint32 0)

    // Convert pixel format → required for Direct2D
    use converter = wic.CreateFormatConverter()
    converter.Initialize(
        frame,
        PixelFormat.Format32bppPBGRA,
        BitmapDitherType.None,
        null,
        0.0,
        BitmapPaletteType.Custom
    ) |> ignore

    // Create Direct2D bitmap
    rt.CreateBitmapFromWicBitmap(converter)

let drawToWICBitmap (w,h) drawer = 

    use cw = new CodeWrapper("Workstation.Graphics.drawOnImg")
    
    use wicFactory = new Vortice.WIC.IWICImagingFactory()
    let wicbmp = wicFactory.CreateBitmap(w, h, PixelFormat.Format32bppPBGRA, BitmapCreateCacheOption.CacheOnLoad)
    use factory = D2D1.D2D1CreateFactory()
    use rt = factory.CreateWicBitmapRenderTarget(wicbmp, renderProps)
    
    rt.BeginDraw()

    {   w = w |> float32
        h = h |> float32
        factory = factory
        rt = rt } |> drawer

    rt.EndDraw() |> ignore
    
    wicbmp

let WICBitmap__GdiBmp (wicbmp:IWICBitmap) = 
     
    use cw = new CodeWrapper("Workstation.Graphics.WICBitmap__GdiBmp")

    let gdibmp = new System.Drawing.Bitmap(wicbmp.Size.Width,wicbmp.Size.Height)
    let bmpData = 
        gdibmp.LockBits(
            new System.Drawing.Rectangle(0,0,wicbmp.Size.Width,wicbmp.Size.Height),
            System.Drawing.Imaging.ImageLockMode.WriteOnly,
            System.Drawing.Imaging.PixelFormat.Format32bppPArgb)
    wicbmp.CopyPixels(uint32 bmpData.Stride,uint32 (bmpData.Stride * bmpData.Height),bmpData.Scan0)
    gdibmp.UnlockBits(bmpData)

    gdibmp


type LayerContainer = {
ctx: Ctx
layers: ID2D1Bitmap[]
final: ID2D1Bitmap }

let create2LayerContainer ctx = 
    let w,h = int ctx.w,int ctx.h
    {   ctx = ctx
        layers = 
            [|
                ctx.rt.CreateBitmap(SizeI(w,h))
                ctx.rt.CreateBitmap(SizeI(w,h)) |]
        final = ctx.rt.CreateBitmap(SizeI(w,h)) }

let layers__final (layercontainer:LayerContainer) = 

    let rt = layercontainer.ctx.rt
    rt.BeginDraw()
    rt.Clear(Color4(0.1f, 0.1f, 0.1f, 1.0f))    // background

    layercontainer.layers
    |> Array.iter rt.DrawBitmap
    rt.EndDraw() |> ignore

let control__ctx (control: Control) = 
    let factory,rt = createRenderTarget control
    {
        w = control.Width |> float32
        h = control.Height |> float32
        factory = factory
        rt = rt }

type PaintingCanvas() = 
    inherit Panel()

    member val suspend = false with get,set

    override this.WndProc (m: byref<Message>): unit = 
        if this.suspend = false then
            if m.Msg = 0x000F then // WM_PAINT
                base.WndProc (&m)

let htmlcolor__color4 s = 
    let c = System.Drawing.ColorTranslator.FromHtml s
    Color4(float32 c.R / 255.f, float32 c.G / 255.f, float32 c.B / 255.f, float32 c.A / 255.f)
