module Util.DirectX

open System.Collections.Generic

open SharpDX
open SharpDX.Mathematics.Interop
open SharpDX.Direct2D1
open SharpDX.DXGI

open SharpDX.IO
open SharpDX.WIC



let float_alpha__sharpdx_color(a:float)(r:float,g:float,b:float) =
    new Color(float32 r, float32 g, float32 b, float32 a)

let float__sharpdx_color(r:float,g:float,b:float) = float_alpha__sharpdx_color(1.0)(r,g,b)

let sharpdxcolor__rawcolor4(src:SharpDX.Color) = 
    new RawColor4(float32(float(src.R)/255.0),float32(float(src.G)/255.0),float32(float(src.B)/255.0),float32(float(src.A)/255.0))

let float__rawcolor4(r:float,g:float,b:float) = 
    float_alpha__sharpdx_color(1.0)(r,g,b)
    |> sharpdxcolor__rawcolor4


type GraphicsD2D = 
    {
        width:int;
        height:int;
        wrt:RenderTarget}

let Format = Format.B8G8R8A8_UNorm;
let D2PixelFormat = new SharpDX.Direct2D1.PixelFormat(Format, SharpDX.Direct2D1.AlphaMode.Premultiplied);
let BitmapProps1 = new BitmapProperties1(D2PixelFormat, float32(96), float32(96), BitmapOptions.Target);

// image.PixelFormat = GdiPixelFormat.Format32bppPArgb
//let drawingBmp__SharpDxBmp(rt)(image:System.Drawing.Bitmap) =

//    let rect = new System.Drawing.Rectangle(0, 0, image.Width, image.Height)
//    //let imageData = image.LockBits(rect, System.Drawing.Imaging.ImageLockMode.ReadOnly, image.PixelFormat)
//    let imageData = image.LockBits(rect, System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppPArgb)
//    let dataStream = new DataStream(imageData.Scan0, int64(imageData.Stride * imageData.Height), true, false)

//    let pixelFormat = new SharpDX.Direct2D1.PixelFormat(SharpDX.DXGI.Format.B8G8R8A8_UNorm,SharpDX.Direct2D1.AlphaMode.Premultiplied)
//    let properties = new BitmapProperties(pixelFormat)

//    let res = new Direct2D1.Bitmap(rt, new Size2(image.Width, image.Height), dataStream, imageData.Stride, properties)

//    image.UnlockBits(imageData)

//    res

type ImageCanvas(width,height) =
        
    let wicFactory = new ImagingFactory()
    let d2dFactory = new SharpDX.Direct2D1.Factory()
    let bmp = new Bitmap(wicFactory, width, height, SharpDX.WIC.PixelFormat.Format32bppBGR, BitmapCreateCacheOption.CacheOnLoad)
    let pixelformat = new SharpDX.Direct2D1.PixelFormat(SharpDX.DXGI.Format.Unknown, AlphaMode.Unknown)

    let rtp = new RenderTargetProperties(RenderTargetType.Default, pixelformat, 0.0f, 0.0f, RenderTargetUsage.None, FeatureLevel.Level_DEFAULT)
        
    let wrt = new WicRenderTarget(d2dFactory, bmp, rtp)

    interface System.IDisposable with
        member this.Dispose() = 
            wrt.Dispose()
            d2dFactory.Dispose()
            wicFactory.Dispose()
            bmp.Dispose()

    member this.WRT
        with get() = wrt

    member this.rect() = 
        new RectangleF(
            0 |> float32, 
            0 |> float32, 
            width |> float32, 
            height |> float32)

    member this.ImageBin
        with get() =
                
            try
                use ms = new System.IO.MemoryStream()
                use stream = new WICStream(wicFactory, ms)

                let encoder = new PngBitmapEncoder(wicFactory)
                encoder.Initialize(stream)

                let encode = new BitmapFrameEncode(encoder)
                encode.Initialize();
                encode.SetSize(width |> int, height |> int)

                let pixelFormatGuid = SharpDX.WIC.PixelFormat.FormatDontCare
                encode.SetPixelFormat(ref pixelFormatGuid)
                encode.WriteSource(bmp)

                encode.Commit()
                encoder.Commit()

                let bs = ms.ToArray()

                encode.Dispose()
                encoder.Dispose()
                stream.Dispose()
                bs
            with
            | _ -> [||]

