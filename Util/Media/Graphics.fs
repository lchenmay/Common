module Util.Graphics

open System.Collections.Generic

open SharpDX
open SharpDX.Mathematics.Interop
open SharpDX.Direct2D1
open SharpDX.DXGI



let white = Util.DirectX.sharpdxcolor__rawcolor4(SharpDX.Color.White)
let black = Util.DirectX.sharpdxcolor__rawcolor4(SharpDX.Color.Black)
let silver = Util.DirectX.sharpdxcolor__rawcolor4(SharpDX.Color.Silver)
let gray = Util.DirectX.sharpdxcolor__rawcolor4(SharpDX.Color.Gray)
let blue = Util.DirectX.sharpdxcolor__rawcolor4(SharpDX.Color.Blue)
let yellow = Util.DirectX.sharpdxcolor__rawcolor4(SharpDX.Color.Yellow)
let red = Util.DirectX.sharpdxcolor__rawcolor4(SharpDX.Color.Red)
let green = Util.DirectX.sharpdxcolor__rawcolor4(SharpDX.Color.Green)

// Geometry ===================================================

type Coord = 
    {
        mutable dinf:float32;
        mutable dsup:float32;
        mutable pinf:float;
        mutable psup:float;
        mutable formatter:(float -> string) }

    member this.drange() = this.dsup - this.dinf
    member this.prange() = this.psup - this.pinf
    member this.display physical = this.dinf + float32(((physical - this.pinf) * float(this.dsup - this.dinf) / (this.psup - this.pinf)))
    member this.phisical display = this.pinf + float(display - this.dinf) * (this.psup - this.pinf) / float(this.dsup - this.dinf)

    member this.containsDisplay display = (display - this.dinf) * (display - this.dsup) < 0.0f
    member this.containsPysical physical = (physical - this.pinf) * (physical - this.psup) < 0.0

    member this.checkRange physical = 
        if(this.pinf > physical) then
            this.pinf <- physical
        if(this.psup < physical) then
            this.psup <- physical

    member this.paddingRangePhysical ratio = 
        let padding = ratio * (this.psup - this.pinf)
        this.psup <- this.psup + padding
        this.pinf <- this.pinf - padding

    member this.clone() = {dinf = this.dinf; dsup = this.dsup; pinf = this.pinf; psup = this.psup; formatter = this.formatter }

    member this.copyFrom(src:Coord) = 
        this.dinf <- src.dinf
        this.dsup <- src.dsup
        this.pinf <- src.pinf
        this.psup <- src.psup

let empty__Coord() = 
    {dinf = 0.0f; dsup = 0.0f; pinf = 0.0; psup = 0.0; formatter = (fun p -> p.ToString()) }

let formatter__Coord formatter = 
    {dinf = 0.0f; dsup = 0.0f; pinf = 0.0; psup = 0.0; formatter = formatter }

let drange__coord(dinf,dsup) = 
    {
        dinf = dinf;
        dsup = dsup;
        pinf = 0.0;
        psup = 0.0;
        formatter = (fun p -> p.ToString()) }

let splitCoord (coord:Coord) n = 
    let drange = coord.drange() / float32(n)
    let mutable d = coord.dinf
    [| 0..n |]
    |> Array.map(fun i ->
        d <- d + drange
        drange__coord(d - drange,d))

type Align =
| LeftTop
| LeftBottom
| LeftMiddle
| RightTop
| RightBottom
| RightMiddle
| MiddleTop
| MiddleBottom
| MiddleMiddle

type Rect = 
    {
        mutable l:float32;
        mutable t:float32;
        mutable w:float32;
        mutable h:float32 }

    member this.r() = this.l + this.w
    member this.b() = this.t + this.h

    member this.RawRectangleF() = new RawRectangleF(float32(this.l),float32(this.t),float32(this.r()),float32(this.b()))

    member this.clone() = 
        {l = this.l; t = this.t; w = this.w; h = this.h}

    member this.tableLayout(cols:float32[],rows:float32[]) = 
        let r = rows |> Array.sum
        let c = cols |> Array.sum

        let hors = 
            let mutable x = this.l
            cols |> 
            Array.map(fun i -> 
                let l = x
                let w = this.w * i / c
                x <- x + w
                drange__coord(l,l+w))

        let vers = 
            let mutable y = this.t
            rows 
            |> 
            Array.map(fun i -> 
                let t = y
                let h = this.h * i / r
                y <- y + h
                drange__coord(t+h,t))

        hors,vers
            
let SharpDXRectange__Rect(src:SharpDX.Rectangle) = { l = float32(src.Left); t = float32(src.Top); w = float32(src.Width); h = float32(src.Height) }

let empty__Rect() = 
    {l = 0.0f; t = 0.0f; w = 0.0f; h = 0.0f}

let create_Rect(l,t,w,h) = 
    {l = l; t = t; w = w; h = h}

let text__ctx(fontname,fontsize) (text:string) = 
    let factory = new SharpDX.DirectWrite.Factory()
    let format = new SharpDX.DirectWrite.TextFormat(factory, fontname, fontsize)
    let layout = new SharpDX.DirectWrite.TextLayout(factory, text, format, 0.0f, 0.0f)
    layout.WordWrapping <- SharpDX.DirectWrite.WordWrapping.NoWrap
    format,layout.Metrics.Width,layout.Metrics.Height

let text__align(align,x,y,w,h,padding) = 
    let rect = 
        match align with
        | LeftTop -> create_Rect(x,y,w,h)
        | LeftBottom -> create_Rect(x,y-h,w,h)
        | LeftMiddle -> create_Rect(x,y-0.5f*h,w,h)
        | RightTop -> create_Rect(x-w,y,w,h)
        | RightBottom -> create_Rect(x-w,y-h,w,h)
        | RightMiddle -> create_Rect(x-w,y-0.5f*h,w,h)
        | MiddleTop -> create_Rect(x-0.5f*w,y,w,h)
        | MiddleBottom -> create_Rect(x-0.5f*w,y-h,w,h)
        | MiddleMiddle -> create_Rect(x-0.5f*w,y-0.5f*h,w,h)

    let rect_outer =
        if(padding = 0.0f)then
            rect.clone()
        else
            create_Rect(rect.l - padding,rect.t - padding,rect.w + 2.0f*padding,rect.h + 2.0f*padding)

    rect,rect_outer

let clear(wrt:RenderTarget) color = wrt.Clear(new System.Nullable<RawColor4>(color))

let drawline(wrt:RenderTarget) color (x1,y1,x2,y2) = 
    use brush = new SolidColorBrush(wrt,color)
    wrt.DrawLine(new RawVector2(x1,y1),new RawVector2(x2,y2),brush)
        
let drawcurve(wrt:RenderTarget) color (points:RawVector2[]) = 
    use brush = new SolidColorBrush(wrt,color)
    [| 0..points.Length - 2 |]
    |> Array.iter(fun i -> wrt.DrawLine(points.[i],points.[i+1],brush))


let drawlinehor(wrt:RenderTarget) color (x1,x2) y =
    use brush = new SolidColorBrush(wrt,color)
    wrt.DrawLine(new RawVector2(x1,y),new RawVector2(x2,y),brush)

let drawlinever(wrt:RenderTarget) color (y1,y2) x =
    use brush = new SolidColorBrush(wrt,color)
    wrt.DrawLine(new RawVector2(x,y1),new RawVector2(x,y2),brush)
         
let drawrect wrt edge_fill color (rect:Rect) = 
    use brush = new SolidColorBrush(wrt,color)
    if edge_fill then
        wrt.DrawRectangle(rect.RawRectangleF(),brush)
    else
        wrt.FillRectangle(rect.RawRectangleF(),brush)

let drawrectCorners wrt edge_fill color (l,t,r,b) = 
    { l = l; t = t; w = r - l; h = b - t}
    |> drawrect wrt edge_fill color

type TextFormat = {
    fontname: string;
    fontsize: float32;
    forecolor: RawColor4;
    backcolor: RawColor4 }

let drawString
    wrt
    textformat
    (align,x,y,text:string) = 

    use brush = new SolidColorBrush(wrt, textformat.forecolor)
    let format,w,h = text__ctx(textformat.fontname,textformat.fontsize) text
    let rect,rect_outter = text__align(align, x, y, w, h, 0.0f)

    drawrect wrt false textformat.backcolor rect_outter
    wrt.DrawText(text, format, rect.RawRectangleF(), brush)

type RectChart = 
    {
        mutable hor: Coord;
        mutable ver: Coord }

    member this.rect() = 
        {
            l = this.hor.dinf;
            t = this.ver.dsup;
            w = this.hor.drange();
            h = -this.ver.drange() }

    member this.drawline wrt color (x1Physical,y1Physical,x2Physical,y2Physical) = 
        let x1 = this.hor.display x1Physical
        let y1 = this.ver.display y1Physical
        let x2 = this.hor.display x2Physical
        let y2 = this.ver.display y2Physical
        drawline wrt color (x1,y1,x2,y2)

    member this.drawlineHor wrt color yPhysical = 
        let y = this.ver.display yPhysical
        drawline wrt color (this.hor.dinf,y,this.hor.dsup,y)

    member this.drawlineVer wrt color xPhysical = 
        let x = this.hor.display xPhysical
        drawline wrt color (x,this.ver.dinf,x,this.ver.dsup)

    member this.drawCross wrt color size (xPhysical,yPhysical) = 
        let x = this.hor.display xPhysical
        let y = this.ver.display yPhysical
        drawline wrt color (x - size,y,x + size,y)
        drawline wrt color (x, y - size,x,y + size)

    member this.drawcurve wrt color psPhysical =
        psPhysical
        |> Array.map(fun item -> 
            new RawVector2(this.hor.display(item |> fst),this.ver.display(item |> snd)))
        |> drawcurve wrt color

    member this.drawRangeHor wrt textformat = 
        let drawer = drawString wrt textformat
        drawer(Align.LeftTop,this.hor.dinf,this.ver.dsup,this.ver.psup |> this.ver.formatter)
        drawer(Align.LeftBottom,this.hor.dinf,this.ver.dinf,this.ver.pinf |> this.ver.formatter)

    member this.drawRangeVer wrt textformat = 
        let drawer = drawString wrt textformat
        drawer(Align.LeftTop,this.hor.dinf + 1.0f,this.ver.dsup + 1.0f,this.ver.psup |> this.ver.formatter)
        drawer(Align.LeftBottom,this.hor.dinf + 1.0f,this.ver.dinf - 1.0f,this.ver.pinf |> this.ver.formatter)

let empty__RectChart() = 
    {hor = empty__Coord(); ver = empty__Coord()}

let formatter__RectChart (fhor,fver) = 
    {hor = formatter__Coord fhor; ver = formatter__Coord fver }

let create_rectchart(l,t,w,h) = 
    {
        hor = drange__coord(l,l+w);
        ver = drange__coord(t+h,t) }

let rect__rectchart rect = create_rectchart(rect.l,rect.t,rect.w,rect.h)


//type LayerContainer = 
//    {
//        layers: Util.DirectX.ImageCanvas[] }

//let createLayerContainer(width,height,layers) = 
//    let layers = 
//        [|0 .. layers - 1|]
//        |> Array.map(fun i -> new Util.DirectX.ImageCanvas(width,height))

//    {   layers = layers  }

//let drawLayers(wrt)(layers) = 
//    layers.layers
//    |> Array.iter(fun layer ->
//        let img = 
//            layer.Image 
//            |> Util.DirectX.drawingBmp__SharpDxBmp(wrt)
//        wrt.DrawBitmap(img,1.0f,SharpDX.Direct2D1.BitmapInterpolationMode.Linear))