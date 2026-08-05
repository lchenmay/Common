module UtilAvalonia.SkiaBiLayer

open System
open SkiaSharp

open Util.Perf
open Util.GraphicsGeo
open UtilAvalonia.SkiaCommon

/// 双缓冲层——替代 UtilVortice.BiLayer.BiLayer
/// 背景层可缓存为 SKBitmap，前景层每帧重绘
type BiLayer = {
    w: uint32
    h: uint32
    drawerBackground: Ctx -> unit
    drawerForeground: Ctx -> unit
    mutable backgroundo: SKBitmap option }

/// 创建双缓冲——替代 UtilVortice.BiLayer.createBiLayer
let createBiLayer (w: uint32) (h: uint32) (dBackground: Ctx -> unit) (dForeground: Ctx -> unit) = {
    w = w
    h = h
    drawerBackground = dBackground
    drawerForeground = dForeground
    backgroundo = None }

/// 渲染背景层并缓存——替代 UtilVortice.BiLayer.flushBackground
let flushBackground (bilayer: BiLayer) =
    bilayer.backgroundo <-
        drawToSKBitmap bilayer.w bilayer.h bilayer.drawerBackground
        |> Some

/// 合并背景+前景，输出为 SKBitmap——替代 UtilVortice.BiLayer.flushFinal
let flushFinal (bilayer: BiLayer) =
    use cw = new CodeWrapper("UtilAvalonia.Skia.flushFinal")

    drawToSKBitmap bilayer.w bilayer.h (fun ctx ->
        let canvas = ctx.canvas
        canvas.Clear(SKColor(0uy, 0uy, 0uy, 0uy))

        // 如果背景未缓存，先渲染
        if bilayer.backgroundo.IsNone then
            flushBackground bilayer

        // 绘制缓存的背景
        match bilayer.backgroundo with
        | Some bg ->
            canvas.DrawBitmap(bg, 0f, 0f)
        | None -> ()

        // 绘制前景
        bilayer.drawerForeground ctx)

/// 将 BiLayer 最终结果输出为 Avalonia WriteableBitmap（用于 Image 控件）
/// 合成出的临时 SKBitmap 在像素拷贝完成后立即释放，避免泄漏。
let flushFinalToWriteableBitmap (bilayer: BiLayer) =
    use final = flushFinal bilayer
    SKBitmap__WriteableBitmap final

/// 把「背景缓存 + 前景」直接合成进一张已存在的 WriteableBitmap（零分配路径）。
/// 语义与 flushFinalToWriteableBitmap 完全一致，区别只在于不新建任何位图：
/// 鼠标移动等高频帧走这里，避免每帧分配全尺寸位图造成的 GC 累积卡顿。
let flushFinalToExistingBitmap (bilayer: BiLayer) (wbmp: Avalonia.Media.Imaging.WriteableBitmap) =
    use cw = new CodeWrapper("UtilAvalonia.Skia.flushFinalToExisting")

    // 背景未缓存则先渲染（与 flushFinal 行为一致）
    if bilayer.backgroundo.IsNone then
        flushBackground bilayer

    drawToWriteableBitmap wbmp (fun ctx ->
        let canvas = ctx.canvas
        canvas.Clear(SKColor(0uy, 0uy, 0uy, 0uy))

        match bilayer.backgroundo with
        | Some bg -> canvas.DrawBitmap(bg, 0f, 0f)
        | None -> ()

        bilayer.drawerForeground ctx)

/// 将 BiLayer 最终结果输出为 GDI+ Bitmap（用于兼容旧代码）
/// 注意：UtilAvalonia 不依赖 System.Drawing.Common，如需 GDI+ Bitmap 请在调用方自行转换
