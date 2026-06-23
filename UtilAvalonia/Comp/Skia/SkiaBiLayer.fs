module UtilAvalonia.Comp.Skia.SkiaBiLayer

open System
open SkiaSharp

open Util.Perf
open Util.GraphicsGeo
open UtilAvalonia.Comp.Skia.SkiaCommon

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
let flushFinalToWriteableBitmap (bilayer: BiLayer) =
    flushFinal bilayer
    |> SKBitmap__WriteableBitmap

/// 将 BiLayer 最终结果输出为 GDI+ Bitmap（用于兼容旧代码）
let flushFinalToGdiBmp (bilayer: BiLayer) =
    flushFinal bilayer
    |> SKBitmap__GdiBmp
