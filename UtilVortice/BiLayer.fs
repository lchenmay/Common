module UtilVortice.BiLayer

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

open UtilVortice.Common
open UtilVortice.Graphics

type BiLayer = {
w: uint32
h: uint32
drawerBackground: Ctx -> unit
drawerForeground: Ctx -> unit
mutable backgroundo: IWICBitmap option }

let createBiLayer (w,h) dBackground dForeground = {
    w = w
    h = h
    drawerBackground = dBackground
    drawerForeground = dForeground
    backgroundo = None}

let flushBackground bilayer = 
    bilayer.backgroundo <- 
        drawToWICBitmap (bilayer.w,bilayer.h) bilayer.drawerBackground
        |> Some

let flushFinal bilayer = 
    use final = drawToWICBitmap (bilayer.w,bilayer.h) (fun x -> 
        let rt = x.rt
        rt.Clear(Color4(0f,0f,0f,0f))

        if bilayer.backgroundo.IsNone then
            flushBackground bilayer
        bilayer.backgroundo.Value
        |> rt.CreateBitmapFromWicBitmap
        |> rt.DrawBitmap

        bilayer.drawerForeground x)

    WICBitmap__GdiBmp final
