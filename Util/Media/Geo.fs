module Util.GraphicsGeo

open System

type Coord = {
mutable pinf: float
mutable psup: float
mutable dinf: float32
mutable dsup: float32 }

let coord__str (formatter:string) coord = 
    [|  "d = ["
        coord.dinf.ToString formatter
        " : "
        coord.dsup.ToString formatter
        "], p = ["
        coord.pinf.ToString formatter
        " : "
        coord.psup.ToString formatter
        "]" |]
    |> String.Concat

let p__d coord p = 
    coord.dinf + (coord.dsup - coord.dinf) * float32(p - coord.pinf) / float32(coord.psup - coord.pinf)

let d__p coord d = 
    coord.pinf + (coord.psup - coord.pinf) * float(d - coord.dinf) / float(coord.dsup - coord.dinf)

let checkP coord p = 
    if coord.pinf > p then
        coord.pinf <- p
    if coord.psup < p then
        coord.psup <- p

let stretch coord rate = 
    coord.pinf <- coord.pinf * (1.0 + rate)
    coord.psup <- coord.psup * (1.0 + rate)

type CoordXY = {
mutable x: Coord
mutable y: Coord }

let pp__dd cxy (px,py) = 
    p__d cxy.x px,p__d cxy.y py

let dd__pp cxy (dx,dy) = 
    d__p cxy.x dx,d__p cxy.y dy

type Chart = {
l: float32
t: float32
w: float32
h: float32
mutable cxy: CoordXY }

let rect__chart (l,t,w,h) = {
    l = l
    t = t
    w = w
    h = h 
    cxy = {
        x = {
            pinf = 0.0
            psup = 0.0
            dinf = l
            dsup = l + w }
        y = {
            pinf = 0.0
            psup = 0.0
            dinf = t + h
            dsup = t }}}

type ChartCorner = 
| LeftTop
| LeftBottom
| RightTop
| RightBottom
