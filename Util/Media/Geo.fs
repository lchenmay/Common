module Util.GraphicsGeo

open System

type Coord = {
mutable pinf: float
mutable psup: float
mutable dinf: float32
mutable dsup: float32
mutable formatter: string }

let coord__str coord = 
    [|  "d = ["
        coord.dinf.ToString coord.formatter
        " : "
        coord.dsup.ToString coord.formatter
        "], p = ["
        coord.pinf.ToString coord.formatter
        " : "
        coord.psup.ToString coord.formatter
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

let dWithin coord d = 
    (coord.dinf - d) * (coord.dsup - d) <= 0f

let pWithin coord p = 
    (coord.pinf - p) * (coord.psup - p) <= 0.0

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

let rect__chart (l,t,w,h) formatter = {
    l = l
    t = t
    w = w
    h = h 
    cxy = {
        x = {
            pinf = 0.0
            psup = 0.0
            dinf = l
            dsup = l + w
            formatter = "" }
        y = {
            pinf = 0.0
            psup = 0.0
            dinf = t + h
            dsup = t
            formatter = formatter }}}

type ChartCorner = 
| LeftTop
| LeftBottom
| RightTop
| RightBottom

type VerAlign = 
| LeftAbove
| LeftCenter
| LeftBelow
| RightAbove
| RightCenter
| RightBelow

type HorAlign = 
| TopLeft
| TopCenter
| TopRight
| BottomLeft
| BottomCenter
| BottomRight

