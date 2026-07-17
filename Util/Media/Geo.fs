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
  if coord.pinf > 0.0 then
      coord.pinf <- coord.pinf * (1.0 - rate)
  else
      coord.pinf <- coord.pinf * (1.0 + rate)

  if coord.psup > 0.0 then
      coord.psup <- coord.pinf * (1.0 + rate)
  else
      coord.psup <- coord.psup * (1.0 - rate)

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
mutable cxy: CoordXY
mutable visible: bool }

let rect__chart (l,t,w,h) formatter = 
  {
    l = l
    t = t
    w = w
    h = h 
    cxy = 
      {
        x = 
          {
            pinf = 0.0
            psup = 0.0
            dinf = l
            dsup = l + w
            formatter = "" }
        y = 
          {
            pinf = 0.0
            psup = 0.0
            dinf = t + h
            dsup = t
            formatter = formatter }}
    visible = true }

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


type ScaleAttach = 
| Top
| Bottom
| Left
| Right

type Scale = {
  coord: Coord
  pincrementScaleMin: float // 小刻度的物理量步进
  pincrementScaleMaj: float // 大刻度的物理量步进
  pincrementText: float // 数字标记的物理量步进
  attach: ScaleAttach }

// ============================================================================
// 刻度步进补偿算法
// ----------------------------------------------------------------------------
// 目标：给定物理量范围 [pinf, psup] 与期望刻度数，把"步进"补偿为
//   1 / 2 / 5 × 10ᵏ  这类有限位十进制小数，
// 并把刻度起点/终点 snap 到步进的整数倍。
// 这样沿轴迭代出来的物理量恒为有限有效数字（如 3.14, 3.15, 3.16），
// 绝不出现无限循环/不循环小数的刻度值。
// 该算法与"线性 or 对数"无关：对数轴在构造 Scale 时把 coord 换成 log 映射、步进给在
// log 空间即可，补偿算法本身一视同仁。
// ============================================================================

/// 核心补偿：把任意物理量跨度补偿为"漂亮"步进。
/// 步进恒为 1/2/5 × 10ᵏ（有限位十进制小数）。
/// range 为物理量跨度（自动取绝对值），nTick 为期望刻度数量。
let niceStep (range: float) (nTick: float) : float =
  if range <= 0.0 || nTick <= 0.0 then 0.0
  else
    let raw = abs range / nTick
    let mag = Math.Pow(10.0, floor (log10 raw))
    let k = raw / mag
    let f =
      if   k < 1.5 then 1.0
      elif k < 3.5 then 2.0
      elif k < 7.5 then 5.0
      else 10.0
    f * mag

/// 把物理量 p 向下补偿对齐到 step 的整数倍（刻度起点 snap）。
let alignDown (p: float) (step: float) : float =
  if step = 0.0 then p else floor (p / step) * step

/// 把物理量 p 向上补偿对齐到 step 的整数倍（刻度终点 snap）。
let alignUp (p: float) (step: float) : float =
  if step = 0.0 then p else ceil (p / step) * step

/// 由 [pinf, psup] 与目标大刻度数，求补偿后的 (对齐起点, 对齐终点, 大刻度步进)。
/// 起点/终点都被 snap 到 step 整数倍，保证刻度落点整齐且为有限有效数字。
let compensatedRange (pinf: float) (psup: float) (nMaj: float) : float * float * float =
  let step = niceStep (psup - pinf) nMaj
  if step <= 0.0 then pinf, psup, step
  else alignDown pinf step, alignUp psup step, step

/// 为 Scale 计算三档步进（小刻度 / 大刻度 / 数字标注）。
/// major 由范围补偿得出；minor = major / 5（细分，仍为有限位小数）；
/// text = 2 × major（数字标注只落在大刻度的每 2 个上）。
/// 三者比例恒为 1 : 5 : 10（小刻度 : 大刻度 : 数字标注），且均保证迭代出有限有效数字。
let scaleIncrements (pinf: float) (psup: float) (nMaj: float) : float * float * float =
  let maj = niceStep (psup - pinf) nMaj
  if maj <= 0.0 then 0.0, 0.0, 0.0
  else maj / 5.0, maj, 2.0 * maj

// ============================================================================
// 可配步进比 + 像素驱动刻度生成
// ----------------------------------------------------------------------------
// 从像素空间出发：给定轴像素长 L 与小刻度最小像素间距 minPix，
// 反算期望小刻度数 → niceStep 补偿 → 1:minorPerMaj:textPerMaj 三级刻度。
// 格式化为尽量少有效数字，横轴按实测文字宽度自适应防止重叠。
// ============================================================================

/// 可配步进比版本：给定范围与期望小刻度数(像素驱动)，返回三档步进。
/// small = 小刻度步进, maj = small × minorPerMaj, txt = small × textPerMaj
let scaleIncrementsCfg (pinf: float) (psup: float) (nMinor: float) (minorPerMaj: float) (textPerMaj: float) : float * float * float =
  let small = niceStep (psup - pinf) nMinor
  if small <= 0.0 then 0.0, 0.0, 0.0
  else small, small * minorPerMaj, small * textPerMaj

/// 最少有效数字格式化：给定步进 step，把 v 格式化为整齐且无多余尾零的字符串。
/// step=0.05 时 0.30→"0.3"、1.25→"1.25"；大数量级(≥1e6)或极小(非零且<1e-3)用科学计数。
let formatTick (step: float) (v: float) : string =
  let aStep = abs step
  let decimals =
    if aStep >= 1.0 then 0
    else max 0 (int (ceil (-log10 aStep - 1e-9)))
  let v = Math.Round(v, decimals)
  let s = v.ToString("F" + decimals.ToString())
  let s = if s.Contains(".") then s.TrimEnd('0').TrimEnd('.') else s
  if abs v >= 1e6 || (abs v > 0.0 && abs v < 1e-3) then
    v.ToString("0.###e+0")
  else
    s

/// 刻度集合：小刻度/大刻度/文字标注的物理量列表
type TickSet = {
  minors: float list
  majors: float list
  texts : (float * string) list
}

/// 从 Coord + 配置构建 Scale（像素驱动）。
/// minPix = 小刻度最小像素间距，minorPerMaj = 小:大比例(默认 5)，textPerMaj = 小:文字比例(默认 10)
let buildScale (coord: Coord) (attach: ScaleAttach) (minPix: float32) (minorPerMaj: float) (textPerMaj: float) : Scale =
  let L =
    match attach with
    | Left | Right -> abs(coord.dinf - coord.dsup) |> float
    | Top  | Bottom -> abs(coord.dsup - coord.dinf) |> float
  let nMinor = L / float minPix
  let small, maj, txt = scaleIncrementsCfg coord.pinf coord.psup nMinor minorPerMaj textPerMaj
  { coord = coord; pincrementScaleMin = small; pincrementScaleMaj = maj; pincrementText = txt; attach = attach }

/// 判断 v 是否接近 step 的整数倍（浮点容差）
let isMultipleOf (v: float) (step: float) : bool =
  if step <= 0.0 then false
  else
    let r = abs(v % step)
    r < step * 1e-9 || abs(r - abs step) < abs step * 1e-9

/// 从 Scale 生成全部刻度位置（minor/major/text 三级，起点 snap 到 small 的整数倍）
let genTicks (scale: Scale) : TickSet =
  let c = scale.coord
  let small = scale.pincrementScaleMin
  let maj   = scale.pincrementScaleMaj
  let txt   = scale.pincrementText
  if small <= 0.0 || maj <= 0.0 || txt <= 0.0 then
    { minors = []; majors = []; texts = [] }
  else
    let first = alignDown c.pinf small
    let last  = alignUp c.psup small
    let allTicks =
      [ let mutable v = first
        while v <= last + small * 0.001 do
          yield v
          v <- v + small ]
    { minors = allTicks
      majors = allTicks |> List.filter (fun v -> isMultipleOf v maj)
      texts  = allTicks |> List.filter (fun v -> isMultipleOf v txt)
                        |> List.map (fun v -> v, formatTick small v) }




