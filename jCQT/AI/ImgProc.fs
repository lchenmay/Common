module jCQT.AI.ImgProc

open System
open System.Runtime.InteropServices
open SkiaSharp

// ===================== 像素基础 =====================

/// 把位图拷贝成 BGRA8888 的可写字节数组，返回 (位图, 像素数组)。
/// 返回的位图与像素数组一一对应（BGRA 布局），修改数组后需自行写回；
/// 返回的位图用于需要再绘制的下游步骤（如叠加框线后再二值化）。
let copyPixels (bmp: SKBitmap) : SKBitmap * byte[] =
    let src = bmp.Copy(SKColorType.Bgra8888)
    let arr = Array.zeroCreate<byte> src.ByteCount
    Marshal.Copy(src.GetPixels(), arr, 0, arr.Length)
    (src, arr)

/// 计算像素平均亮度 (R+G+B)/3，作为墨量近似（BGRA/RGBA 布局顺序无关）。
/// arr 为原始字节数组，pi 为像素起始字节索引；luma < 200 视为印刷墨。
let pixelLuma (arr: byte[]) (pi: int) : int =
    (int arr.[pi] + int arr.[pi + 1] + int arr.[pi + 2]) / 3

// ===================== 二值化 / 缩放 =====================

/// 二值化：把"暗"像素（luma < 200）视为墨迹、保留为不透明黑；
/// 其余（背景/浅色）设为全透明。用于把乐谱页切成干净的黑线 PNG。
/// 兼容任意输入色彩类型（非 BGRA/RGBA 会先转成 BGRA8888）。
let binarizeTransparent (bmp: SKBitmap) =
    // 先归一化成 BGRA8888（copyPixels 内部也会 Copy 成该类型，这里仅保证 guard 通过）
    let work =
        if bmp.ColorType = SKColorType.Bgra8888 || bmp.ColorType = SKColorType.Rgba8888
        then bmp
        else bmp.Copy(SKColorType.Bgra8888)
    try
        let (src, arr) = copyPixels work
        let n = arr.Length
        let count = src.Width * src.Height
        let alpha = Array.zeroCreate<byte> count
        let mutable i = 0
        while i < n do
            let luma = pixelLuma arr i
            alpha.[i / 4] <- if luma > 200 then 0uy else 255uy
            i <- i + 4
        let out = new SKBitmap(src.Width, src.Height, SKColorType.Bgra8888, SKAlphaType.Premul)
        use canvas = new SKCanvas(out)
        canvas.DrawBitmap(src, 0.0f, 0.0f)
        let outArr = Array.zeroCreate<byte> out.ByteCount
        Marshal.Copy(out.GetPixels(), outArr, 0, outArr.Length)
        for p in 0 .. count - 1 do
            outArr.[p * 4 + 3] <- alpha.[p]
        Marshal.Copy(outArr, 0, out.GetPixels(), outArr.Length)
        src.Dispose()
        out
    finally
        if not (obj.ReferenceEquals(work, bmp)) then work.Dispose()

/// 把二值化后的墨迹重新染成任意目标色，透明背景保持不变。
/// 只对 alpha > 0（即墨迹遮罩）的像素改写字面 RGB，alpha 通道原样保留，
/// 因此可反复调用：透明遮罩始终稳定，每次把墨迹覆写成新颜色。
/// 用于"非空白部分将来可以变换为任何颜色"的下游输出着色。
let recolorInk (bmp: SKBitmap) (color: SKColor) =
    let (src, arr) = copyPixels bmp   // 始终拷贝成 BGRA8888，安全写入
    let n = arr.Length
    let mutable i = 0
    while i < n do
        if arr.[i + 3] > 0uy then
            arr.[i]     <- color.Blue
            arr.[i + 1] <- color.Green
            arr.[i + 2] <- color.Red
        i <- i + 4
    Marshal.Copy(arr, 0, src.GetPixels(), arr.Length)
    src

/// 整体等比放大（scale > 1 才放大，否则原样返回），用于预览截图放大。
let scaleBitmap (src: SKBitmap) (scale: float) (bg: SKColor option) =
    if scale <= 1.0 then src
    else
        let w2 = max 1 (int (float src.Width * scale))
        let h2 = max 1 (int (float src.Height * scale))
        let dst = new SKBitmap(w2, h2, SKColorType.Bgra8888, SKAlphaType.Premul)
        let bgColor = defaultArg bg SKColors.White
        dst.Erase(bgColor)
        use canvas = new SKCanvas(dst)
        use paint = new SKPaint(IsAntialias = false)
        canvas.Save() |> ignore
        canvas.Scale(float32 scale, float32 scale)
        canvas.DrawBitmap(src, 0.0f, 0.0f, paint)
        canvas.Restore()
        dst

// ===================== 直方图 / 投影 =====================

/// 整页像素直方图（绿）：底部横向 + 左侧纵向，墨量以 luma<200 计。
/// content 是已叠加框的页面；page 用于干净像素统计（避免框线干扰分布）。
let drawPageHistograms (page: SKBitmap) (content: SKBitmap) =
    let (_, arr) = copyPixels page
    let w = page.Width
    let h = page.Height
    let colInk = Array.zeroCreate<int64> w
    let rowInk = Array.zeroCreate<int64> h
    for y in 0 .. h - 1 do
        let baseIdx = y * w * 4
        let mutable rowCount = 0L
        for x in 0 .. w - 1 do
            let idx = baseIdx + x * 4
            let luma = pixelLuma arr idx
            if luma < 200 then
                colInk.[x] <- colInk.[x] + 1L
                rowCount <- rowCount + 1L
        rowInk.[y] <- rowCount
    let maxCol = max 1L (if w > 0 then Array.max colInk else 1L)
    let maxRow = max 1L (if h > 0 then Array.max rowInk else 1L)
    let histW = max 40 (w / 12)
    let histH = max 40 (h / 12)
    let outW = w + histW
    let outH = h + histH
    let out = new SKBitmap(outW, outH, SKColorType.Bgra8888, SKAlphaType.Premul)
    out.Erase(SKColors.White)
    use canvas = new SKCanvas(out)
    canvas.DrawBitmap(content, float32 histW, 0.0f)
    use greenPaint = new SKPaint(Color = SKColors.Green, Style = SKPaintStyle.Fill, IsAntialias = false)
    for x in 0 .. w - 1 do
        let bh = int (float colInk.[x] / float maxCol * float histH)
        if bh > 0 then
            canvas.DrawRect(float32 (histW + x), float32 (h + histH - bh), 1.0f, float32 bh, greenPaint)
    for y in 0 .. h - 1 do
        let bw = int (float rowInk.[y] / float maxRow * float histW)
        if bw > 0 then
            canvas.DrawRect(float32 (histW - bw), float32 y, float32 bw, 1.0f, greenPaint)
    out

/// 在每段 band [topY, bottomY] 的底部叠加该段的列投影（vertical projection）。
/// 对每段在 [topY, bottomY] 内做列暗像素统计 (luma<200)，归一化后画绿色竖条；
/// 条带位置与页面列对齐，自动截断到下一段上边界，避免压住下一段内容。
/// 输入 content 已叠加红/绿/蓝框；page 是原始页面（干净像素统计）。
/// 输出位图尺寸 (w, h + regionHistH)。
let drawBandProjections (page: SKBitmap) (content: SKBitmap)
                        (bands: (int * int) list) =
    let w = page.Width
    let h = page.Height
    let regionHistH = max 30 (h / 22)
    if List.isEmpty bands || regionHistH <= 0 then content
    else
        let outH = h + regionHistH
        let out = new SKBitmap(w, outH, SKColorType.Bgra8888, SKAlphaType.Premul)
        out.Erase(SKColors.White)
        use canvas = new SKCanvas(out)
        canvas.DrawBitmap(content, 0.0f, 0.0f)
        let sortedBands = bands |> List.sortBy (fun (tY, _) -> tY) |> List.toArray
        let (_, arr) = copyPixels page
        use greenPaint = new SKPaint(Color = SKColors.Green, Style = SKPaintStyle.Fill, IsAntialias = false)
        for i in 0 .. sortedBands.Length - 1 do
            let (topY, bottomY) = sortedBands.[i]
            let yT = max 0 topY
            let yB = min h (bottomY + 1)
            if yB > yT then
                let colInk = Array.zeroCreate<int64> w
                for y in yT .. yB - 1 do
                    let baseIdx = y * w * 4
                    for x in 0 .. w - 1 do
                        let idx = baseIdx + x * 4
                        let luma = pixelLuma arr idx
                        if luma < 200 then colInk.[x] <- colInk.[x] + 1L
                let maxCol = max 1L (Array.max colInk)
                let stripTop = yB
                let defaultStripBot = min outH (yB + regionHistH)
                let stripBot =
                    if i + 1 < sortedBands.Length then
                        let (nextTop, _) = sortedBands.[i + 1]
                        let nextTop = max 0 nextTop
                        if nextTop > stripTop then min defaultStripBot nextTop
                        else defaultStripBot
                    else defaultStripBot
                if stripBot > stripTop then
                    let spanH = stripBot - stripTop
                    for x in 0 .. w - 1 do
                        let bh = int (float colInk.[x] / float maxCol * float spanH)
                        if bh > 0 then
                            canvas.DrawRect(float32 x, float32 (stripBot - bh),
                                            1.0f, float32 bh, greenPaint)
        out

// ===================== 连通分量（通用 4-连通 CCA） =====================

/// 4-连通 Union-Find：路径压缩查根。
let ufFind (parent: int[]) (x: int) =
    let mutable root = x
    while parent.[root] <> root do
        root <- parent.[root]
    let mutable i = x
    while i <> root do
        let next = parent.[i]
        parent.[i] <- root
        i <- next
    root

/// 4-连通 Union-Find：合并两个集合。
let ufUnion (parent: int[]) (a: int) (b: int) =
    let ra = ufFind parent a
    let rb = ufFind parent b
    if ra <> rb then parent.[ra] <- rb

/// 在子区域 [xL, xR] × [yT, yB] 内做墨迹 4-连通分量分析。
/// 墨判定：luma < 200。返回过滤掉小分量 (area < minArea) 后的分量列表，
/// 每项 = (centerX, centerY, absMinY, absMaxY, area)，坐标均为页面绝对坐标。
let findInkComponentsInSubregion
        (arr: byte[]) (pageW: int) (xL: int) (xR: int) (yT: int) (yB: int) (minArea: int) =
    let subW = xR - xL + 1
    let subH = yB - yT + 1
    if subW <= 0 || subH <= 0 then [||]
    else
        let n = subW * subH
        let isInk = Array.zeroCreate<bool> n
        let mutable row = 0
        while row < subH do
            let pageY = yT + row
            let baseIdx = pageY * pageW * 4
            let rowOff = row * subW
            let mutable col = 0
            while col < subW do
                let idx = baseIdx + (xL + col) * 4
                let luma = pixelLuma arr idx
                isInk.[rowOff + col] <- luma < 200
                col <- col + 1
            row <- row + 1

        let parent = Array.init n id
        let inline pixIdx r c = r * subW + c
        let mutable row = 0
        while row < subH do
            let mutable col = 0
            while col < subW do
                let i = pixIdx row col
                if isInk.[i] then
                    if col > 0 && isInk.[i - 1] then
                        ufUnion parent i (i - 1)
                    if row > 0 && isInk.[i - subW] then
                        ufUnion parent i (i - subW)
                col <- col + 1
            row <- row + 1

        let compArea = System.Collections.Generic.Dictionary<int, int>()
        let compBB = System.Collections.Generic.Dictionary<int, (int * int * int * int)>()
        let mutable row = 0
        while row < subH do
            let mutable col = 0
            while col < subW do
                let i = pixIdx row col
                if isInk.[i] then
                    let root = ufFind parent i
                    match compArea.TryGetValue(root) with
                    | true, v -> compArea.[root] <- v + 1
                    | false, _ -> compArea.[root] <- 1
                    match compBB.TryGetValue(root) with
                    | true, (mn, mx, mnY, mxY) -> compBB.[root] <- (min mn col, max mx col, min mnY row, max mxY row)
                    | false, _ -> compBB.[root] <- (col, col, row, row)
                col <- col + 1
            row <- row + 1

        [| for KeyValue(root, (mnX, mxX, mnY, mxY)) in compBB do
            let area = compArea.[root]
            if area >= minArea then
                let centerX = xL + (mnX + mxX) / 2
                let centerY = yT + (mnY + mxY) / 2
                let absMinY = yT + mnY
                let absMaxY = yT + mxY
                yield (centerX, centerY, absMinY, absMaxY, area) |]
