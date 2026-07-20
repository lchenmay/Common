module jCQT.Music.SheetMusic

open System
open System.IO
open System.Runtime.InteropServices

open jCQT.AI.ImgProc

open SkiaSharp



/// 单个双行谱（Grand Staff）区域，页面坐标系
type StaffRegion = {
    /// 裁剪区上边界（含 padding，可能为负，调用方用 max 0 处理）
    topY: int
    /// 裁剪区下边界（含 padding）
    bottomY: int
    /// 上五线谱（高音谱号）第一条线的绝对 Y 坐标，用于垂直对齐
    firstLineY: int
    /// 该区域内检测到的各五线谱横线绝对 Y 坐标（含上下两行谱，用于预览叠加细红线）
    lineYs: int list }

// ===================== 底层像素工具 =====================
// 像素拷贝 copyPixels 已迁移至 jCQT.AI.ImgProc（见该模块）。

/// 受限霍夫变换检测近水平谱线（剥离符干/符头/连音线等干扰）。
/// 只扫描 θ∈[87°,93°]（近水平带），对每枚暗像素按 (θ,ρ) 共线性投票；
/// 取足够强的峰值即为一根五线谱横线，返回其中心 Y 坐标（升序）。
/// 算法天然剥离：竖向符干(θ≈0 无峰)、斜向符杠/连音线(对角 θ 无峰)、
/// 点状符头(与谱线共线、反而强化峰值)、短 ledger 线(横向贯穿不足、票数不够)。
/// 受限霍夫变换检测近水平谱线（剥离符干/符头/连音线等干扰）。
/// 只扫描 θ∈[87°,93°]（近水平带），对每枚暗像素按 (θ,ρ) 共线性投票；
/// 取足够强的峰值即为一根五线谱横线，返回其中心 Y 坐标（升序）。
/// 算法天然剥离：竖向符干(θ≈0 无峰)、斜向符杠/连音线(对角 θ 无峰)、
/// 点状符头(与谱线共线、反而强化峰值)、短 ledger 线(横向贯穿不足、票数不够)。
///
/// 参数化内核（供 detectGrandStaffs 双轮调用）：
///   minVotesScale：票数门槛 = minVotesScale * 宽；主轮 0.08，敏感轮 0.03
///   yBand：只在 Y 带 [yLo,yHi] 内采集暗像素投票；None = 全图（主轮）
///   coverageThresh：横向覆盖率门槛；主轮 0.4，敏感轮 0.25
let private detectStaffLineYsCore
    (arr: byte[]) (w: int) (h: int)
    (minVotesScale: float)
    (yBand: (int * int) option)
    (coverageThresh: float)
    =
    // Y 带裁剪：带内采样既加速，又让敏感轮只聚焦 brace 跨越区段
    let yLo, yHi = match yBand with
                   | None -> 0, h - 1
                   | Some(t, b) -> max 0 t, min (h - 1) b

    // 1) 二值化候选暗像素：luma < 150（仅带内，宽松吃抗锯齿灰线）
    let pts = ResizeArray<int * int>()
    let mutable y = yLo
    while y <= yHi do
        let baseIdx = y * w * 4
        let mutable x = 0
        while x < w do
            let idx = baseIdx + x * 4
            let luma = (int arr.[idx] + int arr.[idx + 1] + int arr.[idx + 2]) / 3
            if luma < 150 then pts.Add((x, y))
            x <- x + 1
        y <- y + 1

    // 2) θ 取值 [87°,93°]，步进 0.25°（d = -12 .. 12 → 90° ± 3°）
    let thetaDegStep = 0.25
    let thetaCount = 25
    let thetas = [| for d in -12 .. 12 -> (90.0 + float d * thetaDegStep) * Math.PI / 180.0 |]
    let maxRho = int (ceil (sqrt (float (w * w + h * h))))
    let rhoBins = maxRho * 2 + 1
    let acc = Array.zeroCreate<int> (thetaCount * rhoBins)

    // 3) 共线性投票（长度加权的直线累加器，被密集音符盖住的谱线仍因共线而累加出强峰）
    for ti in 0 .. thetaCount - 1 do
        let theta = thetas.[ti]
        let cosT = cos theta
        let sinT = sin theta
        for (px, py) in pts do
            let rho = float px * cosT + float py * sinT
            let rhoOff = int (round rho) + maxRho
            if rhoOff >= 0 && rhoOff < rhoBins then
                acc.[ti * rhoBins + rhoOff] <- acc.[ti * rhoBins + rhoOff] + 1

    // 4) 峰值提取 + 非极大值抑制（NMS）：
    //    反复取全累加器内票数最高的 (θ,ρ) 细胞，记录其线 yc，并抑制其
    //    ρ±7 / θ±2 邻域。这样同一根线在离轴 θ 上由断线短段产生的散峰，
    //    会被主峰(θ=90° 整线共线程累加)的邻域一起抹掉 → 每根线只留 1 个峰，
    //    彻底消除此前"一根线变多个峰"导致的过检/聚类崩溃。
    let minVotes = int (minVotesScale * float w)
    let candidates = ResizeArray<float>()
    let mutable searching = true
    while searching do
        let mutable bestV = minVotes
        let mutable bestTi = -1
        let mutable bestRho = -1
        for ti in 0 .. thetaCount - 1 do
            let deg = thetas.[ti] * 180.0 / Math.PI
            if abs (deg - 90.0) < 2.5 then
                for rhoOff in 0 .. rhoBins - 1 do
                    let v = acc.[ti * rhoBins + rhoOff]
                    if v > bestV then
                        bestV <- v; bestTi <- ti; bestRho <- rhoOff
        if bestTi = -1 then searching <- false
        else
            let theta = thetas.[bestTi]
            let cosT = cos theta
            let sinT = sin theta
            let xc = float w / 2.0
            let rho = float (bestRho - maxRho)
            let yc = (rho - xc * cosT) / sinT
            // 带模式下只收带内峰（带外峰由全图主轮负责，避免重复）
            if yc >= float yLo && yc <= float yHi then candidates.Add(yc)
            // 抑制邻域（同一根线的散峰一并清零）
            let t0 = max 0 (bestTi - 2)
            let t1 = min (thetaCount - 1) (bestTi + 2)
            let r0 = max 0 (bestRho - 7)
            let r1 = min (rhoBins - 1) (bestRho + 7)
            for ti in t0 .. t1 do
                for rhoOff in r0 .. r1 do
                    acc.[ti * rhoBins + rhoOff] <- 0

    // 5) 合并近邻峰（极近散峰兜底合并，yc 差 ≤4px 视为同一根；
    //    4px 远小于真实线间距 11px，绝不会误并真实线）
    candidates.Sort()
    let rawMerged = ResizeArray<int>()
    let mutable lastYc = Double.NegativeInfinity
    for yc in candidates do
        if yc - lastYc > 4.0 then
            rawMerged.Add(int (round yc))
            lastYc <- yc

    // 6) 横向覆盖率过滤：谱线必须横向贯穿足够宽度（≥coverageThresh 列在某薄带内有暗像素），
    //    这样短横元素(断线碎片/连音线/短括号)即使票数够也被剔除，只有真谱线留下。
    let lineCoverage (yc: int) =
        let y0 = max 0 (yc - 2)
        let y1 = min (h - 1) (yc + 2)
        let mutable cols = 0
        for x in 0 .. w - 1 do
            let mutable found = false
            let mutable y = y0
            while y <= y1 && not found do
                let idx = y * w * 4 + x * 4
                let luma = (int arr.[idx] + int arr.[idx + 1] + int arr.[idx + 2]) / 3
                if luma < 150 then found <- true
                y <- y + 1
            if found then cols <- cols + 1
        float cols / float w

    let ys =
        [| for yc in rawMerged do
            if lineCoverage yc >= coverageThresh then yield yc |]

    if System.Environment.GetEnvironmentVariable("HOUGH_DEBUG") <> null then
        printfn "[hough] band=%A minVotes=%d coverage>=%.2f rawMerged=%d -> %A"
            yBand minVotes coverageThresh rawMerged.Count (rawMerged |> Seq.toList)
        printfn "[hough] afterCoverage=%d -> %A" ys.Length (ys |> Array.toList)
    ys |> Array.toList

/// 主入口：正常阈值全图检测（保持原 detectStaffLineYs 语义，供外部兼容）
let private detectStaffLineYs arr w h =
    detectStaffLineYsCore arr w h 0.08 None 0.4



/// 按 region 配对检测大括号：每个 Grand Staff（region）左侧最左几列的"高墨量列群"即 brace。
/// 谱号在 brace 右侧不会被误中；brace 中腰凹进也不会被竖向 gap 切碎（直接用 region 边界）。
/// 返回与 regions 一一对应的 (xCenter, yTop, yBot)，y 范围 = region 顶到底（横跨 10 线），
/// 与红框完全对齐，不会再被误切成多段错位假 brace。
let private detectBracesForRegions (arr: byte[]) (w: int) (h: int) (regions: StaffRegion list) =
    if List.isEmpty regions then [] else
    let leftCols = max 1 (int (0.18 * float w))
    regions |> List.map (fun reg ->
        let y0 = max 0 reg.topY
        let y1 = min h (reg.bottomY + 1)
        let regionH = max 1 (y1 - y0)
        // 1) 在 region 的 y 范围内逐列统计暗像素（垂直墨量）
        let colInk = Array.zeroCreate<int> leftCols
        for y in y0 .. y1 - 1 do
            let baseIdx = y * w * 4
            for x in 0 .. leftCols - 1 do
                let idx = baseIdx + x * 4
                let luma = (int arr.[idx] + int arr.[idx + 1] + int arr.[idx + 2]) / 3
                if luma < 150 then colInk.[x] <- colInk.[x] + 1
        // 2) 找"墨量 ≥ 30% region 高度"的最左那列（brace 特征：贯穿整个 grand staff）
        let inkThresh = int (0.30 * float regionH)
        let mutable xStart = -1
        for x in 0 .. leftCols - 1 do
            if xStart = -1 && colInk.[x] >= inkThresh then xStart <- x
        if xStart < 0 then
            // 兜底：取墨量最大的列（即便只有谱号也能凑合标出大致位置）
            let xPeak = colInk |> Array.mapi (fun i v -> (i, v)) |> Array.maxBy snd |> fst
            (xPeak, reg.topY, reg.bottomY)
        else
            // 3) 从 xStart 向右扩展到墨量下降到阈值以下，作为 brace 宽度
            let mutable xEnd = xStart
            let mutable xx = xStart + 1
            while xx < leftCols && colInk.[xx] >= inkThresh do
                xEnd <- xx
                xx <- xx + 1
            let xCenter = (xStart + xEnd) / 2
            (xCenter, reg.topY, reg.bottomY)
    )

/// 从位图检测大括号（用于可视化标注）。必须传入 regions 配对，
/// 每个 region 出一个精确 brace（y 范围 = region 顶到底）。
let detectBracesFromBitmap (bmp: SKBitmap) (regions: StaffRegion list) =
    if List.isEmpty regions then [] else
    let (src, arr) = copyPixels bmp
    use _src = src
    detectBracesForRegions arr src.Width src.Height regions

/// 列宽严格性：以 colInk[x] 的 50% 为阈值向左右扩展，返回该竖线的"有效列宽"(px)。
/// 真实小节线是孤立 1~2px 竖线；宽墨团（notehead、谱号笔画、密集音符串）会显著变宽。
/// 用于剔除"判据都过、但本不该是竖线"的宽墨团。由 markBarLines 判据5 使用。
/// cur < 10L 视为无墨列直接淘汰（返回 999）。
let lineWidthStrict (colInk: int64[]) (x: int) (startX: int) (w: int) =
    let cur = colInk.[x]
    if cur < 10L then 999
    else
        let th = max 1L (cur / 2L)
        let mutable left = x
        while left > startX && colInk.[left - 1] >= th do
            left <- left - 1
        let mutable right = x
        while right < w - 1 && colInk.[right + 1] >= th do
            right <- right + 1
        right - left + 1

/// 检测每段的"谱号/调号/拍号"区域（clef / key signature / time signature）。
/// 【新算法 v8】基于谱线对齐列投影：对每列 x，统计在 lineYs（已知 5/10 根谱线 Y 坐标）的
/// ±2px 带内有墨的线数 → colAlignedInk[x]。谱号/调号/拍号牢牢趴在谱线上 → 左侧连续高密度列；
/// 音符区仅在个别列有墨 → 稀疏。扫描找到高密度持续区的右边界即为 xR。
/// 返回与 regions 一一对应的矩形 (xLeft=0, xRight, yTop, yBottom)。
let detectClefKeyTimeRegions (bmp: SKBitmap) (regions: StaffRegion list) =
    if List.isEmpty regions then [] else
    let (src, arr) = copyPixels bmp
    use _src = src
    let w = src.Width
    let h = src.Height
    [ for reg in regions do
        let y0 = max 0 reg.topY
        let y1 = min h (reg.bottomY + 1)
        let lineYs = reg.lineYs
        let lineCount = List.length lineYs
        if y1 <= y0 || lineCount = 0 then
            yield (0, w, y0, y1)   // 无谱线信息 → 退回全宽
        else
            // ---- 1) 谱线对齐列投影 ----
            //    每列 x 统计有多少根谱线在 ±2px 带内有暗像素 (luma<150)
            let colAlignedInk = Array.zeroCreate<int> w
            for x in 0 .. w - 1 do
                let mutable cnt = 0
                for ly in lineYs do
                    let mutable found = false
                    let mutable dy = -2
                    while dy <= 2 && not found do
                        let yy = ly + dy
                        if yy >= y0 && yy < y1 then
                            let idx = yy * w * 4 + x * 4
                            let luma = (int arr.[idx] + int arr.[idx + 1] + int arr.[idx + 2]) / 3
                            if luma < 150 then found <- true
                        dy <- dy + 1
                    if found then cnt <- cnt + 1
                colAlignedInk.[x] <- cnt

            // ---- 2) 平滑 (±1 列) 去噪 ----
            let smoothed = Array.zeroCreate<int> w
            for x in 0 .. w - 1 do
                let mutable sum = 0
                let mutable n = 0
                for dx in -1 .. 1 do
                    let xx = x + dx
                    if xx >= 0 && xx < w then
                        sum <- sum + colAlignedInk.[xx]
                        n <- n + 1
                smoothed.[x] <- (sum + n / 2) / n   // 四舍五入

            // ---- 3) 分类：列"被谱线符号占据" = 半数以上谱线位置有墨 ----
            let occupiedThr = lineCount / 2   // 10线→5, 5线→2
            let occupied = Array.init w (fun x -> smoothed.[x] >= occupiedThr)

            // ---- 4) 提取被占据列的连续段，合并小间隙（≤5px）
            //        谱号→调号→拍号间可能有窄缝，合并后得到一个完整的"头部块" ----
            let runs = ResizeArray<int * int>()
            let mutable inRun = false
            let mutable runStart = 0
            for x in 0 .. w - 1 do
                if occupied.[x] then
                    if not inRun then runStart <- x; inRun <- true
                elif inRun then
                    runs.Add((runStart, x - 1))
                    inRun <- false
            if inRun then runs.Add((runStart, w - 1))

            let merged = ResizeArray<int * int>()
            if runs.Count > 0 then
                let mutable curS, curE = runs.[0]
                for i in 1 .. runs.Count - 1 do
                    let nS, nE = runs.[i]
                    if nS - curE <= 5 then
                        curE <- nE   // 间隙 ≤5px → 合并
                    else
                        merged.Add((curS, curE))
                        curS <- nS
                        curE <- nE
                merged.Add((curS, curE))

            // ---- 5) 取第一个起始在左侧（≤60px）的合并块 = 谱号/调号/拍号区 ----
            let headerBlock =
                merged |> Seq.tryFind (fun (s, _) -> s <= 60)
            match headerBlock with
            | Some (_, xEnd) -> yield (0, min w (xEnd + 2), y0, y1)
            | None ->
                // 退路：取最左的合并块（即使起始超过 60px，如谱号被裁掉一半的极端情况）
                if merged.Count > 0 then
                    let _, xEnd = merged.[0]
                    yield (0, min w (xEnd + 2), y0, y1)
                else
                    yield (0, w, y0, y1) ]

// ===================== 五线谱检测 =====================

/// 用水平投影 + 阈值扫描检测谱线（替代 Hough/等距链）。
/// 原理：谱线贯穿全宽 → rowInk 值远高于音符/文字；从高到低逐值降阈值，
/// 当连续段数恰好 = 10（大谱表）或 5（单行谱）时，这些段的中点即谱线 Y。
/// 优先匹配 10 根（大谱表），退而求 5 根（单行谱），都没有则返空。
let private detectStaffLinesByProjection (rowInk: int[]) (t: int) (b: int) =
    // 收集 band 内所有唯一 rowInk 值，从高到低排序（作"阈值阶梯"）
    let thresholds =
        let vals = ResizeArray<int>()
        for y in t .. b - 1 do vals.Add(rowInk.[y])
        vals |> Seq.distinct |> Seq.sortDescending |> Array.ofSeq

    // 对给定阈值，统计 band 内 rowInk >= threshold 的连续段数，返回段中点列表
    let segmentsAt (threshold: int) =
        let segs = ResizeArray<int * int>()
        let mutable inSeg = false
        let mutable segStart = 0
        for y in t .. b - 1 do
            if rowInk.[y] >= threshold then
                if not inSeg then segStart <- y; inSeg <- true
            elif inSeg then
                segs.Add((segStart, y - 1)); inSeg <- false
        if inSeg then segs.Add((segStart, b - 1))
        segs |> Seq.map (fun (s, e) -> (s + e) / 2) |> Seq.toList

    // 从高到低扫描阈值，第一个恰好命中 targetCount 的即谱线
    let tryTarget (targetCount: int) =
        thresholds
        |> Array.tryPick (fun thr ->
            let ys = segmentsAt thr
            if ys.Length = targetCount then Some ys else None)

    match tryTarget 10 with
    | Some ys -> ys           // 大谱表：高音 5 + 低音 5
    | None ->
        match tryTarget 5 with
        | Some ys -> ys       // 单行谱：5 线
        | None -> []


/// 检测一页乐谱中每段"大谱表行"的垂直范围（用于分行核对）。
/// 仅用水平直方图分块，不做任何 5 线检测：
///   5 线系统的横线横贯整行，每个系统（单行谱或钢琴大谱表）在行直方图里都是一段
///   连续的墨量带，分块判据极稳健、几乎不漏检。
/// 每个连通墨量块 = 一段大谱表行（钢琴大谱表的高音/低音谱被平滑桥接成一段绿框）。
/// 返回按从上到下排序的 StaffRegion 列表。
let detectGrandStaffs (bmp: SKBitmap) =
    let (src, arr) = copyPixels bmp
    use _src = src
    let w = src.Width
    let h = src.Height

    // ---------- 水平直方图分块（唯一阶段） ----------
    // 逐行统计暗像素数量（luma < 150 视为印刷墨）
    let rowInk = Array.zeroCreate<int> h
    let mutable yy = 0
    while yy < h do
        let baseIdx = yy * w * 4
        let mutable cnt = 0
        let mutable xx = 0
        while xx < w do
            let idx = baseIdx + xx * 4
            let luma = (int arr.[idx] + int arr.[idx + 1] + int arr.[idx + 2]) / 3
            if luma < 150 then cnt <- cnt + 1
            xx <- xx + 1
        rowInk.[yy] <- cnt
        yy <- yy + 1

    // 平滑窗口（≈ 2% h）：把同一系统内的 5 根谱线 + 中间空隙合并成单个"墨量带"，
    // 并桥接大谱表内部（高音谱与低音谱之间）的窄缝，使其成为「一段」连通块。
    // 2% h 明显大于系统内缝（约 1 线距 ≈ 1-2% h 量级）、明显小于不同系统间缝
    // （多个线距 ≫ 2% h），因此只会桥接系统内部、不会误并两个系统。
    let smoothWin = max 8 (int (0.02 * float h))
    let rowInkS = Array.zeroCreate<float> h
    for y in 0 .. h - 1 do
        let a = max 0 (y - smoothWin)
        let b = min (h - 1) (y + smoothWin)
        let mutable s = 0
        for k in a .. b do s <- s + rowInk.[k]
        rowInkS.[y] <- float s / float (b - a + 1)

    // 行是否"有乐谱墨迹"：暗像素占比 ≥ inkFrac。
    // 谱线横贯整行 → 高占比；零散音符/文字只占部分宽度 → 不算
    let inkFrac = 0.12
    let inkThresh = max 4 (int (inkFrac * float w))
    let isInk (y: int) = rowInkS.[y] >= float inkThresh

    // 分块：连续"有墨"行。gapThresh 须 > 大谱表内部空隙、< 不同大谱表之间空隙。
    let gapThresh = max 20 (int (0.03 * float h))
    // 最小块高：约 4 个线间距，保证单行谱系统（《Très animé》等）也能被保留
    let minBlockH = max 24 (int (0.016 * float h))
    let blocks = ResizeArray<int * int>()
    let mutable runTop = -1
    let mutable runBot = -1
    let mutable gap = 0
    let finalize () =
        if runTop >= 0 then
            if runBot - runTop >= minBlockH then blocks.Add((runTop, runBot))
            runTop <- -1; runBot <- -1; gap <- 0
    for y in 0 .. h - 1 do
        if isInk y then
            if runTop = -1 then runTop <- y
            runBot <- y
            gap <- 0
        else
            if runTop >= 0 then
                gap <- gap + 1
                if gap > gapThresh then finalize ()
    finalize ()
    let blocks = blocks |> Seq.sort |> Seq.toList

    if System.Environment.GetEnvironmentVariable("HOUGH_DEBUG") <> null then
        printfn "[hough] histRows(before split)=%d -> %A" blocks.Length blocks

    // ---------- 后处理：基于全局高度分布拆分"被合并的大谱表行" ----------
    // 大部分块高度应落在"单行大谱表"区间；若某块高度明显 ≈ 2× 中位数，
    // 强烈提示：两个大谱表行的间隙被平滑窗口吃掉、合并成一段。
    // 修复：把嫌疑块在块内墨量最低处（系统间空隙）一切为二。
    let finalBlocks =
        if List.isEmpty blocks then blocks
        else
            // 1) 中位数 = 鲁棒的中心高度
            let sortedH = blocks |> List.map (fun (t, b) -> b - t) |> List.sort
            let medianH = sortedH.[sortedH.Length / 2]
            // 2) 合并嫌疑门槛：≈ 1.65× 中位数（双行 ≈ 2×；给容差避开密集音符导致的略高块）
            let mergedThresh = int (1.65 * float medianH)

            // 3) 切分函数：块内正中 ± blockH/4 范围内找墨量最低的行作切点；
            //    切两半后每半须 ≥ minBlockH，否则不切（避免误切正常块）
            let splitMerged (t: int, b: int) =
                let blockH = b - t
                let mid = (t + b) / 2
                let rad = max 1 (blockH / 4)
                let searchT = max (t + 1) (mid - rad)
                let searchB = min (b - 1) (mid + rad)
                let mutable bestY = mid
                let mutable bestV = rowInk.[mid]
                for y in (searchT + 1) .. searchB do
                    if rowInk.[y] < bestV then
                        bestV <- rowInk.[y]
                        bestY <- y
                let upper = (t, max t (bestY - 1))
                let lower = (min b (bestY + 1), b)
                if (snd upper - fst upper) < minBlockH
                   || (snd lower - fst lower) < minBlockH then
                    [(t, b)]
                else [upper; lower]

            blocks
            |> List.collect (fun blk ->
                let ht = snd blk - fst blk
                if ht > mergedThresh then
                    let pieces = splitMerged blk
                    if System.Environment.GetEnvironmentVariable("HOUGH_DEBUG") <> null then
                        printfn "[hough] split outlier h=%d (median=%d, thresh=%d) -> %d pieces %A"
                                ht medianH mergedThresh pieces.Length pieces
                    pieces
                else [blk])

    if System.Environment.GetEnvironmentVariable("HOUGH_DEBUG") <> null then
        printfn "[hough] histRows(after split)=%d" finalBlocks.Length

    // 每个连通墨量块 = 一段大谱表行。上下加 padding；firstLineY 暂取块顶
    // （5 线定位后续作为可选精修再加回，仅用于对齐基准，不再丢弃整块）。
    let pad = max 8 (int (0.012 * float h))
    [ for (t, b) in finalBlocks ->
        // 在每段 band 内检测全部五线谱横线（投影峰 + 等距约束）。
        // 钢琴大谱表 = 上高音谱 5 线 + 下低音谱 5 线 = 10 根；
        // 单行谱系统 = 5 根。lineYs 升序，供 drawStaffLines 画淡紫色线核对。
        let lineYs =
            detectStaffLinesByProjection rowInk t (b - 1)
            |> List.sort
        { topY = t - pad
          bottomY = b + pad
          firstLineY = t
          lineYs = lineYs } ]

// ===================== 裁剪 =====================

/// 裁剪指定垂直范围（全宽）的子图
let cropRegion (bmp: SKBitmap) (topY: int) (bottomY: int) =
    let (src, _) = copyPixels bmp
    use _src = src
    let y0 = max 0 topY
    let y1 = min src.Height (bottomY + 1)
    if y1 <= y0 then src.Copy()
    else
        let rect = SKRectI(0, y0, src.Width, y1)
        let dst = new SKBitmap(src.Width, y1 - y0, SKColorType.Bgra8888, SKAlphaType.Premul)
        src.ExtractSubset(dst, rect) |> ignore
        dst

/// 在位图上画一个红色矩形框（用于标注谱号/调号/拍号区）。
/// 红框颜色 (R=255, G=0, B=0) 的 luma=85 < binarizeTransparent 的 200 阈值，
/// 因此会被保留为不透明而非背景；trimMargins 也因 alpha=255 不会裁掉。
/// 坐标 (xL, xR, yT, yB) 均相对 bmp 本身。
let drawClefOverlay (bmp: SKBitmap) (xL: int) (xR: int) (yT: int) (yB: int) =
    let out = bmp.Copy(SKColorType.Bgra8888)
    use canvas = new SKCanvas(out)
    let cStroke = max 2.0f (float32 out.Height / 280.0f)
    use cPaint = new SKPaint(Color = SKColors.Red, Style = SKPaintStyle.Stroke,
                             StrokeWidth = cStroke, IsAntialias = true)
    let top = max 0 yT
    let bot = min out.Height (yB + 1)
    let l = max 0 xL
    let r = min out.Width xR
    if bot > top && r > l then
        canvas.DrawRect(float32 l, float32 top, float32 (r - l), float32 (bot - top), cPaint)
    out

// ===================== 二值化 + 透明背景 =====================
// 实现已迁移至 jCQT.AI.ImgProc.binarizeTransparent，这里仅做转发以保持 SheetMusic 公开 API 稳定。

/// 二值化阈值处理：亮像素（背景）-> 透明；暗像素（印刷）-> 保留原色、不透明
/// 阈值：灰度 > 200 视为背景
let binarizeTransparent (bmp: SKBitmap) =
    jCQT.AI.ImgProc.binarizeTransparent bmp

/// 把二值化后的墨迹重新染成任意目标色（透明背景不变），转发至 jCQT.AI.ImgProc.recolorInk。
let recolorInk (bmp: SKBitmap) (color: SKColor) =
    jCQT.AI.ImgProc.recolorInk bmp color

// ===================== 去左右留白 =====================

/// 根据 alpha 通道裁掉左右空白列，返回去白边后的子图
let trimMargins (bmp: SKBitmap) =
    let (src, arr) = copyPixels bmp
    use _src = src
    let w = src.Width
    let h = src.Height
    let mutable left = w
    let mutable right = -1
    let mutable y = 0
    while y < h do
        let baseIdx = y * w * 4
        let mutable x = 0
        while x < w do
            if arr.[baseIdx + x * 4 + 3] > 0uy then
                if x < left then left <- x
                if x > right then right <- x
            x <- x + 1
        y <- y + 1
    if left > right then src.Copy()
    else
        let rect = SKRectI(left, 0, right + 1, h)
        let dst = new SKBitmap(rect.Width, rect.Height, SKColorType.Bgra8888, SKAlphaType.Premul)
        src.ExtractSubset(dst, rect) |> ignore
        dst

// ===================== 垂直对齐 =====================

/// 以每个片段上五线谱首线为基准，统一向上补透明像素使拼接时谱线齐平
/// 输入 (bitmap, baseline)，baseline = 首线在片段内的 Y
let alignVertical (segs: (SKBitmap * int) list) =
    if List.isEmpty segs then []
    else
        let maxBase = segs |> List.map snd |> List.max
        let maxBelow = segs |> List.map (fun (b, bl) -> b.Height - bl) |> List.max
        let commonH = maxBase + maxBelow
        segs |> List.map (fun (b, bl) ->
            let out = new SKBitmap(b.Width, commonH, SKColorType.Bgra8888, SKAlphaType.Premul)
            out.Erase(SKColors.Transparent)
            use canvas = new SKCanvas(out)
            canvas.DrawBitmap(b, 0.0f, float32 (maxBase - bl))
            out)

// ===================== 横向拼接 =====================

/// 将多个片段按序横向拼接成一张超宽透明图
let stitchHorizontally (bitmaps: SKBitmap list) (separator: int) =
    if List.isEmpty bitmaps then null
    else
        let sep = max 0 separator
        let totalW = List.sumBy (fun (b: SKBitmap) -> b.Width) bitmaps + sep * (List.length bitmaps - 1)
        let maxH =
            (bitmaps |> List.maxBy (fun (b: SKBitmap) -> b.Height)).Height
        let out = new SKBitmap(totalW, maxH, SKColorType.Bgra8888, SKAlphaType.Premul)
        out.Erase(SKColors.Transparent)
        use canvas = new SKCanvas(out)
        let mutable x = 0
        for b in bitmaps do
            canvas.DrawBitmap(b, float32 x, 0.0f)
            x <- x + b.Width + sep
        out

// ===================== 保存片段 =====================

/// 按顺序保存片段为 PNG 到 {pdfDir}/segments/page{页码}_{行号}.png
/// 返回保存的文件路径列表
let saveSegments (items: (SKBitmap * (int * int)) list) (pdfPath: string) =
    let dir = Path.Combine(Path.GetDirectoryName(pdfPath), "segments")
    Directory.CreateDirectory(dir) |> ignore
    [ for (bmp, (p, r)) in items ->
        let filePath = Path.Combine(dir, sprintf "page%d_%d.png" p r)
        use img = SKImage.FromBitmap(bmp)
        use data = img.Encode(SKEncodedImageFormat.Png, 100)
        use fs = new FileStream(filePath, FileMode.Create, FileAccess.Write)
        data.SaveTo(fs)
        filePath ]

// ===================== 区域线框叠加（用于预览核对准确性） =====================

/// 在页面位图上为每个检测到的区域画红色线框（整页宽度）。
/// regions 来自 detectGrandStaffs，框的上/下边界 = topY / bottomY。
/// clefRegions 来自 detectClefKeyTimeRegions，框出每段的谱号/调号/拍号区。
let drawRegionBoxes (bmp: SKBitmap) (regions: StaffRegion list)
                    (braces: (int * int * int) list)
                    (clefRegions: (int * int * int * int) list) =
    let out = bmp.Copy(SKColorType.Bgra8888)
    use canvas = new SKCanvas(out)

    // ── 绿框：大谱表行区域（用于分行核对） ──
    if not (List.isEmpty regions) then
        let stroke = max 2.0f (float32 out.Height / 250.0f)
        use boxPaint = new SKPaint(Color = SKColors.Green, Style = SKPaintStyle.Stroke,
                                   StrokeWidth = stroke, IsAntialias = true)
        for reg in regions do
            let top = max 0 reg.topY
            let bot = min out.Height (reg.bottomY + 1)
            if bot > top then
                canvas.DrawRect(0.0f, float32 top, float32 out.Width, float32 (bot - top), boxPaint)

    // ── 蓝框：检测到的大括号位置/范围 ──
    if not (List.isEmpty braces) then
        let braceStroke = max 1.5f (float32 out.Height / 400.0f)
        use bracePaint = new SKPaint(Color = SKColors.Blue, Style = SKPaintStyle.Stroke,
                                     StrokeWidth = braceStroke, IsAntialias = true)
        let braceHalfW = max 12 (out.Width / 60)   // 框宽自适应
        for (xc, yT, yB) in braces do
            let top = max 0 yT
            let bot = min out.Height (yB + 1)
            let xL = max 0 (xc - braceHalfW)
            let xR = min out.Width (xc + braceHalfW)
            if bot > top && xR > xL then
                canvas.DrawRect(float32 xL, float32 top,
                                float32 (xR - xL), float32 (bot - top), bracePaint)

    // ── 红框：谱号/调号/拍号区域 ──
    if not (List.isEmpty clefRegions) then
        let cStroke = max 2.0f (float32 out.Height / 280.0f)
        use cPaint = new SKPaint(Color = SKColors.Red, Style = SKPaintStyle.Stroke,
                                 StrokeWidth = cStroke, IsAntialias = true)
        for (xL, xR, yT, yB) in clefRegions do
            let top = max 0 yT
            let bot = min out.Height (yB + 1)
            let l = max 0 xL
            let r = min out.Width xR
            if bot > top && r > l then
                canvas.DrawRect(float32 l, float32 top,
                                float32 (r - l), float32 (bot - top), cPaint)
    out

// ===================== 谱线叠加（用于预览核对准确性） =====================

/// 在每个检测到的区域内，沿其 lineYs（Hough 检出的五线谱横线）画淡紫色水平线，
/// 用于肉眼核对"大谱表的 10 根线（高音 5 + 低音 5）是否都被正确识别"。
/// regions 来自 detectGrandStaffs（lineYs 已在该函数内填充）。
/// 返回新的位图（原 bmp 不被修改）。
let drawStaffLines (bmp: SKBitmap) (regions: StaffRegion list) =
    let out = bmp.Copy(SKColorType.Bgra8888)
    use canvas = new SKCanvas(out)
    use paint = new SKPaint(
        Color = SKColor(180uy, 130uy, 230uy, 255uy),
        Style = SKPaintStyle.Stroke,
        StrokeWidth = 1.5f,
        IsAntialias = false)
    for reg in regions do
        for y in reg.lineYs do
            let yy = float32 y
            if yy >= 0.0f && yy < float32 out.Height then
                canvas.DrawLine(0.0f, yy, float32 out.Width, yy, paint)
    out

// ===================== 像素直方图（用于预览核对） =====================

/// 在页面预览图四周加绿色像素直方图：
///   底部 = 每列的暗像素总数（横向直方图，绿色竖条，列与页面列对齐）；
///   左侧 = 每行的暗像素总数（纵向直方图，绿色横条，行与页面行对齐）。
// ===================== 直方图 / 每块列投影（转发至 jCQT.AI.ImgProc） =====================
// 实现已迁移至 jCQT.AI.ImgProc（drawPageHistograms / drawBandProjections），这里仅做转发
// 以保持 SheetMusic 公开 API 稳定。

/// 整页像素直方图（绿）：底部横向 + 左侧纵向，墨量以 luma<200 计。
/// content 是已叠加框的页面；page 用于干净像素统计（避免框线干扰分布）。
let drawPageHistograms (page: SKBitmap) (content: SKBitmap) =
    jCQT.AI.ImgProc.drawPageHistograms page content

/// 在每段（region）底部叠加该段的列投影（委托 jCQT.AI.ImgProc.drawBandProjections）。
/// regions 来自 detectGrandStaffs；content 已叠加红/绿/蓝框；page 为原始页面。
let drawRegionProjections (page: SKBitmap) (content: SKBitmap)
                          (regions: StaffRegion list) =
    jCQT.AI.ImgProc.drawBandProjections page content [ for r in regions -> (r.topY, r.bottomY) ]

// ===================== 小节线检测与标记 =====================

/// 小节线检测（判据链 v5：列投影 + 子带双峰值 + 连续性 + 列宽严格性）
///   判据1: 列投影 colInk 局部峰值（>= 0.3*bandH 且局部极大）→ 提名候选列。
///   判据2: 子带双峰值——把 band 在竖直中点一分为二（上支/下支），要求
///          上支 colInk >= 0.3*上支高 且 下支 colInk >= 0.3*下支高。
///          滤掉只占单支谱表高度的符干/和弦。
///   判据3: 连续性——列中最长连续暗像素段 maxRun >= 0.4*bandH。
///          真实小节线是 1px 宽的连续竖线 → 在绝大多数 band 高度上都有墨。
///          **重要**：4/4 拍密集 16/32 分音符段，notehead 横向紧密到近乎连续，
///          整列的"列内最大连续段"也能接近 bandH——**判据3 在这种情况下失效**。
///   判据4: 相邻小节线距离下限 minDist，避免同一根线被抖动点重复标记。
///   判据5（v5 新增，与判据1~3 完全正交）: 列宽严格性。
///          **核心几何差异**：
///          - 真实小节线是孤立 1~2px 竖线,"有效列宽"（以 30% 峰值为阈值
///            向左右扩展）必须 ≤ 2px。
///          - 单 notehead 是 3~5px 直径圆形，列宽 3~5px。
///          - 16/32 分音符"notehead 串"横向密集重叠，列宽 6~15px。
///          - 4/4 拍宽和弦块列宽 ≥ 20px。
///          判据5 完全不依赖 ±N 邻域范围（避免 v4 ±2..5 跨过 notehead 中心
///          被污染的坑），仅测本候选列"30% 峰值以上"的左右扩展宽度。
///          宽墨团（notehead 串、和弦块、谱号宽笔画）直接滤掉。
///          **判据1~4 判不了的所有"宽墨团伪迹"都被这一条根除**。
/// 排除最左侧 brace 区（约左侧 4% 页宽）。
/// 用 Blue 画竖线（区别于绿色投影/直方图，且比 Cyan 在白底缩略图上更醒目）。
/// page 用于干净像素统计；content 是已叠加红/绿框的页面（在其上再画蓝色小节线）。
/// 返回新的位图（原 content 不被修改）。
/// 调试信息通过 output 写入主程序日志区（Avalonia TextConsole 日志组件，即 runtime.output）。
let markBarLines (output: string -> unit)
                 (page: SKBitmap) (content: SKBitmap)
                 (regions: StaffRegion list) (_clefRegions: (int * int * int * int) list) =
    if List.isEmpty regions then
        output "[小节线] 无乐谱段，跳过小节线检测"
        (content, [])
    else
        let w = page.Width
        let h = page.Height
        let (_, arr) = copyPixels page
        let out = content.Copy(SKColorType.Bgra8888)
        use canvas = new SKCanvas(out)
        let stroke = max 3.0f (float32 h / 400.0f)
        use bluePaint = new SKPaint(Color = SKColors.Blue, Style = SKPaintStyle.Stroke,
                                     StrokeWidth = stroke, IsAntialias = true)
        // 每段检测到的小节线 x 坐标（按 regions 顺序对齐），供 detectStaffGapByInkComponents 复用
        let barLinesPerRegion = ResizeArray<int list>()
        // 只排除最左侧 brace 区（约左侧 4% 页宽），避免红框偏宽连累小节线
        let braceZone = max 20 (int (0.04 * float w))
        let startX = min w braceZone
        // 判据4: 相邻距离下限
        let minDist = max 8 (int (0.012 * float w))
        // 局部函数：列 x 在 [y0,y1) 内最长连续暗像素段 (长度, 起始y, 结束y)
        // 真实小节线是 1px 宽的连续竖线 → 在大多数 band 高度上都有墨 → maxRun ≈ bandH
        // 符干/和弦簇即使上下都有 notehead，最长连续段也只含 stem 那一段
        // （30~50px），远低于 bandH。
        let longestVerticalRun (x: int) (y0: int) (y1: int) (lumaTh: int) =
            let mutable best = 0
            let mutable bestTop = -1
            let mutable bestBot = -1
            let mutable cur = 0
            let mutable curTop = -1
            for y in y0 .. y1 - 1 do
                let idx = y * w * 4 + x * 4
                let luma = (int arr.[idx] + int arr.[idx + 1] + int arr.[idx + 2]) / 3
                if luma < lumaTh then
                    if cur = 0 then curTop <- y
                    cur <- cur + 1
                else
                    if cur > best then
                        best <- cur; bestTop <- curTop; bestBot <- y - 1
                    cur <- 0
            if cur > best then
                best <- cur; bestTop <- curTop; bestBot <- y1 - 1
            (best, bestTop, bestBot)
        let regionsArr = regions |> List.toArray
        let mutable totalBars = 0
        for ri in 0 .. regionsArr.Length - 1 do
            let reg = regionsArr.[ri]
            let yT = max 0 reg.topY
            let yB = min h (reg.bottomY + 1)
            if yB > yT then
                let bandH = float (yB - yT)
                // 竖直中点把 band 一分为二；midY 归入下支
                let midY = (yT + yB) / 2
                let upH = float (midY - yT)
                let downH = float (yB - midY)
                // 列投影 + 上支/下支分别投影（luma<200 视为印刷墨）
                let colInk = Array.zeroCreate<int64> w
                let upColInk = Array.zeroCreate<int64> w
                let downColInk = Array.zeroCreate<int64> w
                for y in yT .. yB - 1 do
                    let baseIdx = y * w * 4
                    let isUp = y < midY
                    for x in startX .. w - 1 do
                        let idx = baseIdx + x * 4
                        let luma = (int arr.[idx] + int arr.[idx + 1] + int arr.[idx + 2]) / 3
                        if luma < 200 then
                            colInk.[x] <- colInk.[x] + 1L
                            if isUp then upColInk.[x] <- upColInk.[x] + 1L
                            else downColInk.[x] <- downColInk.[x] + 1L
                let minProj = int64 (0.3 * bandH)
                let minUp = int64 (0.3 * upH)
                let minDown = int64 (0.3 * downH)
                // 判据1: 列投影局部峰值提名候选列
                let candidates =
                    [| for x in startX .. w - 1 do
                        if colInk.[x] >= minProj &&
                           (x = startX || colInk.[x] >= colInk.[x - 1]) &&
                           (x = w - 1 || colInk.[x] > colInk.[x + 1]) then
                            yield x |]
                // 判据2+3: 子带双峰值 + 连续性（maxRun >= 0.4*bandH）
                let minRun = 0.4 * bandH
                let pass234 =
                    [| for x in candidates do
                        if upColInk.[x] >= minUp && downColInk.[x] >= minDown then
                            let runLen, _, _ = longestVerticalRun x yT yB 200
                            if float runLen >= minRun then
                                yield x |]
                // 判据5（v6）: 列宽严格性——真实小节线是孤立 1~2px 竖线。
                //   对候选列 x，以 colInk[x] 的 50% 为阈值，向左右扩展到
                //   阈值以下。"有效列宽"必须 ≤ 2px。
                //   阈值由 v5 的 30% 提升到 50%: 某些小节线恰与五线谱某条横线
                //   相交，横线在 ±1 列贡献约 5~10px 墨。30% 阈值(≈峰值/3) 会把
                //   这类邻列纳入 → 列宽扩到 3~4px 误杀真实小节线；50% 阈值
                //   (≈峰值/2) 下横线穿越贡献远低于半峰值，扩不过去。
                //   - 真实小节线: lineWidth = 1~2px，过。
                //   - 单 notehead: lineWidth = 3~5px，滤掉。
                //   - 16/32 分 notehead 串: lineWidth = 6~15px，滤掉。
                //   - 4/4 宽和弦块/谱号笔画: lineWidth ≥ 20px，滤掉。
                //   此判据与判据1~3 完全正交，专治"判据2+3 都过、但本不该是
                //   小节线"的宽墨团（第4页密集 16/32 分音符漏网项）。
                let maxLineWidth = 2
                let accepted =
                    [| for x in pass234 do
                        if lineWidthStrict colInk x startX w <= maxLineWidth then yield x |]
                // 结束线兜底（v6）: 结束线通常是双竖线(细线+粗线)，本身宽 4~6px，
                //   lineWidth >2 会被判据5 误杀。在段最右端 15% 宽度内，凡过判据
                //   1~3 的候选列、且尚未入选 accepted 的，取最右那根作为结束线追加。
                let endBarCandidates =
                    let segLeft = startX
                    let segRight = w - 1
                    let endZoneStart = segLeft + int (0.85 * float (segRight - segLeft))
                    [| for x in pass234 do
                        if x >= endZoneStart && not (Array.contains x accepted) then
                            yield x |]
                let finalAccepted =
                    if endBarCandidates.Length > 0 then
                        Array.append accepted [| Array.last endBarCandidates |]
                    else
                        accepted
                // 判据4: 相邻距离下限
                let acceptedSorted = finalAccepted |> Array.sort
                let mutable lastX = -100000
                let mutable drawnThis = 0
                let regionBars = ResizeArray<int>()
                for x in acceptedSorted do
                    if x - lastX >= minDist then
                        canvas.DrawLine(float32 x, float32 yT,
                                        float32 x, float32 (yB - 1), bluePaint)
                        lastX <- x
                        drawnThis <- drawnThis + 1
                        regionBars.Add(x)
                barLinesPerRegion.Add(regionBars |> Seq.toList)
                totalBars <- totalBars + drawnThis
                let widthFiltered =
                    pass234 |> Array.filter (fun x -> lineWidthStrict colInk x startX w <= 2) |> Array.length
                output (sprintf "[小节线] 段#%d bandH=%.0f 上支高=%.0f 下支高=%.0f 候选=%d 判据2-3=%d 列宽过滤后=%d 结束线兜底=%d 绘制=%d"
                                  ri bandH upH downH candidates.Length pass234.Length
                                  widthFiltered endBarCandidates.Length drawnThis)
        output (sprintf "[小节线] 完成：%d 段共绘制 %d 根蓝色竖线" regionsArr.Length totalBars)
        (out, barLinesPerRegion |> Seq.toList)

// ===================== 位图缩放（用于预览清晰化） =====================

/// 将位图按 scale 倍放大（双线性/双三次），用于预览 2x 清晰化与截图。
// scaleBitmap 实现已迁移至 jCQT.AI.ImgProc，这里仅做转发以保持 SheetMusic 公开 API 稳定。
/// scale <= 1.0 直接返回原图（不复制，避免无谓开销）。
/// 用于将带直方图/框的预览位图整体放大，使 Image 控件以更大尺寸渲染时仍保持清晰。
let scaleBitmap (src: SKBitmap) (scale: float) =
    jCQT.AI.ImgProc.scaleBitmap src scale

// ===================== 背景连通分量：高低声部间隙检测（实验性） =====================
// 通用 4-连通 Union-Find（ufFind/ufUnion）已迁移至 jCQT.AI.ImgProc，detectStaffGapBy* 通过 open 复用。

/// 用背景连通分量检测高低声部之间的间隙。
/// 对每个 StaffRegion 切片做二值化（luma > 220 为背景），
/// 在背景白区上运行 4-连通 Union-Find，取横贯切片中部 35%~95% 的最宽分量 = 高低声部间隙。
/// 返回每个 region 对应的间隙 Y 范围（页面坐标），None 表示该 region 中未检测到间隙。
let detectStaffGapByBgComponents (page: SKBitmap) (regions: StaffRegion list) =
    let w = page.Width
    let (_, arr) = copyPixels page
    let debug = System.Environment.GetEnvironmentVariable("GAP_DEBUG") <> null

    [ for reg in regions do
        let yT = max 0 reg.topY
        let yB = min page.Height reg.bottomY
        let sliceH = yB - yT
        if sliceH <= 0 then
            yield None
        else
            let n = w * sliceH

            // 1) 二值化：luma > 220 → 背景 true，否则 → 墨迹 false
            //    阈值放宽到 220 以包含抗锯齿边缘（仍 < 印墨 luma 200+）
            let isBg = Array.zeroCreate<bool> n
            let mutable row = 0
            while row < sliceH do
                let pageY = yT + row
                let baseIdx = pageY * w * 4
                let rowOff = row * w
                let mutable col = 0
                while col < w do
                    let idx = baseIdx + col * 4
                    let luma = (int arr.[idx] + int arr.[idx + 1] + int arr.[idx + 2]) / 3
                    isBg.[rowOff + col] <- luma > 220
                    col <- col + 1
                row <- row + 1

            // 2) 4-连通 Union-Find on 背景像素
            let parent = Array.init n id
            let inline pixIdx r c = r * w + c
            let mutable row = 0
            while row < sliceH do
                let mutable col = 0
                while col < w do
                    let i = pixIdx row col
                    if isBg.[i] then
                        // 左邻
                        if col > 0 && isBg.[i - 1] then
                            ufUnion parent i (i - 1)
                        // 上邻
                        if row > 0 && isBg.[i - w] then
                            ufUnion parent i (i - w)
                    col <- col + 1
                row <- row + 1

            // 3) 统计每个分量：面积 + 边界框 (minX,maxX,minY,maxY) 相对 slice
            let compArea = System.Collections.Generic.Dictionary<int, int>()
            let compBB = System.Collections.Generic.Dictionary<int, (int*int*int*int)>()
            let mutable row = 0
            while row < sliceH do
                let mutable col = 0
                while col < w do
                    let i = pixIdx row col
                    if isBg.[i] then
                        let root = ufFind parent i
                        // 面积
                        match compArea.TryGetValue(root) with
                        | true, v -> compArea.[root] <- v + 1
                        | false, _ -> compArea.[root] <- 1
                        // 边界框
                        match compBB.TryGetValue(root) with
                        | true, (mx, maxX, my, maxY) -> compBB.[root] <- (min mx col, max maxX col, min my row, max maxY row)
                        | false, _ -> compBB.[root] <- (col, col, row, row)
                    col <- col + 1
                row <- row + 1

            // 4) 收集所有"横贯式"白区分量（宽 ≥ 35% 页宽），按面积降序，挑出中部最合理的
            //    Y 范围 [5%, 95%] 避免与切片边缘的留白混淆
            let candidates =
                [| for KeyValue(root, (minX, maxX, minY, maxY)) in compBB do
                    let compW = maxX - minX + 1
                    let area = compArea.[root]
                    if compW >= int (0.35 * float w) &&
                       float minY > 0.05 * float sliceH &&
                       float maxY < 0.95 * float sliceH then
                        yield (area, minY + yT, maxY + yT, compW, minY, maxY) |]
                |> Array.sortByDescending (fun (a, _, _, _, _, _) -> a)

            if debug then
                let yMid = float sliceH / 2.0
                let top5 = candidates |> Array.truncate 5
                printfn "[gap] reg(top=%d,bot=%d,h=%d) w=%d candidates=%d top5: %s"
                        reg.topY reg.bottomY sliceH w candidates.Length
                        (top5 |> Array.map (fun (a, gt, gb, cw, my, My) ->
                            sprintf "area=%d y=[%d,%d] mid=%.1f w=%d" a gt gb ((float my + float My) / 2.0) cw)
                         |> String.concat " | ")

            if candidates.Length > 0 then
                // 多个候选时优先选"最居中"的（避免被外部留白连成的"假大分量"骗到）
                let sliceCenter = float (yT + yB) / 2.0
                let best =
                    candidates
                    |> Array.minBy (fun (_, gt, gb, _, _, _) ->
                        let mid = float (gt + gb) / 2.0
                        // 距切片中心的像素距离：值越小越居中
                        abs (mid - sliceCenter))
                let _, gapTop, gapBot, _, _, _ = best
                yield Some (gapTop, gapBot)
            else
                yield None ]

/// 在位图上用半透明青色填充检测到的谱表间隙区域。
/// gaps 来自 detectStaffGapByInkComponents（每元素为 (gapTopY, gapBotY) option）。
/// 返回新位图（原 bmp 不被修改）。
let drawStaffGaps (bmp: SKBitmap) (gaps: (int * int) option list) =
    let out = bmp.Copy(SKColorType.Bgra8888)
    use canvas = new SKCanvas(out)
    // 50% 不透明度的青色填充 + 顶部/底部各画一条更深的青色边线，便于识别边界
    use fillPaint = new SKPaint(
        Color = SKColor(0uy, 255uy, 255uy, 128uy),
        Style = SKPaintStyle.Fill,
        IsAntialias = false)
    use edgePaint = new SKPaint(
        Color = SKColor(0uy, 200uy, 230uy, 220uy),
        Style = SKPaintStyle.Stroke,
        StrokeWidth = 1.5f,
        IsAntialias = false)
    for gapOpt in gaps do
        match gapOpt with
        | Some (yT, yB) when yB > yT ->
            let top = max 0.0f (float32 yT)
            let bot = min (float32 out.Height) (float32 yB)
            if bot > top then
                canvas.DrawRect(0.0f, top, float32 out.Width, bot - top, fillPaint)
                canvas.DrawLine(0.0f, top, float32 out.Width, top, edgePaint)
                canvas.DrawLine(0.0f, bot - 1.0f, float32 out.Width, bot - 1.0f, edgePaint)
        | _ -> ()
    out

// ===================== 墨迹连通分量：高低声部间隙检测 v2 =====================
// 通用墨迹 4-连通分量分析 findInkComponentsInSubregion 已迁移至 jCQT.AI.ImgProc，
// detectStaffGapByInkComponents 通过 open 复用。

/// 用墨迹连通分量的垂直中心分布检测高低声部之间的间隙（v2 算法）。
/// 步骤：
///   1) 按 barLines 把 region 切分（每两个相邻小节线间为一片）
///   2) 每片内做墨迹 CCA，过滤小分量（噪声）
///   3) 收集所有分量垂直中心 → 1D 直方图
///   4) 在切片中部 30%-70% 找最低密度 Y = 间隙中心
///   5) 间隙范围 = bestY ± 4% sliceH
/// barLines 与 regions 一一对应（来自 markBarLines 返回值）。
let detectStaffGapByInkComponents
        (page: SKBitmap) (regions: StaffRegion list) (barLines: int list list) =
    let w = page.Width
    let (_, arr) = copyPixels page
    let debug = System.Environment.GetEnvironmentVariable("GAP_DEBUG") <> null

    [ for (reg, bars) in List.zip regions barLines do
        let yT = max 0 reg.topY
        let yB = min page.Height reg.bottomY
        let sliceH = yB - yT
        if sliceH <= 0 then
            yield None
        else
            // 切片边界：[0, bar1, bar2, ..., w-1]
            let xs = (0 :: (w - 1) :: bars) |> List.sort |> List.toArray
            // 排除小节线自身（约 2px 半宽）
            let barHalfW = max 2 (w / 200)
            // 启发式：分量面积下限（避免污点和极小符号污染直方图）
            let minArea = max 6 (w / 80)

            // 收集每片内墨迹分量的垂直跨度（相对 region topY）
            let intervals = ResizeArray<int * int>()
            for i in 0 .. xs.Length - 2 do
                let xL = xs.[i] + barHalfW
                let xR = min (w - 1) (xs.[i + 1] - barHalfW)
                if xR > xL then
                    let comps = findInkComponentsInSubregion arr w xL xR yT (yB - 1) minArea
                    for (_, _, minY, maxY, _) in comps do
                        intervals.Add((minY - yT, maxY - yT))

            if intervals.Count < 5 then
                if debug then
                    printfn "[gap-ink] reg(top=%d,bot=%d) too few components: %d"
                            reg.topY reg.bottomY intervals.Count
                yield None
            else
                // 按 minY 排序，合并重叠/贴邻（≤3px）的分量
                let sorted = intervals |> Seq.sortBy fst |> Seq.toArray
                let merged = ResizeArray<int * int>()
                let mutable curMin, curMax = sorted.[0]
                for i in 1 .. sorted.Length - 1 do
                    let nMin, nMax = sorted.[i]
                    if nMin - curMax <= 3 then
                        curMax <- max curMax nMax
                    else
                        merged.Add((curMin, curMax))
                        curMin <- nMin
                        curMax <- nMax
                merged.Add((curMin, curMax))

                // 在中部 [30%, 70%] 范围内，找相邻合并区间间最大空隙作为谱间间隙
                let searchT = int (0.3 * float sliceH)
                let searchB = int (0.7 * float sliceH)
                let mutable bestGap = -1
                let mutable bestTop = -1
                let mutable bestBot = -1
                for i in 0 .. merged.Count - 2 do
                    let prevMin, prevMax = merged.[i]
                    let nextMin, nextMax = merged.[i + 1]
                    let gapMid = (prevMax + nextMin) / 2
                    if gapMid >= searchT && gapMid <= searchB then
                        let gapW = nextMin - prevMax
                        if gapW > bestGap then
                            bestGap <- gapW
                            bestTop <- prevMax + yT
                            bestBot <- nextMin + yT

                if bestGap < 0 then
                    if debug then
                        printfn "[gap-ink] reg(top=%d,bot=%d) no valid gap in [30%%,70%%]" reg.topY reg.bottomY
                    yield None
                else
                    if debug then
                        printfn "[gap-ink] reg(top=%d,bot=%d,h=%d) comps=%d merged=%d gapW=%d gap=[%d,%d]"
                                reg.topY reg.bottomY sliceH intervals.Count merged.Count bestGap bestTop bestBot
                    yield Some (bestTop, bestBot) ]
