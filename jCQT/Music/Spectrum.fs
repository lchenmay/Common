module jCQT.Music.Spectrum

open System
open MathNet.Numerics.IntegralTransforms
open SkiaSharp

// ============================================================================
//  频谱 DSP：滑动窗 + Hann + FFT（Math.NET）+ 对数频率轴声谱图 + 时刻切片
//  依赖：MathNet.Numerics（FFT）、SkiaSharp（绘制）
// ============================================================================

/// 预计算的整段声谱图（frames[frame, bin] = dB 幅值，线性 bin 索引）
type Spectrogram = {
    frames: float[,]
    sr: int
    win: int
    hop: int
    nBins: int
    duration: float
}

/// Hann 窗
let private hann (n: int) : float[] =
    Array.init n (fun i ->
        let x = float i / float (max 1 (n - 1))
        0.5 - 0.5 * Math.Cos(2.0 * Math.PI * x))

/// 预计算整段声谱图：滑动窗 → FFT → 20log10，dB 限幅 [-90, 0]
let compute (pcm: float32[]) (sr: int) (win: int) (hop: int) : Spectrogram =
    // 去直流（DC 偏移）：减去整体均值，消除最低频 bin 的时间无关恒定成分
    let dcMean = Array.average pcm
    let pcm = pcm |> Array.map (fun v -> v - dcMean)
    let nSamples = Array.length pcm
    let nFrames = max 1 ((nSamples - win) / hop + 1)
    let nBins = win / 2 + 1
    let frames = Array2D.zeroCreate<float> nFrames nBins
    let window = hann win
    let cx = Array.zeroCreate<System.Numerics.Complex> win
    for f in 0 .. nFrames - 1 do
        let start = f * hop
        for i in 0 .. win - 1 do
            let s = if start + i < nSamples then float pcm.[start + i] else 0.0
            cx.[i] <- System.Numerics.Complex(s * window.[i], 0.0)
        Fourier.Forward(cx, FourierOptions.Default)
        for b in 0 .. nBins - 1 do
            let mag = cx.[b].Magnitude
            let db = if mag > 1e-9 then 20.0 * Math.Log10 mag else -90.0
            frames.[f, b] <- max -90.0 db
    { frames = frames; sr = sr; win = win; hop = hop; nBins = nBins
      duration = float nSamples / float sr }

/// 帧索引 → 时间（秒）
let frameTime (sg: Spectrogram) (fi: int) : float =
    float (fi * sg.hop) / float sg.sr

/// bin 索引 → 频率（Hz，线性轴）
let binFreq (sg: Spectrogram) (bi: int) : float =
    float bi * float sg.sr / float sg.win

/// 取时刻 t（秒）对应帧的 1D 幅值向量（切片谱线用）
let sliceAt (sg: Spectrogram) (t: float) : float[] =
    let nf = Array2D.length1 sg.frames
    let fi = int (t * float sg.sr / float sg.hop)
    let fi = max 0 (min (nf - 1) fi)
    Array.init sg.nBins (fun b -> sg.frames.[fi, b])

// ---------------------------------------------------------------------------
//  拍点检测：时域音量峰值法（见下方 volumeEnvelope / detectBeatsByVolume）
//  以及通用的 tempo 估计（estimateTempo，基于任意包络的自相关）
// ---------------------------------------------------------------------------

/// 基于 flux 自相关的 tempo 估计（BPM），搜索范围 30–300 BPM
/// 取自相关峰值对应的 lag 换算 BPM；lag 越大周期越长 → BPM 越低
let estimateTempo (sg: Spectrogram) (flux: float[]) : float =
    let sr = float sg.sr
    let hop = float sg.hop
    // framesPerBeat = sr/hop * 60/BPM；30..300 BPM 对应的 lag（帧）范围
    let lagMin = max 1 (int (ceil  (sr / hop * 60.0 / 300.0)))
    let lagMax =        int (floor (sr / hop * 60.0 /  30.0))
    let nf = flux.Length
    if nf <= lagMax || lagMax <= lagMin then 0.0
    else
        let mean = Array.average flux
        let z = flux |> Array.map (fun v -> v - mean)   // 零均值
        let mutable bestLag = lagMin
        let mutable bestAc = -infinity
        for lag in lagMin .. min lagMax (nf - 1) do
            let mutable ac = 0.0
            for i in 0 .. nf - 1 - lag do
                ac <- ac + z.[i] * z.[i + lag]
            if ac > bestAc then
                bestAc <- ac
                bestLag <- lag
        60.0 * sr / (float bestLag * hop)

// ---------------------------------------------------------------------------
//  时域音量峰值检测（替代频谱通量法，拍点更稀疏、更贴合"响度突起"）
//  直接在原始 PCM 上按 spectrogram 帧网格计算 RMS 能量包络，对其做峰值选取
// ---------------------------------------------------------------------------

/// 时域音量包络：按 spectrogram 帧网格（hop/win）计算每帧 RMS 能量（"音量"度量）
/// 直接吃原始 PCM，不依赖 STFT；帧 f 窗口 [f*hop, f*hop+win)
let volumeEnvelope (pcm: float32[]) (sg: Spectrogram) : float[] =
    let nf = max 1 ((pcm.Length - sg.win) / sg.hop + 1)
    let env = Array.zeroCreate<float> nf
    for f in 0 .. nf - 1 do
        let start = f * sg.hop
        let mutable s = 0.0
        for i in 0 .. sg.win - 1 do
            let x = if start + i < pcm.Length then float pcm.[start + i] else 0.0
            s <- s + x * x
        env.[f] <- sqrt (s / float sg.win)
    env

/// 一维时序高斯模糊（卷积，边界 clamp），用于平滑音量包络，
/// 避免同一宽峰上出现多个局部极大被重复识别为拍点
let private gaussianKernel (sigma: float) : float[] =
    let r = max 1 (int (ceil (3.0 * sigma)))
    let k = Array.init (2 * r + 1) (fun i ->
        let x = float (i - r)
        exp (-(x * x) / (2.0 * sigma * sigma)))
    let s = Array.sum k
    k |> Array.map (fun v -> v / s)

let gaussianSmooth1D (sigma: float) (arr: float[]) : float[] =
    if sigma <= 0.0 || arr.Length = 0 then arr
    else
        let k = gaussianKernel sigma
        let r = (k.Length - 1) / 2
        let out = Array.zeroCreate<float> arr.Length
        for i in 0 .. arr.Length - 1 do
            let mutable s = 0.0
            for j in 0 .. k.Length - 1 do
                let idx = max 0 (min (arr.Length - 1) (i + j - r))   // 边界 clamp
                s <- s + arr.[idx] * k.[j]
            out.[i] <- s
        out

/// 基于时域音量峰值的拍点检测：先对音量包络做时序高斯模糊，再做【绝对峰值 + 相对峰值】结合选取
/// 高斯模糊（σ 帧）用于在"取峰值之前"合并同一宽峰上的相邻抖动，避免同一峰值被多次识别。
/// 峰值选取用两套门限的结合：
///   • 相对峰值：短窗近邻均值倍数（muShort）——找局部突起，抑制过检/重复识别；
///   • 绝对峰值：本乐段背景响度基线（长窗均值 × muLong）+ 全曲绝对底噪 absFloor——
///     背景取"本乐段"而非全曲最强峰，故 pp 弱奏乐段也能检出，而静音段因低于底噪被排除。
/// 只有同时满足相对与绝对门限的局部极大才记为拍点。
let detectBeatsByVolume (pcm: float32[]) (sg: Spectrogram) : float[] =
    let raw = volumeEnvelope pcm sg
    let sigma = 3.0                    // 高斯 σ（帧）：合并同一宽峰的相邻抖动，不宜过大否则抹平峰值
    let env = gaussianSmooth1D sigma raw
    let nf = env.Length
    if nf < 5 then [||]
    else
        let gMax  = Array.max env
        let gMean = Array.average env
        // 绝对底噪：基于全曲均值（而非全曲最强峰）的一个低比例，仅用于排除静音/底噪段；
        // 不随强奏段伸缩，故不会误砍 pp 弱奏乐段（pp 只要发声即高于底噪）
        let absFloor = max (gMean * 0.15) (if gMax <= 0.0 then 0.0 else gMax * 0.02)
        let winShort = 8               // 短窗半窗（帧）：捕捉近邻相对突起 → 相对峰值
        let winLong  = 40              // 长窗半窗（帧≈0.9s）：估计本乐段背景响度 → 绝对峰值参考
        let muShort  = 1.3             // 相对门限倍数：高于近邻均值 1.3 倍
        let muLong   = 1.1             // 背景门限倍数：高于本乐段背景 1.1 倍
        let minGap   = 6               // 最小间隔帧（≈70ms @22050/256）→ 去抖，抑制连续过检
        let mutable last = -100
        [| for f in winShort .. nf - 1 - winShort do
            // 在模糊后包络上取局部极大
            if env.[f] >= env.[f - 1] && env.[f] >= env.[f + 1] then
                // 短窗近邻均值（相对峰值参考）
                let mutable s = 0.0
                for k in f - winShort .. f + winShort do s <- s + env.[k]
                let shortMean = s / float (2 * winShort + 1)
                // 长窗背景均值（本乐段绝对基线）
                let lo = max 0 (f - winLong)
                let hi = min (nf - 1) (f + winLong)
                let mutable sl = 0.0
                for k in lo .. hi do sl <- sl + env.[k]
                let longMean = sl / float (hi - lo + 1)
                let thrRel = muShort * shortMean               // 相对峰值门限
                let thrAbs = max absFloor (muLong * longMean)  // 绝对/背景门限
                if env.[f] >= thrRel && env.[f] >= thrAbs && f - last >= minGap then
                    last <- f
                    yield frameTime sg f |]

// ---------------------------------------------------------------------------
//  纯时域钢琴触键点（onset）检测：不依赖任何频域变换（无 FFT/STFT/梅尔）。
//  原始 PCM → 短时 RMS 能量包络 → 一阶差分(半波整流) → 动态门限寻峰 + 前沿回溯。
//  击弦的本质是"振幅变化率爆发"而非"振幅大"，故天然避开余音/延音踏板背景误检，
//  对 pp 弱奏与 legato 连奏比 detectBeatsByVolume（找"振幅大"）更鲁棒。
//  复杂度 O(N)，适合实时低延迟场景。
//  可调参数版本为 detectOnsetsTimeDomainWith；detectOnsetsTimeDomain 是其默认封装。
// ---------------------------------------------------------------------------

/// 纯时域 onset 检测的可调参数版本。
/// frameMs/hopMs=能量窗/步长(ms)，alpha=门限灵敏度(越大越严)，
/// noiseFloorRatio=绝对底噪比例(基于全曲最大差分值，过滤尾音/静音微小抖动)，
/// smooth=包络平滑 σ(帧)，winMs=局部稳健门限统计窗(ms)，minGapMs=最小音符间隔(不应期)。
let detectOnsetsTimeDomainWith (pcm: float32[]) (sr: int)
        (frameMs: float) (hopMs: float) (alpha: float) (noiseFloorRatio: float)
        (smooth: float) (winMs: float) (minGapMs: float) : float[] =
    let frame = max 1 (int (float sr * frameMs / 1000.0))
    let hop   = max 1 (int (float sr * hopMs / 1000.0))
    if pcm.Length < frame || frame <= 0 then [||]
    else
        let nf = (pcm.Length - frame) / hop + 1
        // 步骤1：短时 RMS 能量包络（= 全波整流 + 窗口平滑的等效）
        let env = Array.zeroCreate<float> nf
        for f in 0 .. nf - 1 do
            let s0 = f * hop
            let mutable s = 0.0
            for i in 0 .. frame - 1 do
                let x = float pcm.[s0 + i]
                s <- s + x * x
            env.[f] <- sqrt (s / float frame)
        // 轻高斯平滑（σ 帧），吸收窗边界抖动，但不过度抹平峰值
        let env = if smooth > 0.0 then gaussianSmooth1D smooth env else env
        // 步骤2：一阶差分 + 半波整流（只保留振幅急剧上升的爆发）
        let diff = Array.zeroCreate<float> nf
        for i in 1 .. nf - 1 do
            let d = env.[i] - env.[i - 1]
            diff.[i] <- if d > 0.0 then d else 0.0
        // 全局绝对底噪：基于全曲最大差分值的比例，低于此的波动直接忽略
        let maxDiff = if nf > 0 then Array.max diff else 0.0
        let floor = maxDiff * noiseFloorRatio
        // 步骤3：稳健动态门限（局部 均值 + α·标准差，O(1) 前缀和）+ 局部极大 + 不应期 + 前沿回溯
        let cum  = Array.scan (+) 0.0 diff
        let cum2 = Array.scan (+) 0.0 (diff |> Array.map (fun v -> v * v))
        let win  = max 1 (int (winMs / hopMs))
        let minGap = max 1 (int (minGapMs / hopMs))
        let onsets = ResizeArray<float>()
        let mutable last = -1000000          // 上一次实际标记的 onset 帧（前沿回溯后的 j）
        for i in 1 .. nf - 2 do
            // 不应期：距上一个触键点太近，直接跳过（防单个音符起振段连续重触发）
            if i - last < minGap then ()
            else
                let lo = max 0 (i - win / 2)
                let hi = min nf (i + win / 2)
                let n = float (hi - lo)
                let localMean = (cum.[hi] - cum.[lo]) / n
                let localVar  = (cum2.[hi] - cum2.[lo]) / n - localMean * localMean
                let localStd  = sqrt (max 0.0 localVar)
                // 稳健门限：局部均值 + α·标准差（背景/延音下均值趋零时仍由标准差把门抬高）
                let thr = localMean + alpha * localStd
                if diff.[i] >= floor
                   && diff.[i] > thr
                   && diff.[i] > diff.[i - 1] && diff.[i] >= diff.[i + 1] then
                    // 前沿回溯：从峰值往回退，直到差分值跌到门限 25% 以下，
                    // 定位"振幅开始起飞"的物理击弦起点（而非峰值最高点）
                    let mutable j = i
                    while j - 1 >= 0 && diff.[j - 1] > max floor (thr * 0.25) do
                        j <- j - 1
                    if j - last >= minGap then
                        last <- j
                        onsets.Add(float j * float hop / float sr)
        onsets.ToArray()

/// 纯时域 onset 检测默认封装：frameMs=10, hopMs=5, alpha=2.5, noiseFloorRatio=0.02,
/// smooth=1.0, winMs=500, minGapMs=60。其余语义与 detectOnsetsTimeDomainWith 一致。
let detectOnsetsTimeDomain (pcm: float32[]) (sr: int) : float[] =
    detectOnsetsTimeDomainWith pcm sr 10.0 5.0 2.5 0.02 1.0 500.0 60.0

// ---------------------------------------------------------------------------
//  基于触键点间隔(IOI)的网格量化法估计 1/32 音符基准时长 τ 与全曲 BPM。
//  思路（MIR 经典做法）：人类演奏存在 Rubato，IOI 不会是完美倍数，但绝大部分
//  间隔 Δt_i ≈ k_i·τ（k_i∈{1,2,3,4,6,8,12,16…}）。遍历 τ 候选区间，取使
//  Σ(Δt_i - round(Δt_i/τ)·τ)² 最小者作为最优 τ；再反推四分音符 BPM = 60/(8τ)。
//  另提供 ioiNoteName 把倍数 k 映射为乐理时值名，便于展示。
// ---------------------------------------------------------------------------

/// 把间隔倍数 k 映射为乐理时值名（k = 整数倍 · 1/32 音符）
let ioiNoteName (k: int) : string =
    match k with
    | 1  -> "1/32"
    | 2  -> "1/16"
    | 3  -> "附点1/16"
    | 4  -> "1/8"
    | 6  -> "附点1/8"
    | 8  -> "1/4"
    | 12 -> "附点1/4"
    | 16 -> "1/2"
    | _  -> sprintf "%dx1/32" k

/// 基于 IOI 网格量化的速度/时值估计。
/// onsetTimes: 触键点时刻(秒)；minBpm/maxBpm: 四分音符 BPM 搜索范围。
/// 返回 (tau32nd, bpm, multipliers)：τ(1/32)=秒，bpm=四分音符速度，
///   multipliers[i]=第 i 个有效间隔 Δt_i 取整到最近 k·τ 的倍数 k（时值，1→1/32…）。
let estimateGridByIOI (onsetTimes: float[]) (minBpm: float) (maxBpm: float)
        : float * float * int[] =
    let iois = [| for i in 0 .. onsetTimes.Length - 2 ->
                    onsetTimes.[i + 1] - onsetTimes.[i] |]
    // 剔除极大异常间隔（漏检/长休止符 > 3s）与非法负值
    let valid = iois |> Array.filter (fun d -> d > 0.0 && d < 3.0)
    if valid.Length < 2 then (0.0, 0.0, [||])
    else
        // τ 候选区间：1/32 音符 = 四分音符 / 8；BPM 范围 → τ 范围
        let tauMin = (60.0 / maxBpm) / 8.0
        let tauMax = (60.0 / minBpm) / 8.0
        let nCand = 1000
        let cands =
            [| for k in 0 .. nCand - 1 ->
                tauMin + (tauMax - tauMin) * float k / float (nCand - 1) |]
        let mutable bestTau = cands.[0]
        let mutable bestLoss = infinity
        for tau in cands do
            let mutable s = 0.0
            for d in valid do
                let k = max 1.0 (round (d / tau))
                let e = d - k * tau
                s <- s + e * e
            let loss = s / float valid.Length
            if loss < bestLoss then
                bestLoss <- loss
                bestTau <- tau
        let tau = bestTau
        let bpm = 60.0 / (tau * 8.0)
        let multipliers = valid |> Array.map (fun d -> max 1 (int (round (d / tau))))
        (tau, bpm, multipliers)

/// 生成 IOI 网格估计的可读摘要（多行），供 UI 日志输出
let estimateGridSummary (onsetTimes: float[]) (minBpm: float) (maxBpm: float) : string =
    let tau, bpm, mults = estimateGridByIOI onsetTimes minBpm maxBpm
    let totalIOI = max 0 (onsetTimes.Length - 1)
    if mults.Length = 0 then
        sprintf "🎼 IOI 网格估计：触键点不足（共 %d 个 onset），无法估计" onsetTimes.Length
    else
        let counts = System.Collections.Generic.Dictionary<int, int>()
        for m in mults do
            match counts.TryGetValue m with
            | true, c -> counts.[m] <- c + 1
            | _ -> counts.[m] <- 1
        let dist =
            counts.Keys |> Seq.sort
            |> Seq.map (fun k -> sprintf "%s×%d" (ioiNoteName k) counts.[k])
            |> String.concat "，"
        sprintf "🎼 IOI 网格估计：τ(1/32)=%.1fms，BPM≈%.0f（有效间隔 %d/%d）\n   时值分布：%s"
                (tau * 1000.0) bpm mults.Length totalIOI dist

// ---------------------------------------------------------------------------
//  颜色映射：v ∈ [0,1] → 暗蓝→青→品红→黄→白（类 magma）
// ---------------------------------------------------------------------------
let private colormap (v: float) : byte * byte * byte =
    let v = max 0.0 (min 1.0 v)
    let stops =
        [| (0.00,  5uy,   5uy,  25uy)
           (0.25, 20uy,  70uy, 170uy)
           (0.50, 30uy, 175uy, 180uy)
           (0.75, 240uy, 200uy,  45uy)
           (1.00, 255uy, 255uy, 230uy) |]
    let rec interp i =
        if i >= stops.Length - 1 then
            let (_, r, g, b) = stops.[stops.Length - 1]
            r, g, b
        else
            let (t0, r0, g0, b0) = stops.[i]
            let (t1, r1, g1, b1) = stops.[i + 1]
            if v <= t1 then
                let f = if t1 = t0 then 0.0 else (v - t0) / (t1 - t0)
                byte (float r0 + f * float (int r1 - int r0) + 0.5),
                byte (float g0 + f * float (int g1 - int g0) + 0.5),
                byte (float b0 + f * float (int b1 - int b0) + 0.5)
            else interp (i + 1)
    interp 0

let private dbNorm (db: float) : float = (db + 90.0) / 90.0

/// 把声谱图渲染成 SKBitmap（time→宽, 对数频率轴→高, dB→颜色）
/// 像素格式 BGRA8888，与 UtilAvalonia.SkiaCommon.SKBitmap__WriteableBitmap 对齐
let drawSpectrogram (sg: Spectrogram) (w: int) (h: int) : SKBitmap =
    let bmp = new SKBitmap(w, h, SKColorType.Bgra8888, SKAlphaType.Premul)
    let fmin = max 1.0 (binFreq sg 1)
    let fmax = binFreq sg (sg.nBins - 1)
    let logMin = Math.Log fmin
    let logMax = Math.Log fmax
    let nFrames = Array2D.length1 sg.frames
    for py in 0 .. h - 1 do
        // py=0 顶部 → 高频；py=h-1 底部 → 低频
        let frac = 1.0 - float py / float (max 1 (h - 1))
        let freq = Math.Exp(logMin + frac * (logMax - logMin))
        let bin = int (freq * float sg.win / float sg.sr)
        let bin = max 0 (min (sg.nBins - 1) bin)
        for px in 0 .. w - 1 do
            let fi = int (float px / float (max 1 (w - 1)) * float (nFrames - 1))
            let db = sg.frames.[fi, bin]
            let (r, g, b) = colormap (dbNorm db)
            bmp.SetPixel(px, py, SKColor(r, g, b, 255uy))
    bmp

/// 时域波形图（amplitude vs time，每列 min/max 包络），视频软件风格
/// onsetCols：可选（None 则只画波形），已检测到拍点的像素列（x 坐标），会用红色竖线标记
let drawWaveform (pcm: float32[]) (w: int) (h: int) (onsetCols: int[] option) : SKBitmap =
    let info = SKImageInfo(w, h, SKColorType.Bgra8888, SKAlphaType.Premul)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SKColors.Black)
    let mid = float32 h / 2.0f
    use midPaint = new SKPaint(Color = SKColor(70uy, 70uy, 70uy, 255uy), StrokeWidth = 1f)
    canvas.DrawLine(0f, mid, float32 w, mid, midPaint)
    if pcm.Length > 0 then
        let samplesPerCol = max 1 (pcm.Length / w)
        use wavePaint = new SKPaint(Color = SKColors.Cyan, StrokeWidth = 1f)
        let scale = mid - 2f
        for x in 0 .. w - 1 do
            let s0 = x * samplesPerCol
            let s1 = min pcm.Length ((x + 1) * samplesPerCol)
            let mutable mn = 0.0f
            let mutable mx = 0.0f
            for s in s0 .. s1 - 1 do
                let v = pcm.[s]
                if v < mn then mn <- v
                if v > mx then mx <- v
            let yTop = mid - mx * scale
            let yBot = mid - mn * scale
            canvas.DrawLine(float32 x, yTop, float32 x, yBot, wavePaint)
    // 拍点标记：在波形之上画红色竖线（全高）
    if onsetCols.IsSome then
        use onsetPaint = new SKPaint(Color = SKColors.Red, StrokeWidth = 1.5f, IsAntialias = true)
        for col in onsetCols.Value do
            let x = float32 (max 0 (min (w - 1) col))
            canvas.DrawLine(x, 0f, x, float32 h, onsetPaint)
    use img = surface.Snapshot()
    SKBitmap.FromImage(img)

/// 渲染声谱图 [t0,t1] 时间子区间（时间→宽, 对数频率轴→高, dB→颜色）
/// 仅对时间轴做缩放，频率轴保持全频段不变——波形与频谱都在 zoom 范围内绘制
let drawSpectrogramRange (sg: Spectrogram) (w: int) (h: int) (t0: float) (t1: float) : SKBitmap =
    let bmp = new SKBitmap(w, h, SKColorType.Bgra8888, SKAlphaType.Premul)
    let fmin = max 1.0 (binFreq sg 1)
    let fmax = binFreq sg (sg.nBins - 1)
    let logMin = Math.Log fmin
    let logMax = Math.Log fmax
    let nFrames = Array2D.length1 sg.frames
    let hop = float sg.hop
    let sr = float sg.sr
    let fStart = max 0 (int (t0 * sr / hop))
    let fEnd   = min (nFrames - 1) (int (t1 * sr / hop))
    let span = max 0 (fEnd - fStart)
    for py in 0 .. h - 1 do
        // py=0 顶部 → 高频；py=h-1 底部 → 低频
        let frac = 1.0 - float py / float (max 1 (h - 1))
        let freq = Math.Exp(logMin + frac * (logMax - logMin))
        let bin = int (freq * float sg.win / float sg.sr) |> max 0 |> min (sg.nBins - 1)
        for px in 0 .. w - 1 do
            let fi =
                if span <= 0 then fEnd
                else fStart + int (float px / float (max 1 (w - 1)) * float span)
                |> max fStart |> min fEnd
            let (r, g, b) = colormap (dbNorm sg.frames.[fi, bin])
            bmp.SetPixel(px, py, SKColor(r, g, b, 255uy))
    bmp

/// 渲染时域波形 [t0,t1] 子区间（min/max 包络 + 拍点竖线），onsets 为时刻（秒）
let drawWaveformRange (pcm: float32[]) (sr: int) (w: int) (h: int)
                      (t0: float) (t1: float) (onsets: float[] option) : SKBitmap =
    let info = SKImageInfo(w, h, SKColorType.Bgra8888, SKAlphaType.Premul)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SKColors.Black)
    let mid = float32 h / 2.0f
    use midPaint = new SKPaint(Color = SKColor(70uy, 70uy, 70uy, 255uy), StrokeWidth = 1f)
    canvas.DrawLine(0f, mid, float32 w, mid, midPaint)
    if pcm.Length > 0 then
        let s0 = max 0 (int (t0 * float sr))
        let s1 = min pcm.Length (int (t1 * float sr))
        let samplesPerCol = max 1 ((s1 - s0) / w)
        use wavePaint = new SKPaint(Color = SKColors.Cyan, StrokeWidth = 1f)
        let scale = mid - 2f
        for x in 0 .. w - 1 do
            let a = s0 + x * samplesPerCol
            let b = min pcm.Length (s0 + (x + 1) * samplesPerCol)
            let mutable mn, mx = 0.0f, 0.0f
            for s in a .. b - 1 do
                let v = pcm.[s]
                if v < mn then mn <- v
                if v > mx then mx <- v
            canvas.DrawLine(float32 x, mid - mx * scale, float32 x, mid - mn * scale, wavePaint)
    // 拍点标记：在波形之上画红色竖线（全高），仅绘制落在视图区间内者
    if onsets.IsSome then
        use onsetPaint = new SKPaint(Color = SKColors.Red, StrokeWidth = 1.5f, IsAntialias = true)
        let dur = max 1e-6 (t1 - t0)
        for ot in onsets.Value do
            if ot >= t0 && ot <= t1 then
                let x = float32 w * float32 ((ot - t0) / dur)
                canvas.DrawLine(x, 0f, x, float32 h, onsetPaint)
    use img = surface.Snapshot()
    SKBitmap.FromImage(img)

/// 在时刻 t 画 1D 切片谱线（对数频率轴 + C 音符网格线，无填充折线）
/// tilt: 每倍频程补偿量（dB/oct）。0 = 原始；正值（如 3）= 频率补偿，使宽带音频包络趋平
let drawSlice (tilt: float) (sg: Spectrogram) (t: float) (w: int) (h: int) : SKBitmap =
    let slice = sliceAt sg t
    let fmin = max 1.0 (binFreq sg 1)
    let fmax = binFreq sg (sg.nBins - 1)
    let logMin = Math.Log fmin
    let logMax = Math.Log fmax
    let refFreq = 1000.0
    let info = SKImageInfo(w, h, SKColorType.Bgra8888, SKAlphaType.Premul)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SKColors.Black)
    let xOf (freq: float) =
        float32 (w - 1) * float32 ((Math.Log freq - logMin) / (logMax - logMin))
    let yOf (db: float) =
        float32 h - float32 ((db + 90.0) / 90.0) * float32 h
    // 水平基线（dB = -90 参考线，确保基线水平）
    use basePaint = new SKPaint(Color = SKColor(90uy, 90uy, 90uy, 255uy), StrokeWidth = 1f)
    canvas.DrawLine(0f, float32 h - 1f, float32 w, float32 h - 1f, basePaint)
    // C 音符竖直网格线（C1..C7）
    use gridPaint = new SKPaint(Color = SKColor(70uy, 70uy, 90uy, 255uy), StrokeWidth = 1f)
    use labelPaint = new SKPaint(Color = SKColors.Gray)
    use labelFont = new SKFont(SKTypeface.Default, 10f)
    for midi in 24 .. 12 .. 84 do
        let freq = 440.0 * Math.Pow(2.0, float (midi - 69) / 12.0)
        if freq >= fmin && freq <= fmax then
            let x = xOf freq
            canvas.DrawLine(x, 0f, x, float32 h, gridPaint)
            let oct = (midi / 12) - 1
            canvas.DrawText(sprintf "C%d" oct, x + 2f, float32 h - 4f, labelFont, labelPaint)
    // 频谱折线（对数频率轴 + C 音符网格线，无填充）
    use curvePath = new SKPath()
    let mutable first = true
    let mutable lastX = 0f
    // 沿对数频率轴均匀采样（与 drawSpectrogram 一致）：消除 C1–C3 低频欠采样造成的直斜边
    let nLog = max 256 (w / 2)
    for i in 0 .. nLog - 1 do
        let frac = float i / float (max 1 (nLog - 1))
        let freq = Math.Exp(logMin + frac * (logMax - logMin))
        let bin = int (freq * float sg.win / float sg.sr) |> max 1 |> min (sg.nBins - 1)
        // 该对数采样点覆盖的相邻 bin 区间取最大幅值，避免低频平顶台阶
        let binHi = int (Math.Exp(logMin + float (i + 1) / float (max 1 (nLog - 1)) * (logMax - logMin))
                            * float sg.win / float sg.sr) |> min (sg.nBins - 1)
        let mutable db = slice.[bin]
        for bb in bin .. binHi do
            if slice.[bb] > db then db <- slice.[bb]
        let x = xOf freq
        // 频率补偿：每倍频程 +tilt dB（参考 1kHz），使 1/f 型包络趋平
        let comp = tilt * Math.Log2(freq / refFreq)
        let db = max -90.0 (min 0.0 (db + comp))
        let y = yOf db
        if first then
            curvePath.MoveTo(x, y)
            first <- false
        else
            curvePath.LineTo(x, y)
        lastX <- x
    use curvePaint = new SKPaint(Color = SKColors.Lime, StrokeWidth = 1.5f, IsAntialias = true)
    canvas.DrawPath(curvePath, curvePaint)
    use img = surface.Snapshot()
    SKBitmap.FromImage(img)

// ---------------------------------------------------------------------------
//  色谱图（Chromagram / Pitch Class Profile, PCP）：抹去八度差异，把全频谱
//  折叠到 12 个半音类 (C,C#,D,…,B)。每个 bin 的频率 f → MIDI → pitch class
//  (pc = round(midi) mod 12)，把该 bin 的线性功率(10^(db/10))累加到对应 pc。
//  chromagram: frames×12 矩阵；globalChroma: 全曲累加 L1 归一化 → 调号能量指纹。
// ---------------------------------------------------------------------------

/// 半音类名称（pc 索引 0..11 → 音名）
let pitchClassNames =
    [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |]

/// 单帧频谱 (dB 行向量) → 12 维 Chroma 向量（线性功率累加，抹去八度）
let frameChroma (sg: Spectrogram) (frame: int) : float[] =
    let c = Array.zeroCreate<float> 12
    for b in 1 .. sg.nBins - 1 do          // 跳过 DC (bin 0, f=0)
        let f = binFreq sg b
        if f > 0.0 then
            let midi = 69.0 + 12.0 * Math.Log(f / 440.0) / Math.Log(2.0)
            let pc = ((int (Math.Round midi)) % 12 + 12) % 12
            let power = Math.Pow(10.0, sg.frames.[frame, b] / 10.0)  // dB → 线性功率
            c.[pc] <- c.[pc] + power
    c

/// 整段声谱图 → Chromagram 矩阵 (frames × 12)
let chromagram (sg: Spectrogram) : float[,] =
    let n = Array2D.length1 sg.frames
    let chroma = Array2D.zeroCreate<float> n 12
    for fi in 0 .. n - 1 do
        let row = frameChroma sg fi
        for pc in 0 .. 11 do chroma.[fi, pc] <- row.[pc]
    chroma

/// 全曲全局 Chroma 向量（帧累加后 L1 归一化到 [0,1]）——调号能量指纹
let globalChroma (chroma: float[,]) : float[] =
    let n = Array2D.length1 chroma
    let g = Array.zeroCreate<float> 12
    for fi in 0 .. n - 1 do
        for pc in 0 .. 11 do g.[pc] <- g.[pc] + chroma.[fi, pc]
    let s = Array.sum g
    if s > 0.0 then g |> Array.map (fun v -> v / s) else g

/// Krumhansl–Schmuckler 大调/小调权重轮廓
let private ksMajor = [| 6.35; 2.23; 3.48; 2.33; 4.38; 4.09; 2.52; 5.19; 2.39; 3.66; 2.29; 2.88 |]
let private ksMinor = [| 6.33; 2.68; 3.52; 5.38; 2.60; 3.53; 2.54; 4.75; 3.98; 2.69; 3.34; 3.17 |]

/// 调号判定：globalChroma 与 12 候选根音 × (大/小调) 旋转模板做皮尔逊相关，
/// 返回 (音名, 是否大调, 相关度)
let detectKey (g: float[]) : string * bool * float =
    let corr (weights: float[]) (root: int) : float =
        let prof = Array.init 12 (fun pc -> weights.[(pc - root + 12) % 12])
        let mg = Array.average g
        let mp = Array.average prof
        let mutable num, dg, dp = 0.0, 0.0, 0.0
        for i in 0 .. 11 do
            let a = g.[i] - mg
            let b = prof.[i] - mp
            num <- num + a * b
            dg <- dg + a * a
            dp <- dp + b * b
        if dg > 0.0 && dp > 0.0 then num / sqrt (dg * dp) else 0.0
    let mutable bestRoot, bestMajor, bestCorr = 0, true, -infinity
    for root in 0 .. 11 do
        let cm = corr ksMajor root
        if cm > bestCorr then bestCorr <- cm; bestRoot <- root; bestMajor <- true
        let cn = corr ksMinor root
        if cn > bestCorr then bestCorr <- cn; bestRoot <- root; bestMajor <- false
    pitchClassNames.[bestRoot], bestMajor, bestCorr

/// 把 Chromagram 矩阵渲染成热力图（时间→宽, 12 半音类→高, 能量→颜色，对数压缩）
/// 底=C(pc=0)，顶=B(pc=11)
let drawChromagram (chroma: float[,]) (w: int) (h: int) : SKBitmap =
    let nFrames = Array2D.length1 chroma
    let bmp = new SKBitmap(w, h, SKColorType.Bgra8888, SKAlphaType.Premul)
    let lg x = Math.Log(1.0 + max 0.0 x)
    let mutable mx = 0.0
    for fi in 0 .. nFrames - 1 do
        for pc in 0 .. 11 do mx <- max mx (lg chroma.[fi, pc])
    let norm = if mx > 0.0 then 1.0 / mx else 0.0
    for py in 0 .. h - 1 do
        let pc = max 0 (min 11 (11 - int (0.5 + float py / float (max 1 (h - 1)) * 11.0)))
        for px in 0 .. w - 1 do
            let fi = if nFrames > 1
                     then max 0 (min (nFrames - 1) (int (float px / float (max 1 (w - 1)) * float (nFrames - 1))))
                     else 0
            let v = max 0.0 (min 1.0 (lg chroma.[fi, pc] * norm))
            let (r, g, b) = colormap v
            bmp.SetPixel(px, py, SKColor(r, g, b, 255uy))
    bmp

/// 全局 Chroma 条形图（12 竖条 + 音名标签），配合 detectKey 文本显示
let drawGlobalChroma (gvec: float[]) (w: int) (h: int) : SKBitmap =
    let info = SKImageInfo(w, h, SKColorType.Bgra8888, SKAlphaType.Premul)
    use surface = SKSurface.Create(info)
    let canvas = surface.Canvas
    canvas.Clear(SKColors.Black)
    let bw = float w / 12.0
    use barPaint = new SKPaint(Color = SKColors.Orange, IsAntialias = true)
    use labelPaint = new SKPaint(Color = SKColors.LightGray)
    use labelFont = new SKFont(SKTypeface.Default, 11f)
    for pc in 0 .. 11 do
        let x0 = float32 (pc * int bw)
        let bh = float32 (gvec.[pc] * float h)
        canvas.DrawRect(x0 + 1f, float32 h - bh, float32 bw - 2f, bh, barPaint)
        canvas.DrawText(pitchClassNames.[pc], x0 + float32 bw * 0.15f, float32 h - 2f, labelFont, labelPaint)
    use img = surface.Snapshot()
    SKBitmap.FromImage(img)
