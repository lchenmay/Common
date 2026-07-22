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
