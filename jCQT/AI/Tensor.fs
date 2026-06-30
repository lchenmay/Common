module jCQT.AI.Tensor

#nowarn "64"  // INumber<'T> 泛型函数无实参调用点导致的过度狭义警告，float/int 均可

open System
open System.Numerics
open System.Threading.Tasks

// ============================================================
// Type definitions
// ============================================================

type Scala<'T> = 'T
type Vct<'T> = 'T[]
type Matrix<'T> = 'T[,]
type Tensor3<'T> = 'T[,,]
type Tensor4<'T> = 'T[,,,]

// 并行阈值
let private THRESHOLD_1D : int = 1024
let private THRESHOLD_2D : int = 4096

// ============================================================
// Vct（向量）operations
// ============================================================

module Vct =
    let inline len (v: Vct<'T>) = v.Length

    let inline zeros n : Vct<'T> = Array.init n (fun _ -> LanguagePrimitives.GenericZero<'T>)

    let inline init n (f: int -> 'T) : Vct<'T> = Array.init n f

    let inline ofArray (a: 'T[]) : Vct<'T> = Array.copy a

    let inline get (v: Vct<'T>) i = v.[i]
    let inline set (v: Vct<'T>) i x = v.[i] <- x

    let add (a: Vct<'T>) (b: Vct<'T>) : Vct<'T> when 'T :> INumber<'T> =
        if a.Length <> b.Length then failwithf "Vct.add: length %d vs %d" a.Length b.Length
        let r = Array.zeroCreate a.Length
        if a.Length < THRESHOLD_1D then
            for i = 0 to a.Length - 1 do r.[i] <- a.[i] + b.[i]
        else
            Parallel.For(0, a.Length, fun i -> r.[i] <- a.[i] + b.[i]) |> ignore
        r

    let mul (a: Vct<'T>) (b: Vct<'T>) : Vct<'T> when 'T :> INumber<'T> =
        if a.Length <> b.Length then failwithf "Vct.mul: length %d vs %d" a.Length b.Length
        let r = Array.zeroCreate a.Length
        if a.Length < THRESHOLD_1D then
            for i = 0 to a.Length - 1 do r.[i] <- a.[i] * b.[i]
        else
            Parallel.For(0, a.Length, fun i -> r.[i] <- a.[i] * b.[i]) |> ignore
        r

    let scale (s: 'T) (v: Vct<'T>) : Vct<'T> when 'T :> INumber<'T> =
        let r = Array.zeroCreate v.Length
        if v.Length < THRESHOLD_1D then
            for i = 0 to v.Length - 1 do r.[i] <- v.[i] * s
        else
            Parallel.For(0, v.Length, fun i -> r.[i] <- v.[i] * s) |> ignore
        r

    let inline dot (a: Vct< ^T >) (b: Vct< ^T >) : ^T =
        if a.Length <> b.Length then failwithf "Vct.dot: length %d vs %d" a.Length b.Length
        let mutable s = LanguagePrimitives.GenericZero< ^T >
        for i = 0 to a.Length - 1 do s <- s + a.[i] * b.[i]
        s

    // ---- float-only（需要 exp / sqrt）----
    let sum (v: Vct<float>) : float =
        let mutable s = 0.0
        for i = 0 to v.Length - 1 do s <- s + v.[i]
        s

    let mean (v: Vct<float>) : float = (sum v) / float v.Length

    let sqrt (v: Vct<float>) : Vct<float> =
        let r = Array.zeroCreate v.Length
        if v.Length < THRESHOLD_1D then
            for i = 0 to v.Length - 1 do r.[i] <- Math.Sqrt v.[i]
        else
            Parallel.For(0, v.Length, fun i -> r.[i] <- Math.Sqrt v.[i]) |> ignore
        r

    let reciprocal (v: Vct<float>) : Vct<float> =
        let r = Array.zeroCreate v.Length
        if v.Length < THRESHOLD_1D then
            for i = 0 to v.Length - 1 do r.[i] <- 1.0 / v.[i]
        else
            Parallel.For(0, v.Length, fun i -> r.[i] <- 1.0 / v.[i]) |> ignore
        r

    let softmax (v: Vct<float>) : Vct<float> =
        let mutable maxVal = v.[0]
        for i = 1 to v.Length - 1 do maxVal <- max maxVal v.[i]
        let exps = Array.zeroCreate v.Length
        let mutable sumExp = 0.0
        for i = 0 to v.Length - 1 do
            let e = exp (v.[i] - maxVal)
            exps.[i] <- e
            sumExp <- sumExp + e
        for i = 0 to v.Length - 1 do exps.[i] <- exps.[i] / sumExp
        exps

    let apply (f: float -> float) (v: Vct<float>) : Vct<float> =
        if v.Length < THRESHOLD_1D then Array.map f v
        else Array.Parallel.map f v

// ============================================================
// Mat（矩阵）operations
// ============================================================

module Mat =
    let inline rows (m: Matrix<'T>) = Array2D.length1 m
    let inline cols (m: Matrix<'T>) = Array2D.length2 m

    let inline zeros r c : Matrix<'T> =
        Array2D.init r c (fun _ _ -> LanguagePrimitives.GenericZero<'T>)

    let inline init r c (f: int -> int -> 'T) : Matrix<'T> = Array2D.init r c f

    let inline get (m: Matrix<'T>) i j = m.[i, j]
    let inline set (m: Matrix<'T>) i j x = m.[i, j] <- x

    /// (m×k) × (k×n) → (m×n)
    let matmul (a: Matrix<'T>) (b: Matrix<'T>) : Matrix<'T> when 'T :> INumber<'T> =
        let ma, ka = rows a, cols a
        let kb, nb = rows b, cols b
        if ka <> kb then failwithf "Mat.matmul: (%d,%d) × (%d,%d)" ma ka kb nb
        let zero = LanguagePrimitives.GenericZero<'T>
        let c = zeros ma nb
        if ma * nb < THRESHOLD_2D then
            for i = 0 to ma - 1 do
                for j = 0 to nb - 1 do
                    let mutable s = zero
                    for k = 0 to ka - 1 do
                        s <- s + a.[i, k] * b.[k, j]
                    c.[i, j] <- s
        else
            Parallel.For(0, ma, fun i ->
                for j = 0 to nb - 1 do
                    let mutable s = zero
                    for k = 0 to ka - 1 do
                        s <- s + a.[i, k] * b.[k, j]
                    c.[i, j] <- s
            ) |> ignore
        c

    /// (m×n) → (n×m) — 纯搬移
    let transpose (m: Matrix<'T>) : Matrix<'T> =
        let r, c = rows m, cols m
        Array2D.init c r (fun i j -> m.[j, i])

    let add (a: Matrix<'T>) (b: Matrix<'T>) : Matrix<'T> when 'T :> INumber<'T> =
        let ra, ca = rows a, cols a
        if ra <> rows b || ca <> cols b then failwith "Mat.add: shape mismatch"
        let r = zeros ra ca
        if ra * ca < THRESHOLD_2D then
            for i = 0 to ra - 1 do
                for j = 0 to ca - 1 do
                    r.[i, j] <- a.[i, j] + b.[i, j]
        else
            Parallel.For(0, ra, fun i ->
                for j = 0 to ca - 1 do
                    r.[i, j] <- a.[i, j] + b.[i, j]
            ) |> ignore
        r

    let mul (a: Matrix<'T>) (b: Matrix<'T>) : Matrix<'T> when 'T :> INumber<'T> =
        let ra, ca = rows a, cols a
        if ra <> rows b || ca <> cols b then failwith "Mat.mul: shape mismatch"
        let r = zeros ra ca
        if ra * ca < THRESHOLD_2D then
            for i = 0 to ra - 1 do
                for j = 0 to ca - 1 do
                    r.[i, j] <- a.[i, j] * b.[i, j]
        else
            Parallel.For(0, ra, fun i ->
                for j = 0 to ca - 1 do
                    r.[i, j] <- a.[i, j] * b.[i, j]
            ) |> ignore
        r

    let scale (s: 'T) (m: Matrix<'T>) : Matrix<'T> when 'T :> INumber<'T> =
        let r, c = rows m, cols m
        let result = zeros r c
        if r * c < THRESHOLD_2D then
            for i = 0 to r - 1 do
                for j = 0 to c - 1 do
                    result.[i, j] <- m.[i, j] * s
        else
            Parallel.For(0, r, fun i ->
                for j = 0 to c - 1 do
                    result.[i, j] <- m.[i, j] * s
            ) |> ignore
        result

    /// 沿行方向 softmax — 每行独立 → 并行
    let softmax (m: Matrix<float>) : Matrix<float> =
        let r, c = rows m, cols m
        let result = zeros r c
        Parallel.For(0, r, fun i ->
            let mutable maxVal = m.[i, 0]
            for j = 1 to c - 1 do maxVal <- max maxVal m.[i, j]
            let mutable sumExp = 0.0
            for j = 0 to c - 1 do
                let e = exp (m.[i, j] - maxVal)
                result.[i, j] <- e
                sumExp <- sumExp + e
            for j = 0 to c - 1 do
                result.[i, j] <- result.[i, j] / sumExp
        ) |> ignore
        result

    let sumRows (m: Matrix<float>) : Vct<float> =
        let r, c = rows m, cols m
        let result = Array.zeroCreate r
        Parallel.For(0, r, fun i ->
            let mutable s = 0.0
            for j = 0 to c - 1 do s <- s + m.[i, j]
            result.[i] <- s
        ) |> ignore
        result

    let sumCols (m: Matrix<float>) : Vct<float> =
        let r, c = rows m, cols m
        let result = Array.zeroCreate c
        Parallel.For(0, c, fun j ->
            let mutable s = 0.0
            for i = 0 to r - 1 do s <- s + m.[i, j]
            result.[j] <- s
        ) |> ignore
        result

    /// 按索引 gather 行 — 纯搬移
    let gather (m: Matrix<'T>) (indices: int[]) : Matrix<'T> =
        let _, c = rows m, cols m
        Array2D.init indices.Length c (fun i j -> m.[indices.[i], j])

    let apply (f: float -> float) (m: Matrix<float>) : Matrix<float> =
        let r, c = rows m, cols m
        let result = zeros r c
        if r * c < THRESHOLD_2D then
            for i = 0 to r - 1 do
                for j = 0 to c - 1 do
                    result.[i, j] <- f m.[i, j]
        else
            Parallel.For(0, r, fun i ->
                for j = 0 to c - 1 do
                    result.[i, j] <- f m.[i, j]
            ) |> ignore
        result

// ============================================================
// T3（3 维张量）operations
// ============================================================

module T3 =
    let inline d0 (t: Tensor3<'T>) = Array3D.length1 t
    let inline d1 (t: Tensor3<'T>) = Array3D.length2 t
    let inline d2 (t: Tensor3<'T>) = Array3D.length3 t

    let inline zeros a b c : Tensor3<'T> =
        Array3D.init a b c (fun _ _ _ -> LanguagePrimitives.GenericZero<'T>)

    let inline get (t: Tensor3<'T>) i j k = t.[i, j, k]
    let inline set (t: Tensor3<'T>) i j k x = t.[i, j, k] <- x

    /// batch matmul: (b,m,k) × (b,k,n) → (b,m,n)
    let batchMatmul (a: Tensor3<'T>) (b: Tensor3<'T>) : Tensor3<'T> when 'T :> INumber<'T> =
        let ba, ma, ka = d0 a, d1 a, d2 a
        let bb, kb, nb = d0 b, d1 b, d2 b
        if ba <> bb || ka <> kb then
            failwithf "T3.batchMatmul: (%d,%d,%d) × (%d,%d,%d)" ba ma ka bb kb nb
        let zero = LanguagePrimitives.GenericZero<'T>
        let c = zeros ba ma nb
        Parallel.For(0, ba, fun bi ->
            for i = 0 to ma - 1 do
                for j = 0 to nb - 1 do
                    let mutable s = zero
                    for k = 0 to ka - 1 do
                        s <- s + a.[bi, i, k] * b.[bi, k, j]
                    c.[bi, i, j] <- s
        ) |> ignore
        c

    let add (a: Tensor3<'T>) (b: Tensor3<'T>) : Tensor3<'T> when 'T :> INumber<'T> =
        let a0, a1, a2 = d0 a, d1 a, d2 a
        if a0 <> d0 b || a1 <> d1 b || a2 <> d2 b then failwith "T3.add: shape mismatch"
        let r = zeros a0 a1 a2
        Parallel.For(0, a0, fun i ->
            for j = 0 to a1 - 1 do
                for k = 0 to a2 - 1 do
                    r.[i, j, k] <- a.[i, j, k] + b.[i, j, k]
        ) |> ignore
        r

    let mul (a: Tensor3<'T>) (b: Tensor3<'T>) : Tensor3<'T> when 'T :> INumber<'T> =
        let a0, a1, a2 = d0 a, d1 a, d2 a
        if a0 <> d0 b || a1 <> d1 b || a2 <> d2 b then failwith "T3.mul: shape mismatch"
        let r = zeros a0 a1 a2
        Parallel.For(0, a0, fun i ->
            for j = 0 to a1 - 1 do
                for k = 0 to a2 - 1 do
                    r.[i, j, k] <- a.[i, j, k] * b.[i, j, k]
        ) |> ignore
        r

    let scale (s: 'T) (t: Tensor3<'T>) : Tensor3<'T> when 'T :> INumber<'T> =
        let a, b, c = d0 t, d1 t, d2 t
        let r = zeros a b c
        Parallel.For(0, a, fun i ->
            for j = 0 to b - 1 do
                for k = 0 to c - 1 do
                    r.[i, j, k] <- t.[i, j, k] * s
        ) |> ignore
        r

    /// 沿最后一维 (d2) softmax — (i,j) 平面独立
    let softmax (t: Tensor3<float>) : Tensor3<float> =
        let a, b, c = d0 t, d1 t, d2 t
        let r = zeros a b c
        let totalPairs = a * b
        Parallel.For(0, totalPairs, fun idx ->
            let i = idx / b
            let j = idx % b
            let mutable maxVal = t.[i, j, 0]
            for k = 1 to c - 1 do maxVal <- max maxVal t.[i, j, k]
            let mutable sumExp = 0.0
            for k = 0 to c - 1 do
                let e = exp (t.[i, j, k] - maxVal)
                r.[i, j, k] <- e
                sumExp <- sumExp + e
            for k = 0 to c - 1 do
                r.[i, j, k] <- r.[i, j, k] / sumExp
        ) |> ignore
        r

    /// transpose d1↔d2 — 纯搬移
    let transpose12 (t: Tensor3<'T>) : Tensor3<'T> =
        let a, b, c = d0 t, d1 t, d2 t
        Array3D.init a c b (fun i j k -> t.[i, k, j])

    let apply (f: float -> float) (t: Tensor3<float>) : Tensor3<float> =
        let a, b, c = d0 t, d1 t, d2 t
        let r = zeros a b c
        Parallel.For(0, a, fun i ->
            for j = 0 to b - 1 do
                for k = 0 to c - 1 do
                    r.[i, j, k] <- f t.[i, j, k]
        ) |> ignore
        r

// ============================================================
// T4（4 维张量）operations
// ============================================================

module T4 =
    let inline d0 (t: Tensor4<'T>) = t.GetLength(0)
    let inline d1 (t: Tensor4<'T>) = t.GetLength(1)
    let inline d2 (t: Tensor4<'T>) = t.GetLength(2)
    let inline d3 (t: Tensor4<'T>) = t.GetLength(3)

    let inline zeros a b c d : Tensor4<'T> =
        Array4D.init a b c d (fun _ _ _ _ -> LanguagePrimitives.GenericZero<'T>)

    let inline get (t: Tensor4<'T>) i j k l = t.[i, j, k, l]
    let inline set (t: Tensor4<'T>) i j k l x = t.[i, j, k, l] <- x

    let add (a: Tensor4<'T>) (b: Tensor4<'T>) : Tensor4<'T> when 'T :> INumber<'T> =
        let a0, a1, a2, a3 = d0 a, d1 a, d2 a, d3 a
        if a0 <> d0 b || a1 <> d1 b || a2 <> d2 b || a3 <> d3 b then failwith "T4.add: shape mismatch"
        let r = zeros a0 a1 a2 a3
        Parallel.For(0, a0, fun i ->
            for j = 0 to a1 - 1 do
                for k = 0 to a2 - 1 do
                    for l = 0 to a3 - 1 do
                        r.[i, j, k, l] <- a.[i, j, k, l] + b.[i, j, k, l]
        ) |> ignore
        r

    let mul (a: Tensor4<'T>) (b: Tensor4<'T>) : Tensor4<'T> when 'T :> INumber<'T> =
        let a0, a1, a2, a3 = d0 a, d1 a, d2 a, d3 a
        if a0 <> d0 b || a1 <> d1 b || a2 <> d2 b || a3 <> d3 b then failwith "T4.mul: shape mismatch"
        let r = zeros a0 a1 a2 a3
        Parallel.For(0, a0, fun i ->
            for j = 0 to a1 - 1 do
                for k = 0 to a2 - 1 do
                    for l = 0 to a3 - 1 do
                        r.[i, j, k, l] <- a.[i, j, k, l] * b.[i, j, k, l]
        ) |> ignore
        r

    let scale (s: 'T) (t: Tensor4<'T>) : Tensor4<'T> when 'T :> INumber<'T> =
        let a, b, c, d = d0 t, d1 t, d2 t, d3 t
        let r = zeros a b c d
        Parallel.For(0, a, fun i ->
            for j = 0 to b - 1 do
                for k = 0 to c - 1 do
                    for l = 0 to d - 1 do
                        r.[i, j, k, l] <- t.[i, j, k, l] * s
        ) |> ignore
        r

    /// 沿最后一维 (d3) softmax — (i,j,k) 纤维独立
    let softmax (t: Tensor4<float>) : Tensor4<float> =
        let a, b, c, d = d0 t, d1 t, d2 t, d3 t
        let r = zeros a b c d
        let totalFibers = a * b * c
        Parallel.For(0, totalFibers, fun idx ->
            let i = idx / (b * c)
            let rem = idx % (b * c)
            let j = rem / c
            let k = rem % c
            let mutable maxVal = t.[i, j, k, 0]
            for l = 1 to d - 1 do maxVal <- max maxVal t.[i, j, k, l]
            let mutable sumExp = 0.0
            for l = 0 to d - 1 do
                let e = exp (t.[i, j, k, l] - maxVal)
                r.[i, j, k, l] <- e
                sumExp <- sumExp + e
            for l = 0 to d - 1 do
                r.[i, j, k, l] <- r.[i, j, k, l] / sumExp
        ) |> ignore
        r

    let apply (f: float -> float) (t: Tensor4<float>) : Tensor4<float> =
        let a, b, c, d = d0 t, d1 t, d2 t, d3 t
        let r = zeros a b c d
        Parallel.For(0, a, fun i ->
            for j = 0 to b - 1 do
                for k = 0 to c - 1 do
                    for l = 0 to d - 1 do
                        r.[i, j, k, l] <- f t.[i, j, k, l]
        ) |> ignore
        r

// ============================================================
// 激活函数
// ============================================================

[<AutoOpen>]
module Activations =
    let inline relu x : float = if x > 0.0 then x else 0.0
    let inline gelu x : float =
        0.5 * x * (1.0 + tanh (0.7978845608028654 * (x + 0.044715 * x * x * x)))
    let inline silu x : float = x / (1.0 + exp (-x))
    let inline sigmoid x : float = 1.0 / (1.0 + exp (-x))
