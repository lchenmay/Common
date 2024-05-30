module Util.LinearAlgebra

open System.Collections.Generic
open System


type Matrix = 
    {   
        m: float array2d }

    static member (+) (x:Matrix,y:Matrix) = 
            
        let row = x.m.GetLength 0
        let col = x.m.GetLength 1
            
        let m = Array2D.zeroCreate<float> row col 
        [| 0..row - 1 |]
        |> Array.iter(fun i -> 
            [| 0..col - 1 |]
            |> Array.iter(fun j -> 
                m.[i,j] <- x.m.[i,j] + y.m.[i,j]))

        { m = m }

    static member (-) (x:Matrix,y:Matrix) = 
            
        let row = x.m.GetLength 0
        let col = x.m.GetLength 1
            
        let m = Array2D.zeroCreate<float> row col 
        [| 0..row - 1 |]
        |> Array.iter(fun i -> 
            [| 0..col - 1 |]
            |> Array.iter(fun j -> 
                m.[i,j] <- x.m.[i,j] - y.m.[i,j]))

        { m = m }

    static member (*) (a:float,x:Matrix) = 
            
        let row = x.m.GetLength 0
        let col = x.m.GetLength 1
            
        let m = Array2D.zeroCreate<float> row col 
        [| 0..row - 1 |]
        |> Array.iter(fun i -> 
            [| 0..col - 1 |]
            |> Array.iter(fun j -> 
                m.[i,j] <- a * x.m.[i,j]))

        { m = m }

    static member (*) (x:Matrix,y:Matrix) = 
            
        let row = x.m.GetLength 0
        let col = y.m.GetLength 1

        let contract = x.m.GetLength 1
            
        let m = Array2D.zeroCreate<float> row col 
        [| 0..row - 1 |]
        |> Array.iter(fun i -> 
            [| 0..col - 1 |]
            |> Array.iter(fun j -> 
                m.[i,j] <- 
                    [| 0..contract - 1 |]
                    |> Array.map(fun k -> x.m.[i,k] * y.m.[k,j])
                    |> Array.sum))

        { m = m }

let empty__Matrix row col = { m = Array2D.zeroCreate<float> row col }
let empty__ColVector row = empty__Matrix row 1
let empty__RowVector col = empty__Matrix 1 col
let empty__MatrixEndo n = { m = Array2D.zeroCreate<float> n n }

(* e.g. 
let x = empty__Matrix 3 3
let y = empty__Matrix 3 3
let z1 = x + y
let z2 = x - y
let w1 = 3.4 * x
let w2= x * y
//*)
    
type Vct = 
    {
        m: float[] }

    static member (+) (x:Vct,y:Vct) = 
            
        let row = x.m.Length
            
        let m = Array.zeroCreate<float> row
        [| 0..row - 1 |]
        |> Array.iter(fun i -> 
            m.[i] <- x.m.[i] + y.m.[i])

        { m = m }

    static member (-) (x:Vct,y:Vct) = 
            
        let row = x.m.Length
            
        let m = Array.zeroCreate<float> row
        [| 0..row - 1 |]
        |> Array.iter(fun i -> 
            m.[i] <- x.m.[i] - y.m.[i])

        { m = m }

    static member (*) (a:float,x:Vct) = 
            
        let row = x.m.Length
            
        let m = Array.zeroCreate<float> row
        [| 0..row - 1 |]
        |> Array.iter(fun i -> 
            m.[i] <- a * x.m.[i])

        { m = m }

let mean (vs:Vct[]) = 
    let row = vs.[0].m.Length
    let mutable v = { m = Array.zeroCreate<float> row }
        
    vs
    |> Array.iter(fun i -> v <- v + i)

    (1.0 / (float vs.Length)) * v

// 向量组 vs 归一化到 [-1,1]^n 的单位立方体中
let vcts__normalize vs = 
    let mean =  mean vs
    let centralized = 
        vs
        |> Array.map(fun v -> v - mean)

    let row = mean.m.Length
    let ranges = Array.zeroCreate<float> row
    [| 0..row - 1|]
    |> Array.iter(fun i ->
        let ms = centralized |> Array.map(fun v -> v.m.[i])
        ranges.[i] <-
            [|  ms |> Array.min |> Math.Abs;
                ms |> Array.min |> Math.Abs |]
            |> Array.max)

    centralized
    |> Array.iter(fun v -> 
        [| 0..row - 1|]
        |> Array.iter(fun i -> 
            v.m.[i] <- v.m.[i] / ranges.[i]))

let distanceEuclidean(v1,v2) = 

    let squareSum = 
        [| 0 .. v1.m.Length - 1 |]
        |> Array.map(fun i -> 
            let d = v1.m.[i] - v2.m.[i]
            d * d)
        |> Array.sum

    Math.Sqrt squareSum
