module Util.Prob

open System.Collections.Generic
open System
open System.Numerics
open System.Security.Cryptography

// Random =============================================================

let ts__rand divisor (ts:TimeSpan) = 
    let s = DateTime.UtcNow.Millisecond % 1000
    let ticks = float(ts.Ticks) * float(s) * 0.001 / divisor
    new TimeSpan(int64 ticks)

let ts__randx(ts:TimeSpan) = ts__rand 1.0 ts

let rand__bigint256 n = 
    let csprng = RandomNumberGenerator.GetBytes 32
    let bint = Math.bin__bigint256 csprng
    bint % n

let array__rand (array:'T[]) = 
    let csprng = RandomNumberGenerator.GetBytes 32
    let bint = Math.bin__bigint256 csprng
    let index = 
        bint % (BigInteger array.Length)
        |> int
    array[index]
    

// Distribution =============================================================

let samples__moments samples = 
    let sum = samples |> Array.sum
    let mean = sum / float(samples.Length)
    let variances = 
        samples 
        |> Array.map(fun i -> (i - mean) * (i - mean))
        |> Array.sum
    mean,Math.Sqrt(variances / float(samples.Length))
