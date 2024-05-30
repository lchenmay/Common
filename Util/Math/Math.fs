module Util.Math

open System.Collections.Generic
open System
open System.Numerics

open Util.Perf


// Int32 ===================

let int32Pow x y = 
    let mutable p = 1
    [| 0..y-1 |]
    |> Array.iter(fun i -> p <- p * x)
    p

// Big number =============================================================

let e2s256 = 
    [| 0 .. 256 |]
    |> Array.map(fun x -> BigInteger.Pow(2,x))

let bin__bigint256 (bin:byte[]) = 
    [| 0 .. bin.Length - 1 |]
    |> Array.map(fun i -> 
        let v = uint8 bin[bin.Length - 1 - i]
        e2s256[i * 8] * (BigInteger.CreateTruncating v))
    |> Array.sum
    
let modulo a b = 
    let r = a % b
    if r < BigInteger.Zero then
        r + b
    else 
        r

type BigIntPoint = {
    x: BigInteger;
    y: BigInteger }

let clone_BigIntPoint pt = { x = pt.x; y = pt.y }

let coords__BigIntPoint x y = { x = x; y = y }
let ints__BigIntPoint (x:int)(y:int) = { x = BigInteger x; y = BigInteger y }

let equalBigIntPoint p q = p.x = q.x && p.y = q.y

// Time series =============================================================

let ma win (src:float[]) = 
    [| 0 .. src.Length - 1 |]
    |> Array.map(fun i -> 
        if(i > win) then
            (src 
            |> Array.take win
            |> Array.sum)/float(win)
        else
            (src 
            |> Array.take(i+1)
            |> Array.sum)/float(i+1))

let maWithBias win src = 
    let ma = ma win src
    let bias = 
        [| 0 .. src.Length - 1 |]
        |> Array.map(fun i -> src.[i] - ma.[i])
    ma,bias

let simpleRegression win src = src

// Number theory =============================================================

let adics p n =
    let mutable x = 1UL
    [| 0.. n - 1 |]
    |> Array.map(fun i -> 
        x <- x * p
        x)

let uint64__adic (adics:uint64[]) (input:uint64) = 

    let mutable r = input
        
    [| 0..adics.Length - 1 |]
    |> Array.map(fun i ->
        let residue = r % adics.[i]
        r <- r - residue
        if i > 0 then
            residue / adics.[i-1]
        else
            residue / 1UL)

let adic__uint64 (adics:uint64[]) (input:uint64[]) = 

    let mutable r = input.[0]
        
    [| 1..adics.Length - 1 |]
    |> Array.iter(fun i ->
        r <- r + input.[i] * adics.[i-1])
        
    r

// Scientific Computing =============================================================

let adicByteAdd length (x:byte[],y:byte[]) = 
        
    let res = Array.zeroCreate length
        
    let mutable within = true

    [| 0 .. length - 1 |]
    |> Array.iter(fun i -> 

        let z = 
            if within then
                (uint16 x[i]) + (uint16 y[i])
            else
                (uint16 x[i]) + (uint16 y[i]) + 1us

        within <- z < 256us
        if within then
            res[i] <- z |> uint8 |> byte
        else
            res[i] <- z - 256us |> uint8 |> byte)

    res

// Galois Field =============================================================

// $\mathbb Z/p \eq GF(p) \eq \mathbb F_p$
// where $p$ is a prime number

// Bezout's Identity ax + by = gcd(a,b)
// gcd(a,b) = 1 iff. a and b coprime iff. 
// ax = 1 (mod b) and by = 1 (mod a)
// extended Euclidean Algorithm
let rec gcdBezout (a:BigInteger,b:BigInteger) = 

    if b.IsZero then
        a, BigInteger.One, BigInteger.Zero
    else
        let gcd,x,y = gcdBezout (b, modulo a b)
        gcd, y, x - (a / b) * y

let gfInv p n = 

    use cw = new CodeWrapper("Util.Math.gfInv")

    let gcd,x,y = gcdBezout (n,p)
    modulo ((modulo x p) + p) p

let gfDiv p x y = modulo (x * (gfInv p y)) p

// Elliptic Curve =============================================================
// y^2 = x^3 + ax + b

// Consider b <> 0 only
// The infy point is defined as (x,y) = (0,0)
type ecPoint = BigInteger * BigInteger

let infy = BigInteger.Zero,BigInteger.Zero

let equalEcPoint P Q = (fst P = fst Q) && (snd P = snd Q)
       
let ecPoint__infy P = fst P = BigInteger.Zero && snd P = BigInteger.Zero

// https://andrea.corbellini.name/2015/05/23/elliptic-curve-cryptography-finite-fields-and-discrete-logarithms/
// https://asecuritysite.com/ecc/ecc_points_add2
let ecGaloisAdd p a (P:ecPoint,Q:ecPoint) = 

    if ecPoint__infy P && ecPoint__infy Q then
        infy
    else if ecPoint__infy P && ecPoint__infy Q = false then
        Q
    else if ecPoint__infy Q && ecPoint__infy P = false then
        P
    else 
        let px,py = P
        let qx,qy = Q

        let mo =
            if equalEcPoint P Q then
                gfDiv p ((BigInteger 3) * px * px + a) ((BigInteger 2) * py)
                |> Some
            else if px <> qx then
                gfDiv p (py - qy) (px - qx) 
                |> Some
            else
                None

        match mo with
        | None -> infy
        | Some m ->
            let rx = modulo (m * m - px - qx)  p
            let ry = modulo (- py - m * (rx - px)) p
            rx,ry

// https://graui.de/code/elliptic2/
(*
    let go p = 
        ecGaloisCalculator 
            (fun (s:string) -> System.Console.WriteLine s) 
            (BigInteger 0) (BigInteger 7) 
            p
    go (BigInteger 5)
    go (BigInteger 11)
*)
let ecGaloisCalculator output a b p = 

    let indices = [| BigInteger.Zero .. p - BigInteger.One |] 

    "y^2 = x^3 + ax + b"
    |> output

    [|  "a = " + a.ToString()
        ", b = " + b.ToString()
        ", p = " + p.ToString() |]
    |> String.Concat
    |> output

    let signualrity = (BigInteger 4) * a * a * a + (BigInteger 27) * b * b
    [|
        "4a^3 + 27b^2 = "
        signualrity.ToString()
        " = " + (signualrity % p).ToString()
        " (mod " + p.ToString() + ")" |]
    |> String.Concat
    |> output

    let pt__coords pt = 
        if ecPoint__infy pt then
            " UNIT "
        else
            let x,y = pt
            [|  "("
                x.ToString()
                ","
                y.ToString() 
                ")" |]
            |> String.Concat

    let point showCoords showPointOnly pt = 
            
        if ecPoint__infy pt then
            " UNIT "
        else
            let x,y = pt
            let gap = x * x * x + a * x + b - y * y  
            let r = gap % p

            let sCoords = pt__coords pt

            let s = 
                [|  
                    if showCoords then sCoords else ""
                    if showPointOnly && r <> BigInteger.Zero then "- - - - - - -" else sCoords
                    // gap.ToString() + " = " + r.ToString() |]
                    |]
                |> String.Concat
            s.PadLeft(20)

    let pts = 
        indices
        |> Array.map(fun x -> 
            indices
            |> Array.map(fun y -> 
                let gap = x * x * x + a * x + b - y * y  
                let r = gap % p
                if r = BigInteger.Zero then 
                    Some(x,y)
                else
                    None)
            |> Array.filter(fun o -> o.IsSome)
            |> Array.map(fun o -> o.Value))
        |> Array.concat

    output ""

    indices
    |> Array.iter(fun y -> 
        indices
        |> Array.map(fun x -> point true false (x,y))
        |> String.Concat
        |> output)

    output ""

    indices
    |> Array.iter(fun y -> 
        indices
        |> Array.map(fun x -> point false true (x,y))
        |> String.Concat
        |> output)

    output ""

    pts
    |> Array.iter (pt__coords >> output)

    output ""

    pts
    |> Array.iter(fun P -> 

        pts
        |> Array.map(fun Q -> 
        
            let sP = pt__coords P
            let sQ = pt__coords Q
            let s = pt__coords (ecGaloisAdd p a (P,Q))

            (sP + "+" + sQ + "=" + s).PadLeft(18))
        |> String.Concat
        |> output)

let round2 (n:float) =
    System.Math.Round(n, 2, MidpointRounding.AwayFromZero)

let roundN round (n:float) =
    System.Math.Round(n, round, MidpointRounding.AwayFromZero)

        

