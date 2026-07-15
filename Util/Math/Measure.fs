module Util.MathMeasure

open System.Collections.Concurrent
open System
open System.Numerics

open Util.Perf

[<Measure>]
type L

[<Measure>]
type M

[<Measure>]
type T

[<Measure>]
type Pressure = M / (L * T * T)

[<Measure>]
type psi

[<Measure>]
type MPa

type PressureUnit = 
| Psi of float<psi>
| MPa of float<MPa>

let PressureStandard p =
    match p with
    | Psi v -> v * 0.00689476<MPa/psi>
    | MPa v -> v


