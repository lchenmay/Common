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
