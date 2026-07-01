module jCQT.AI.Tensor

#nowarn "64"  // INumber<'T> 泛型函数无实参调用点导致的过度狭义警告，float/int 均可

open System
open System.Numerics
open System.Threading.Tasks

type Scalar<'T> = 'T[]
type Vct<'T> = 'T[]
type Matrix<'T> = 'T[,]
type Tensor3<'T> = 'T[,,]
type Tensor4<'T> = 'T[,,,]

type Tensor<'T> = 
| Trival
| Scalar of Scalar<'T>
| Vct of Vct<'T>
| Matrix of Matrix<'T>
| Tensor3 of Tensor3<'T>
| Tensor4 of Tensor4<'T>

open System.Numerics

let inline add a b = 
    match a, b with
    | Scalar x, Scalar y -> 
        [| x.[0] + y.[0] |] |> Scalar

    | Vct x, Vct y -> 
        Array.init x.Length (fun i -> x.[i] + y.[i])
        |> Vct

    | Matrix x, Matrix y ->
        let r, c = Array2D.length1 x, Array2D.length2 x
        Array2D.init r c (fun i j -> x.[i, j] + y.[i, j])
        |> Matrix

    | _ -> Trival
