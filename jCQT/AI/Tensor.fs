module jCQT.AI.Tensor

type Tensor = {
dims: int[]
dim__indices: int[][]
dataIndices: int[]
data: float[] }

let dimes__Tensor dimes =

  let mutable length = 1
  let dim__indices =
    dimes
    |> Array.map(fun dim -> 
      length <- length * dim
      [| 0 .. dim - 1 |])
  
  { dims = dimes
    dim__indices = dim__indices
    dataIndices = [| 0 .. length - 1 |]
    data = Array.zeroCreate length }

let addWith me incoming = 
    me.dataIndices
    |> Array.iter(fun i -> me.data[i] <- me.data[i] + incoming.data[i])

let addHadamardWtih me a b = 
  me.dataIndices
  |> Array.iter(fun i -> me.data[i] <- me.data[i] + a.data[i] * b.data[i])
