module jCQT.AI.Autograd

#nowarn "64"

open System
open System.Collections.Generic
open System.Threading

open jCQT.AI.Tensor

type Op =
| Leaf
| Param
| Add
| Mul
| Relu
| Mse of float

let private nextId = ref 0

type TNode<'T> = {
  id: int
  mutable value: Tensor<'T>
  mutable grad: Tensor<'T>
  prev:  Tensor<'T>[]
  op: Op }

let backward scalar =
  match scalar.op with
  | Add ->
    let a = scalar.prev.[0]
    let b = scalar.prev.[1]
    a.grad <- a.grad + scalar.grad
    b.grad <- b.grad + scalar.grad
  | Mul ->
    let a = scalar.prev.[0]
    let b = scalar.prev.[1]
    a.grad <- a.grad + scalar.grad * b.value
    b.grad <- b.grad + scalar.grad * a.value
  | Relu ->
    let a = scalar.prev.[0]
    if a.value > 0.0 then
      a.grad <- a.grad + scalar.grad
  | Mse yTrue ->
    let yPred = scalar.prev.[0]
    yPred.grad <- yPred.grad + scalar.grad * 2.0 * (yPred.value - yTrue)
  | Leaf 
  | Param -> ()

let create (prev, op, v) = 
  { id = Interlocked.Increment nextId
    value = v
    grad = 0.0
    prev = prev
    op = op }

let outputBackward output =
  let visited = HashSet<int>()
  let topo = List<TNode>()

  let rec buildTopo node =
    if visited.Add node.id then
      node.prev
      |> Array.iter buildTopo
      topo.Add node

  buildTopo output

  output.grad <- 1.0

  topo.ToArray()
  |> Array.rev
  |> Array.iter backward

type SGD = {
  lr: float
  prms: TNode[] }

let step optimizer =
  optimizer.prms
  |> Array.iter(fun s -> s.value <- s.value - optimizer.lr * s.grad)

let zeroGrad optimizer =
  optimizer.prms 
  |> Array.iter(fun s -> s.grad <- 0.0)
