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
| Hadamard
| Relu
| Mse of Tensor

let private nextId = ref 0

type TNode = {
  id: int
  mutable value: Tensor
  mutable grad: Tensor
  prev:  TNode[]
  op: Op }

let backward scalar =
  match scalar.op with
  | Add ->
    let a = scalar.prev[0]
    let b = scalar.prev[1]
    scalar.grad |> addWith a.grad
    scalar.grad |> addWith b.grad 

  | Hadamard ->
    let a = scalar.prev[0]
    let b = scalar.prev[1]
    addHadamardWtih a.grad scalar.grad b.value 
    addHadamardWtih b.grad scalar.grad a.value 

  | Relu ->
    let a = scalar.prev[0]
    a.grad.dataIndices
    |> Array.iter(fun i -> 
      if a.value.data[i] > 0.0 then
        a.grad.data[i] <- a.grad.data[i] + scalar.grad.data[i])
      
  | Mse yTrue ->
    let yPred = scalar.prev[0]
    let factor = 2.0 / float yPred.grad.data.Length * scalar.grad.data[0]
        
    yPred.grad.dataIndices
    |> Array.iter(fun i -> 
      let diff = yPred.value.data[i] - yTrue.data[i]
      yPred.grad.data[i] <- yPred.grad.data[i] + (factor * diff))

  | Leaf 
  | Param -> ()

//let create (prev, op, v) = 
//  { id = Interlocked.Increment nextId
//    value = v
//    grad = 0.0
//    prev = prev
//    op = op }

//let outputBackward output =
//  let visited = HashSet<int>()
//  let topo = List<TNode>()

//  let rec buildTopo node =
//    if visited.Add node.id then
//      node.prev
//      |> Array.iter buildTopo
//      topo.Add node

//  buildTopo output

//  output.grad <- 1.0

//  topo.ToArray()
//  |> Array.rev
//  |> Array.iter backward

//type SGD = {
//  lr: float
//  prms: TNode[] }

//let step optimizer =
//  optimizer.prms
//  |> Array.iter(fun s -> s.value <- s.value - optimizer.lr * s.grad)

//let zeroGrad optimizer =
//  optimizer.prms 
//  |> Array.iter(fun s -> s.grad <- 0.0)
