module jCQT.AI.Autograd

#nowarn "64"

open System
open System.Threading
open System.Collections.Generic

open jCQT.AI.Tensor

// ============================================================
type Op =
    | Leaf
    | Param
    | Add
    | Mul
    | Relu
    | Mse
    override this.ToString() =
        match this with
        | Leaf -> "leaf"
        | Param -> "param"
        | Add -> "add"
        | Mul -> "mul"
        | Relu -> "relu"
        | Mse -> "mse"

// ============================================================
let private nextId = ref 0

type Scalar = {
    id: int
    mutable value: float
    mutable grad: float
    prev: Scalar list
    op: Op
    mutable backwardFn: (unit -> unit) option
}

// ============================================================
module Scalar =


    // 原有
    //let create v prev op =
    //    let id = Interlocked.Increment nextId
    //    { id = id; value = v; grad = 0.0; prev = prev; op = op; backwardFn = None }
    //let leaf v =
    //    create v [] Leaf

    //let param v =
    //    create v [] Param

    // 我的修改1: 通过高阶函数变元的重排，简化参数

    let create prev op v =
        let id = Interlocked.Increment nextId
        { id = id; value = v; grad = 0.0; prev = prev; op = op; backwardFn = None }

    let leaf = create [] Leaf

    let param v = create [] Param

    // 我的修改2：利用高阶函数进一步简化

    let create prev op v =
        let id = Interlocked.Increment nextId
        { id = id; value = v; grad = 0.0; prev = prev; op = op; backwardFn = None }

    let createEmpty = create [] 
    let leaf = createEmpty Leaf
    let param v = createEmpty Param

    let add a b =
        let c = create (a.value + b.value) [a; b] Add
        c.backwardFn <- Some (fun () ->
            a.grad <- a.grad + c.grad
            b.grad <- b.grad + c.grad
        )
        c

    let mul a b =
        let c = create (a.value * b.value) [a; b] Mul
        c.backwardFn <- Some (fun () ->
            a.grad <- a.grad + c.grad * b.value
            b.grad <- b.grad + c.grad * a.value
        )
        c

    let relu a =
        let c = create (max 0.0 a.value) [a] Relu
        c.backwardFn <- Some (fun () ->
            if a.value > 0.0 then
                a.grad <- a.grad + c.grad
        )
        c

// ============================================================
let backward output =
    let visited = HashSet<int>()
    let topo = ResizeArray<Scalar>()

    let rec buildTopo node =
        if visited.Add(node.id) then
            for input in node.prev do
                buildTopo input
            topo.Add(node)

    buildTopo output

    output.grad <- 1.0

    for i = topo.Count - 1 downto 0 do
        let node = topo.[i]
        match node.backwardFn with
        | Some f -> f ()
        | None -> ()

// ============================================================
type SGD = {
    lr: float
    parameters: Scalar list
}

module SGD =

    let create lr parameters =
        { lr = lr; parameters = parameters }

    let step (optimizer: SGD) =
        for p in optimizer.parameters do
            p.value <- p.value - optimizer.lr * p.grad

    let zeroGrad (optimizer: SGD) =
        for p in optimizer.parameters do
            p.grad <- 0.0

// ============================================================
module Loss =

    let mse yPred yTrue =
        let diff = yPred.value - yTrue
        let loss = Scalar.create (diff * diff) [yPred] Mse
        loss.backwardFn <- Some (fun () ->
            yPred.grad <- yPred.grad + loss.grad * 2.0 * (yPred.value - yTrue)
        )
        loss

// ============================================================
module Examples =

    let testScalar () =
        printfn "=== 测试标量 Autograd ==="
        let x = Scalar.leaf 2.0
        let w = Scalar.param 0.5
        let b = Scalar.param 0.0
        let wx = Scalar.mul w x
        let y = Scalar.add wx b
        backward y
        printfn "y = w * x + b"
        printfn "x = %f" x.value
        printfn "w = %f, dw = %f" w.value w.grad
        printfn "b = %f, db = %f" b.value b.grad
        printfn ""
        printfn "期望：dw = 2.0, db = 1.0"
        printfn "实际：dw = %f, db = %f" w.grad b.grad

    let linearRegression () =
        printfn "=== 线性回归示例 ==="
        let xData = [| 1.0; 2.0; 3.0; 4.0 |]
        let yData = [| 2.0; 4.0; 6.0; 8.0 |]
        let w = Scalar.param 0.0
        let b = Scalar.param 0.0
        let optimizer = SGD.create 0.01 [w; b]
        for epoch = 1 to 1000 do
            SGD.zeroGrad optimizer
            let mutable totalLoss = 0.0
            for i = 0 to xData.Length - 1 do
                let x = Scalar.leaf xData.[i]
                let yTrue = yData.[i]
                let wx = Scalar.mul w x
                let yPred = Scalar.add wx b
                let loss = Loss.mse yPred yTrue
                totalLoss <- totalLoss + loss.value
                backward loss
            SGD.step optimizer
            if epoch % 100 = 0 then
                printfn "Epoch %d, Loss: %.6f" epoch (totalLoss / float xData.Length)
        printfn ""
        printfn "训练完成："
        printfn "w = %f (期望 2.0)" w.value
        printfn "b = %f (期望 0.0)" b.value
