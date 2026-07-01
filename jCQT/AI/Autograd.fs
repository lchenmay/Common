module jCQT.AI.Autograd

#nowarn "64"

open LanguagePrimitives

open System
open System.Threading

open jCQT.AI.Tensor

// ============================================================
// 计算图节点（标量）- 正确实现反向传播
// ============================================================

/// 全局 ID 计数器（模块级 mutable 变量，确保唯一 ID）
let private nextId = ref 0

/// 可微分的标量（构建计算图）
/// 设计：每个节点存储输入节点列表，backward 函数负责将梯度传播到输入
type Scalar =
    {
        /// 唯一 ID（用于拓扑排序）
        id: int

        /// 前向值
        mutable value: float

        /// 梯度（反向传播后填充）
        mutable grad: float

        /// 输入节点（计算图依赖，用于构建计算图）
        prev: Scalar list

        /// 操作名称（用于调试）
        op: string

        /// 反向函数（如何将梯度传播到输入）
        mutable backwardFn: (unit -> unit) option
    }

    /// 创建标量节点
    static member create (v: float) (prev: Scalar list) (op: string) : Scalar =
        let id = System.Threading.Interlocked.Increment(nextId)
        { id = id; value = v; grad = 0.0; prev = prev; op = op; backwardFn = None }

    /// 叶子节点（常量，不需要梯度）
    static member leaf (v: float) : Scalar =
        Scalar.create v [] "leaf"

    /// 参数节点（需要梯度）
    static member param (v: float) : Scalar =
        Scalar.create v [] "param"

    /// 加法运算：c = a + b
    static member add (a: Scalar) (b: Scalar) : Scalar =
        let c = Scalar.create (a.value + b.value) [a; b] "add"
        c.backwardFn <- Some (fun () ->
            a.grad <- a.grad + c.grad
            b.grad <- b.grad + c.grad
        )
        c

    /// 乘法运算：c = a * b
    static member mul (a: Scalar) (b: Scalar) : Scalar =
        let c = Scalar.create (a.value * b.value) [a; b] "mul"
        c.backwardFn <- Some (fun () ->
            a.grad <- a.grad + c.grad * b.value
            b.grad <- b.grad + c.grad * a.value
        )
        c

    /// ReLU 激活函数
    static member relu (a: Scalar) : Scalar =
        let c = Scalar.create (max 0.0 a.value) [a] "relu"
        c.backwardFn <- Some (fun () ->
            // ∂L/∂a = ∂L/∂c * (a > 0 ? 1 : 0)
            if a.value > 0.0 then
                a.grad <- a.grad + c.grad
        )
        c

// ============================================================
// 标量反向传播（正确实现）
// ============================================================

/// 反向传播（从输出节点开始，构建计算图并传播梯度）
let backward output =
    // 1. 构建计算图的所有节点（拓扑排序，后序遍历）
    let visited = System.Collections.Generic.HashSet<int>()
    let topo = ResizeArray<Scalar>()

    let rec buildTopo (node: Scalar) : unit =
        if visited.Add(node.id) then
            // 先访问所有输入节点（依赖）
            for input in node.prev do
                buildTopo input
            // 再添加当前节点（后序遍历，确保输入节点在前）
            topo.Add(node)

    buildTopo output

    // 2. 设置输出节点的梯度为 1.0
    output.grad <- 1.0

    // 3. 反向遍历（从输出到输入），依次调用 backwardFn
    //    topo 是按计算顺序排列的（输入在前，输出在后）
    //    所以需要反转，从输出开始向后传播
    for node in List.rev (List.ofSeq topo) do
        match node.backwardFn with
        | Some f -> f ()
        | None -> ()

// ============================================================
// 优化器
// ============================================================

/// SGD 优化器
type SGD =
    {
        /// 学习率
        lr: float

        /// 参数列表
        parameters: Scalar list
    }

    /// 创建 SGD 优化器
    static member create (lr: float) (parameters: Scalar list) : SGD =
        { lr = lr; parameters = parameters }

    /// 执行一步优化（w = w - lr * dw）
    member this.step () : unit =
        for p in this.parameters do
            p.value <- p.value - this.lr * p.grad

    /// 清零梯度
    member this.zeroGrad () : unit =
        for p in this.parameters do
            p.grad <- 0.0

// ============================================================
// 损失函数
// ============================================================

module Loss =

    /// 均方误差损失：L = (y_pred - y_true)²
    let mse (yPred: Scalar) (yTrue: float) : Scalar =
        let diff = yPred.value - yTrue
        let loss = Scalar.create (diff * diff) [yPred] "mse"
        loss.backwardFn <- Some (fun () ->
            // ∂L/∂y_pred = 2 * (y_pred - y_true)
            yPred.grad <- yPred.grad + loss.grad * 2.0 * (yPred.value - yTrue)
        )
        loss

// ============================================================
// 向量和矩阵自动微分操作（待实现）
// ============================================================
// 注意：以下模块因 F# 泛型类型推断问题暂时注释
// 使用时需要内联向量/矩阵操作或添加显式类型注解
//
// module VctA = ...  // 向量自动微分操作
// module MatA = ...  // 矩阵自动微分操作
// module LossT = ...  // 张量损失函数
//
// ============================================================

// ============================================================
// 示例
// ============================================================

module Examples =

    /// 测试标量 Autograd
    let testScalar () : unit =
        printfn "=== 测试标量 Autograd ==="

        // 计算图：y = w * x + b
        let x = Scalar.leaf 2.0
        let w = Scalar.param 0.5
        let b = Scalar.param 0.0

        let wx = Scalar.mul w x
        let y = Scalar.add wx b

        // 反向传播
        backward y

        printfn "y = w * x + b"
        printfn "x = %f" x.value
        printfn "w = %f, dw = %f" w.value w.grad
        printfn "b = %f, db = %f" b.value b.grad
        printfn ""
        printfn "期望：dw = 2.0, db = 1.0"
        printfn "实际：dw = %f, db = %f" w.grad b.grad

    /// 线性回归示例
    let linearRegression () : unit =
        printfn "=== 线性回归示例 ==="

        // 数据
        let xData = [| 1.0; 2.0; 3.0; 4.0 |]
        let yData = [| 2.0; 4.0; 6.0; 8.0 |]

        // 参数初始化
        let w = Scalar.param 0.0
        let b = Scalar.param 0.0

        // 优化器
        let optimizer = SGD.create 0.01 [w; b]

        // 训练循环
        for epoch = 1 to 1000 do
            optimizer.zeroGrad()

            let mutable totalLoss = 0.0
            for i = 0 to xData.Length - 1 do
                let x = Scalar.leaf xData.[i]
                let yTrue = yData.[i]

                // 前向传播：y_pred = w * x + b
                let wx = Scalar.mul w x
                let yPred = Scalar.add wx b

                // 损失函数：MSE
                let loss = Loss.mse yPred yTrue
                totalLoss <- totalLoss + loss.value

                // 反向传播
                backward loss

            // 优化器步进
            optimizer.step()

            // 打印进度
            if epoch % 100 = 0 then
                printfn "Epoch %d, Loss: %.6f" epoch (totalLoss / float xData.Length)

        printfn ""
        printfn "训练完成："
        printfn "w = %f (期望 2.0)" w.value
        printfn "b = %f (期望 0.0)" b.value
