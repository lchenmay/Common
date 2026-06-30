module jCQT.Test.Program

open jCQT.AI.Autograd

[<EntryPoint>]
let main argv =
    // 测试标量 Autograd
    Examples.testScalar ()

    printfn "\n"

    // 线性回归示例
    Examples.linearRegression ()

    0
