module jCQT.AI.Model

open System.Collections.Generic


// ---- 使用场景枚举 ----

type Scenario =
    | MathReasoning
    | DeepReasoning
    | Coding
    | GeneralChat
    | ChineseChat
    | VisionImage
    | ImageGeneration
    | ImageGenerationFast
    | Embedding
    | EmbeddingMultiLang
    | Petrol


// ---- 场景 -> 模型名映射 ----

let scenario__model =
    let d = Dictionary<Scenario, string>()

    // 推理
    d.[MathReasoning]         <- "gpt-oss:20b"
    d.[DeepReasoning]         <- "deepseek-r1:14b"
    d.[Petrol]                <- "gainenergy/ogai-reasoner:latest"

    // 编程 & 通用
    d.[Coding]                <- "qwen2.5-coder:14b"
    d.[GeneralChat]           <- "gpt-oss:20b"
    d.[ChineseChat]           <- "qwen2.5:14b"

    // 视觉
    d.[VisionImage]           <- "gemma3:12b"

    // 图像生成
    d.[ImageGeneration]       <- "x/z-image-turbo:latest"
    d.[ImageGenerationFast]   <- "x/flux2-klein:4b"

    // Embedding
    d.[Embedding]             <- "nomic-embed-text:latest"
    d.[EmbeddingMultiLang]    <- "bge-m3:latest"

    d


// ---- 辅助函数 ----

/// 根据场景获取模型名
let modelName (scenario:Scenario) =
    scenario__model.[scenario]

/// 获取所有场景
let allScenarios() =
    scenario__model.Keys |> Seq.toList

/// 获取所有模型名（去重）
let allModels() =
    scenario__model.Values |> Seq.distinct |> Seq.toList
