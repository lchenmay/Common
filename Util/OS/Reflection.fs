module Util.Reflection

open LanguagePrimitives

open System
open System.IO
open System.Reflection


/// 解析策略 dll 的共享/非共享依赖。
/// 关键：优先复用 AppDomain 中已加载的同名程序集（即主程序那份正确的实例）。
/// 因为策略 dll 已被单独复制到【不含依赖副本】的干净临时目录，其共享依赖
/// （J7.Shared / Util / J7.QLib / FSharp.Core …）无法在临时目录就地解析，必然
/// 触发本 resolver → 复用主程序已加载实例 → 类型身份一致，IsAssignableFrom 成功。
/// 仅当 AppDomain 确实缺失该依赖（真正只属于策略 bin 的私有依赖）时，才退回
/// 从 fallbackDir（原始策略 bin 目录）加载复制体。
let resolveDependencies output (targetAssemblyPath: string) (fallbackDir: string) (sender: obj) (args: ResolveEventArgs) =
    let requestedAssemblyName = args.Name.Split(',') |> Array.head

    // 1) 优先复用 AppDomain 中已加载的同名程序集（主程序那份正确的实例）
    let alreadyLoaded =
        AppDomain.CurrentDomain.GetAssemblies()
        |> Array.tryFind (fun asm ->
            not asm.IsDynamic
            && asm.GetName().Name = requestedAssemblyName)

    match alreadyLoaded with
    | Some asm ->
        $"Reusing already-loaded assembly: {asm.GetName().Name}" |> output
        asm
    | None ->
        // 2) 临时目录（通常无依赖副本）优先，再退回原始策略 bin 目录
        let candidates =
            [ Path.Combine(Path.GetDirectoryName targetAssemblyPath, requestedAssemblyName + ".dll")
              Path.Combine(fallbackDir, requestedAssemblyName + ".dll") ]
            |> List.filter File.Exists
        match candidates with
        | hp :: _ ->
            try
                let resolved = Assembly.LoadFrom hp
                $"Resolved {requestedAssemblyName} from {hp}" |> output
                resolved
            with ex ->
                $"Failed to load dependency {hp}: {ex.Message}" |> output
                null
        | [] ->
            $"Dependency {requestedAssemblyName} not found in temp or fallback dir." |> output
            null

// --- Main Loading Function ---
let tryLoadFromFileWithDependencies output (dllPath: string) =

    "try loading: " + dllPath
    |> output

    // ── 真正的根因修复 ──
    // 旧实现：直接 Assembly.LoadFrom 策略 bin 里的 Stg.<Name>.dll。
    // 由于 ProjectReference 默认 CopyLocal=true，策略 bin 目录里带着 J7.Shared.dll /
    // Util.dll / J7.QLib.dll / FSharp.Core 等的【副本】。.NET 的 LoadFrom 上下文
    // 会优先从策略 bin 目录就地加载这些共享依赖副本，它们与主程序 AppDomain 中已
    // 加载的 J7.Shared 不是同一 Assembly 实例。结果：策略所继承的 Qx 类型身份 ≠
    // 主程序的 Qx → Qx.fs 中 asm__impls 的 iType.IsAssignableFrom(t) 返回 false →
    // 策略类型被静默过滤 → name__qxType 不入册 → 策略下拉完全为空且无任何报错。
    // 更重要的是：该场景下 AssemblyResolve 根本不会触发（LoadFrom 上下文自己就找到
    // 了副本），所以"在 resolver 里复用已加载程序集"的旧逻辑形同虚设。
    //
    // 新方案：把策略 dll【单独】复制到不含任何依赖副本的干净临时目录，再 LoadFrom
    // 该临时副本。这样其共享依赖无法在临时目录就地解析，必然触发 AssemblyResolve
    // → 复用主程序已加载的正确实例 → 类型身份一致。非共享私有依赖则退回 fallbackDir
    // （原始策略 bin 目录）加载。
    let fallbackDir = Path.GetDirectoryName(dllPath)
    let tempDir =
        Path.Combine(Path.GetTempPath(), "J7StgLoad_" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(tempDir) |> ignore
    let tempDll = Path.Combine(tempDir, Path.GetFileName(dllPath))
    File.Copy(dllPath, tempDll, true) |> ignore

    let resolver =
        new ResolveEventHandler(fun s a -> resolveDependencies output tempDll fallbackDir s a)
    AppDomain.CurrentDomain.add_AssemblyResolve(resolver)

    try
        let assembly = Assembly.LoadFrom tempDll

        // --- 强制 CLR 在 resolver 仍生效时解析全部类型 ---
        [|  "SUCCESS ["
            assembly.FullName
            "]: "
            assembly.GetTypes().Length.ToString()
            " types found" |]
        |> String.Concat
        |> output

        AppDomain.CurrentDomain.remove_AssemblyResolve(resolver)

        Some assembly

    with
    | :? ReflectionTypeLoadException as rtle ->
        [|  "FAIL ["
            + dllPath
            "]: unresolved dependencies, ReflectionTypeLoadException loading. "
            "rtle.LoaderExceptions = " + rtle.LoaderExceptions.Length.ToString() |]
        |> String.Concat
        |> output

        rtle.LoaderExceptions
        |> Array.map(fun e -> e.ToString())
        |> Array.iter output

        AppDomain.CurrentDomain.remove_AssemblyResolve(resolver)

        None
    | ex ->
        AppDomain.CurrentDomain.remove_AssemblyResolve(resolver)

        "FAIL [" + dllPath + "]"
        |> output

        ex.Message
        |> output

        None
