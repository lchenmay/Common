module UtilKestrel.Test.Program

open System
open System.IO
open System.Threading
open UtilKestrel.Common

let logPath = "/Dev/ProjCode/Server/log.txt"

let ensureDir (path: string) =
    let dir = Path.GetDirectoryName path
    if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> ignore

let fail msg =
    printfn "  [FAIL] %s" msg
    1

let pass msg =
    printfn "  [PASS] %s" msg
    0

[<EntryPoint>]
let main argv =
    printfn "=== UtilKestrel.Common SC 测试 ==="
    printfn ""

    // ── Test 1: sc.output → history 记录 ──
    printfn "[Test 1] sc.output 写入 history..."
    sc.output "Hello - test message 1"
    sc.output "中文测试消息"
    sc.output "Special: {}[]!@#$%^"

    Thread.Sleep 50

    let h = loadAll()
    let ok = h.Length >= 3
    printfn "  history 记录数: %d" h.Length
    h |> Array.iteri (fun i (dt, msg) ->
        printfn "    [%d] %s -> %s" i (dt.ToString("HH:mm:ss.fff")) msg)
    if ok then pass "消息已全部记录到 history" |> ignore
    else fail "history 记录不足" |> ignore

    // ── Test 2: autosave 文件写入 ──
    printfn ""
    printfn "[Test 2] 设置 autosaveo 并验证自动保存..."
    ensureDir logPath
    // 清除旧日志
    if File.Exists logPath then File.Delete logPath

    sc.autosaveo <- Some logPath
    printfn "  autosaveo 已设为: %s" logPath

    for i in 1..5 do
        sc.output (sprintf "autosave 测试 #%d" i)

    // 等待 cycler (interval=1000ms)
    Thread.Sleep 1500

    if File.Exists logPath then
        let lines = File.ReadAllLines logPath
        printfn "  日志文件: %d 行" lines.Length
        lines |> Array.iter (printfn "    %s")
        pass "自动保存成功" |> ignore
    else
        fail (sprintf "日志文件不存在: %s" logPath) |> ignore

    // ── Test 3: 高并发写入不丢消息 ──
    printfn ""
    printfn "[Test 3] 并发写入 100 条..."

    let before = sc.history.Count
    let batch = [| for i in 1..100 -> sprintf "并发消息 #%d" i |]
    batch |> Array.Parallel.iter sc.output

    Thread.Sleep 100

    let after = sc.history.Count
    let delta = after - before
    printfn "  写入前: %d, 写入后: %d, 增量: %d" before after delta
    if delta >= 100 then pass "并发写入无丢失" |> ignore
    else fail (sprintf "期望 >=100, 实际 %d" delta) |> ignore

    // ── 清理 ──
    sc.autosaveo <- None
    if File.Exists logPath then File.Delete logPath

    printfn ""
    printfn "=== 测试完成 ==="
    printfn "用法: 在实际 EntryPoint 中设置:"
    printfn "  sc.autosaveo <- Some \"/Dev/ProjCode/Server/log.txt\""
    0
