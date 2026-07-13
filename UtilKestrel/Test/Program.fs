module UtilKestrel.Test.Program

open System
open System.IO
open System.Threading
open Util.Json
open UtilKestrel.Common
open UtilKestrel.Paging

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

let pageData (response:(string * Json)[]) =
    match response |> Array.find (fun (name, _) -> name = "data") |> snd with
    | Json.Ary values -> values
    | _ -> [||]

let pageInfo (response:(string * Json)[]) =
    response
    |> Array.find (fun (name, _) -> name = "paging")
    |> snd
    |> json__Pagingo
    |> Option.get

[<EntryPoint>]
let main argv =
    let mutable failures = 0
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
    else failures <- failures + fail "history 记录不足"

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
        failures <- failures + fail (sprintf "日志文件不存在: %s" logPath)

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
    else failures <- failures + fail (sprintf "期望 >=100, 实际 %d" delta)

    // ── Test 4: 分页必须严格限制每页数量 ──
    printfn ""
    printfn "[Test 4] 分页边界..."
    let items = [| 1..14 |]
    let item__json value = value.ToString() |> Json.Num
    let request npp page =
        Json.Braket [|
            "paging", Json.Braket [|
                "npp", npp.ToString() |> Json.Num
                "page", page.ToString() |> Json.Num
                "total", "0" |> Json.Num
                "pages", "0" |> Json.Num |] |]

    let first = paging item__json (request 10 0) items
    let last = paging item__json (request 10 1) items
    let overflow = paging item__json (request 10 99) items
    let empty = paging item__json (request 10 0) [||]
    let capped = paging item__json (request Int32.MaxValue 0) [| 1..250 |]
    let defaulted = paging item__json (request 0 0) [| 1..35 |]
    let complete = allItems item__json [| 1..250 |]
    let firstInfo = pageInfo first
    let lastInfo = pageInfo last
    let overflowInfo = pageInfo overflow
    let emptyInfo = pageInfo empty
    let cappedInfo = pageInfo capped
    let defaultedInfo = pageInfo defaulted
    let completeInfo = pageInfo complete

    let pagingPassed =
        (pageData first).Length = 10
        && firstInfo.total = 14
        && firstInfo.pages = 2
        && (pageData last).Length = 4
        && lastInfo.page = 1
        && (pageData overflow).Length = 4
        && overflowInfo.page = 1
        && (pageData empty).Length = 0
        && emptyInfo.page = 0
        && emptyInfo.pages = 0
        && (pageData capped).Length = 200
        && cappedInfo.npp = 200
        && cappedInfo.pages = 2
        && (pageData defaulted).Length = 30
        && defaultedInfo.npp = 30
        && defaultedInfo.pages = 2
        && (pageData complete).Length = 250
        && completeInfo.total = 250
        && completeInfo.pages = 1

    if pagingPassed then pass "普通分页边界和 ls-all 完整集合均正确" |> ignore
    else failures <- failures + fail "分页结果或边界校验失败"

    // ── 清理 ──
    sc.autosaveo <- None
    if File.Exists logPath then File.Delete logPath

    printfn ""
    printfn "=== 测试完成 ==="
    printfn "用法: 在实际 EntryPoint 中设置:"
    printfn "  sc.autosaveo <- Some \"/Dev/ProjCode/Server/log.txt\""
    if failures = 0 then 0 else 1
