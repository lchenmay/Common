module UtilKestrel.BashDeployer.File

open System.IO
open Util.Linux.Bash
open Util.Linux.Linux
open Util.Monitor
open UtilKestrel.Types

// ==================== 目录管理函数 ====================

/// 删除所有仓库目录
let deleteAllRepos output credential code =
    $"\n=== 开始删除所有仓库目录 ===" |> yellow |> output
    
    let repos = [|
        ("主项目", $"/root/Dev/{code}")
        ("Common", "/root/Dev/Common")
        ("JCS", "/root/Dev/JCS")
        ("Dev 根目录", "/root/Dev")
    |]
    
    repos |> Array.iter (fun (name, path) ->
        deleteRemoteDir output credential path |> ignore)
    
    "=== 所有仓库目录删除完成 ===" |> yellow |> output
