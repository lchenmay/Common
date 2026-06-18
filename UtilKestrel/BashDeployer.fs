module UtilKestrel.BashDeployer

open System
open System.IO
open System.Threading
open System.Collections.Generic

open Util.Bash
open Util.ADT
open Util.Bin
open Util.Db
open Util.DbTx
open Util.Collection
open Util.CollectionModDict
open Util.Orm
open Util.Http
open Util.HttpServer
open Util.Text
open Util.Json

open UtilKestrel.Types
open UtilKestrel.Ctx
open UtilKestrel.Db
open UtilKestrel.Api

let gitPush(gitName,gitEmail) = 
    [|  $"config user.name \"{gitName}\""
        $"config user.email \"{gitEmail}\""
        "add ."
        "commit -m \"auto-deploy\""
        "push" |]

let mutable pathGit = @"C:\Program Files\Git\bin\git.exe"

let remote(deployDir) = 
    [|  $"cd ~/{deployDir}"
        "git fetch --all"
        "git reset --hard origin/main"
        "sudo killall -9 dotnet || true"
        "sudo fuser -k 80/tcp || true"
        "sudo fuser -k 443/tcp || true"
        $"cd ~/{deployDir}/vscode"
        "/root/.bun/bin/bun install"
        "/root/.bun/bin/bun add vite @vitejs/plugin-vue @vitejs/plugin-vue-jsx @vitejs/plugin-basic-ssl -D"
        "/root/.bun/bin/bun generateRoutes.cjs"
        "/root/.bun/bin/bunx --/root/.bun/bin/bun vite build --emptyOutDir"
        "cd .."
        "cd Server"
        "sudo dotnet run" |]

let routine 
    output 
    (user,server) 
    (gitName,gitEmail)
    (devDir, deployDir, fsRoot) =

    output (cyan $">>> 开始部署至 {server}...")

    let exeLocal args = 
        exec output devDir "powershell" args
        |> ignore

    //gitPush(gitName,gitEmail)
    //|> Array.iter (exeLocal pathGit)

    "cd " + devDir |> exeLocal

    let rsaFile = devDir + "/id_rsa"

    [|  "连接远程Linux服务器前，先开通密钥"
        "默认将在 C:\Users\用户\.ssh 目录下生成id_rsa"
        "建议赋值路径 " + rsaFile + " 一路回车" |] 
    |> Array.iter output
    email__SshKey gitEmail |> green |> output

    "从 " + rsaFile + " 复制公钥到远程服务器（需要输入一次密码）" |> output
    remoteCopy_SshKey (user,server) rsaFile |> green |> output

    "如果返回 ssh: connect to host " + server + ": Connection refused 说明网络不通，检查远程主机防火墙或托管商控制台"
    |> output

    //let res = 
    //    "ls"
    //    |> bashOne output "root" server 


    output (cyan $">>> {server} 部署完成。")
    