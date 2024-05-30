module Util.Boost

open System.Collections.Generic
open System.Collections.Concurrent
open System
open System.Text
open System.Text.RegularExpressions
open System.Numerics
open System.Security.Cryptography

open Util.Perf

open Math
open Crypto
open Text

type Abc = {
a: int
b: string
c: float
d: string[]
}


System.Console.OutputEncoding <- System.Text.Encoding.Unicode

let output (s:string) = System.Console.WriteLine s


// === Test == 

let runTest output =

    let d1 = new Dictionary<int64,BigInteger>()
    let d2 = new ConcurrentDictionary<int64,BigInteger>()
    let d3 = Util.CollectionModDict.createMDInt64<BigInteger> 10

    let n = 1024 * 1024

    let populateD1 pairs = 
        pairs
        |> Array.Parallel.iter(fun i ->
            let k,v = i
            use cw = new CodeWrapper("D1")
            lock d1 (fun _ ->
                if d1.ContainsKey k = false then
                    d1.Add(k,v)))

    let populateD2 pairs = 
        pairs
        |> Array.Parallel.iter(fun i ->
            let k,v = i
            use cw = new CodeWrapper("D2")
            if d2.ContainsKey k = false then
                d2[k] <- v)

    let populateD3 pairs = 
        pairs
        |> Array.Parallel.iter(fun i ->
            let k,v = i
            use cw = new CodeWrapper("D3")
            if d3.ContainsKey k = false then
                d3[k] <- v)

    let pairs = 
        [| 0..n |]
        |> Array.map(fun i -> int64 i,ec__privateKey256 secp256k1)

    populateD1 pairs
    populateD2 pairs
    populateD3 pairs

    Perf.statTextLines None
    |> Array.iter output



[<EntryPoint>]
let main argv =

    //runTest output

    //Util.CodeRobot.go output

    //async{
    //    ZWS.testHttpClient output
    //} |> Async.Start

    //async{
    //    ZWS.testWSClient output
    //} |> Async.Start

    while true do
        System.Threading.Thread.Sleep 1000
        

    //let a = BigInteger -1024767
    //let bin = a.ToByteArray()
    //let b = new BigInteger(bin)

    //let ec = secp256k1

    //// 请求方：生成私钥 a 自行保留，发送公钥 aG
    //let a = ec__privateKey256 ec
    //let aG = ecScalar256 ec a

    //// 响应方：生成私钥 b 自行保留，发送公钥 bG
    //let b = ec__privateKey256 ec
    //let bG = ecScalar256 ec b

    //// 双方各自生成的对称密钥
    //let abG = 
    //    let table = ecTable256 ec.p ec.a bG
    //    scalar256 ec.p ec.a table a // a(bG)
    //let aSymKey = 
    //    ecPoint__binKey abG
    //    |> bin__sha256bin

    //let baG =
    //    let table = ecTable256 ec.p ec.a aG
    //    scalar256 ec.p ec.a table b // b(aG)
    //let bSymKey = 
    //    ecPoint__binKey baG
    //    |> bin__sha256bin

    ////let privateKey = ec__privateKey256 spec256k1

    //let privateKey = hex__bigint "9f73f00f36f26d003cb70962a2b7cc69af2ee5b148ec2a32bbe7f606797d7774"

    //let publicKey,pkHex = Crypto.private__public secp256k1 privateKey

    //let msg = 
    //    "aaa"
    //    |> System.Text.Encoding.ASCII.GetBytes

    ////let r,s = ecgSign secp256k1 Crypto.bin__sha256 privateKey msg

    ////let validate = ecgValidate secp256k1 Crypto.bin__sha256 publicKey msg (r,s)

    //GC.Collect()

    ////test.launchClusterNode()
    ////Console.WriteLine("enter any key to call")
    ////Console.ReadLine() |> ignore
    ////test.getNodeInfos()
    ////Console.WriteLine("Done")
    ////Console.ReadLine() |> ignore


    ////let testWsPush() =
    ////    async {
    ////    while true do
    ////        let s = Console.ReadLine()
    ////        ZMQ.wsPush ZMQ.wsUserFilter1 s
    ////    } |> Async.Start

    ////testWsPush()

    0