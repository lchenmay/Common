﻿module UtilWebServer.Server.Net

open System
open System.Threading

open Util.ADT
open Util.Perf
open Util.CollectionModDict
open Util.Bin
open Util.Text
open Util.Json
open Util.Http
open Util.HttpServer
open Util.WebSocket

open UtilWebServer.Common
open UtilWebServer.Server.Common
open UtilWebServer.Server.File
open UtilWebServer.Server.Monitor

let read conn = 

    try
        let bb = new BytesBuilder()

        let bs = Array.zeroCreate conn.client.Available
        let count = conn.ns.Read(bs, 0, bs.Length)
        bb.append bs

        let bin = bb.bytes()

        bin
        |> Some

     with
    | ex -> None

let rcv runtime conn = 

    async{

        //"Conn [" + conn.id.ToString() + "] Receving ..." |> runtime.output

        match read conn with
        | Some incoming -> 

            "Incomining " + incoming.Length.ToString() + " bytes"
            |> runtime.output

            incoming
            |> hex
            |> runtime.output

            match incomingProcess incoming with
            | HttpRequestWithWS.Echo (reqo,(headers,body)) ->

                match reqo with
                | Some req ->

                    let outgoing =
                        req
                        |> runtime.echo
                        |> oPipelineNone (fun _ -> 
                            fileService 
                                runtime.host.fsDir
                                runtime.host.defaultHtml
                                req)
                        |> oPipelineNoneHandlero [||] runtime.h404o
                        |> Option.get

                    try
                        conn.ns.Write(outgoing,0,outgoing.Length)
                    with
                    | ex -> ()

                | None -> ()

            | HttpRequestWithWS.WebSocketUpgrade upgrade -> 

                upgrade |> outputHex runtime.output "Upgrade"

                conn.ns.Write(upgrade,0,upgrade.Length)

                conn.state <- ConnState.Keep
                runtime.keeps[conn.id] <- conn

                "Rcv -> Keep"
                |> runtime.output

            | _ -> ()
        | None -> ()
        
        if conn.state <> ConnState.Keep then
            drop (Some runtime.queue) conn
    }

let cycleAccept runtime = fun () ->
    "Accept" |> runtime.output

    let client = runtime.listener.AcceptTcpClient()
    let id = Interlocked.Increment runtime.connId

    let conn = checkoutConn id client
    runtime.queue[conn.id] <- conn

    "Client accepted [" + id.ToString() + "], Queue = " + runtime.queue.count.ToString()
    |> runtime.output

    //s.Close()

let cycleRcv engine = fun () ->
    engine.queue.array()
    |> Array.filter(fun conn -> conn.ns.DataAvailable)
    |> Array.iter(fun conn -> rcv engine conn |> Async.Start)

let cycleWs runtime = fun () ->

    runtime.keeps.array()
    |> Array.filter(fun conn -> conn.ns.DataAvailable)
    |> Array.Parallel.iter(fun conn -> 
    
        use cw = new CodeWrapper("UtilWebServer.Net.cycleWs/Array.Parallel")
        match read conn with
        | Some incoming ->
            incoming |> outputHex runtime.output "WS Incoming Raw:"
            match wsDecode incoming with
            | Some decoded -> 
                decoded |> outputHex runtime.output "WS Incoming Decoded:"

                let msg =  
                    decoded
                    |> Text.Encoding.UTF8.GetString
                    |> str__root

                let repo = 
                    use cw = new CodeWrapper("UtilWebServer.Net.cycleWs/wsHandler")
                    msg |> runtime.wsHandler 

                match repo with
                | Some rep ->
                    try
                        let encoded = 
                            rep
                            |> json__strFinal
                            |> Text.Encoding.UTF8.GetBytes
                            |> wsEncode
                        encoded |> outputHex runtime.output "WS Outgoging Encoded:"
                        encoded |> conn.ns.Write
                    with
                    | ex -> drop (Some runtime.keeps) conn
                | None -> ()
            | None -> runtime.output "Decode failed"
        | None -> drop (Some runtime.keeps) conn)

