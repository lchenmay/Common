module UtilWebServer.Server.Net

open System
open System.Threading

open Util.ADT
open Util.CollectionModDict
open Util.Bin
open Util.Http
open Util.HttpServer
open Util.WebSocket

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

let rcv engine conn = 

    async{
        [|  "Conn [" + conn.id.ToString() + "]"
            " = " + conn.state.ToString() |]
        |> String.Concat
        |> engine.output

        "Conn [" + conn.id.ToString() + "] Receving ..."
        |> engine.output
    

        match read conn with
        | Some incoming -> 

            "Incomining " + incoming.Length.ToString() + " bytes"
            |> engine.output

            incoming
            |> hex
            |> engine.output

            match incomingProcess incoming with
            | HttpRequestWithWS.Echo (reqo,(headers,body)) ->

                match reqo with
                | Some req ->

                    let outgoing =
                        req
                        |> engine.echo
                        |> oPipelineNone (fun _ -> 
                            fileService 
                                engine.folder 
                                engine.defaultHtml 
                                req)
                        |> oPipelineNoneHandlero [||] engine.h404o
                        |> Option.get

                    try
                        conn.ns.Write(outgoing,0,outgoing.Length)
                    with
                    | ex -> ()

                | None -> ()

            | HttpRequestWithWS.WebSocketUpgrade upgrade -> 

                "Upgrade"
                |> engine.output

                conn.ns.Write(upgrade,0,upgrade.Length)

                conn.state <- ConnState.Keep
                engine.keeps[conn.id] <- conn

                "Rcv -> Keep"
                |> engine.output

            | _ -> ()
        | None -> ()
        
        if conn.state <> ConnState.Keep then
            drop engine (Some engine.queue) conn
    }

let cycleAccept engine = fun () ->
    "Accept" |> engine.output

    let client = engine.listener.AcceptTcpClient()
    let id = Interlocked.Increment engine.connId

    let conn = checkoutConn engine id client
    engine.queue[conn.id] <- conn

    "Client accepted [" + id.ToString() + "], Queue = " + engine.queue.count.ToString()
    |> engine.output

    //s.Close()

let cycleRcv engine = fun () ->
    engine.queue.array()
    |> Array.filter(fun conn -> conn.ns.DataAvailable)
    |> Array.iter(fun conn -> rcv engine conn |> Async.Start)

let cycleWs engine = fun () ->

    engine.keeps.array()
    |> Array.filter(fun conn -> conn.ns.DataAvailable)
    |> Array.Parallel.iter(fun conn -> 
    
        match read conn with
        | Some incoming ->
            incoming |> outputHex engine "WS Incoming Raw:"
            match wsDecode incoming with
            | Some decoded -> 
                decoded |> outputHex engine "WS Incoming Decoded:"
                match engine.wsHandler decoded with
                | Some rep ->
                    rep |> outputHex engine "WS Outgoging Raw:"
                    try
                        let encoded = rep |> wsEncode
                        encoded |> outputHex engine "WS Outgoging Encoded:"
                        encoded |> conn.ns.Write
                    with
                    | ex -> drop engine (Some engine.keeps) conn
                | None -> ()
            | None -> engine.output "Decode failed"
        | None -> drop engine (Some engine.keeps) conn)

