module UtilWebServer.Server.Net

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

let tryReadToEnd conn = 

    let bb = new BytesBuilder()

    let mutable ip = ""
    let mutable offset = 0         

    try

        ip <- conn.client.Client.RemoteEndPoint.ToString()
        while conn.client.Available > 0 do
            let bs = Array.zeroCreate conn.client.Available
            Console.WriteLine("conn[" + conn.id.ToString() + "] bs = " + bs.Length.ToString())
            
            let length = 
                if offset + bs.Length < conn.client.Available then
                    bs.Length
                else
                    conn.client.Available
            Console.WriteLine("offset = " + offset.ToString())
            Console.WriteLine("length = " + length.ToString())
            let count = conn.ns.Read(bs, offset, length)
            if count > 0 then
                offset <- offset + count
                bb.append bs
    with
    | ex -> 
        Console.WriteLine("Exception: " + ex.ToString())

    let bin = bb.bytes()
    ip,bin

let rcv runtime conn = 

    async{

        //"Conn [" + conn.id.ToString() + "] Receving ..." |> runtime.output

        let ip,incoming = tryReadToEnd conn

        "[" + conn.id.ToString() + "] Incomining " + incoming.Length.ToString() + " bytes"
        |> runtime.output

        //incoming
        //|> hex
        //|> runtime.output

        match incomingProcess (ip,incoming) with
        | HttpRequestWithWS.Echo (reqo,(headers,body)) ->

            match reqo with
            | Some req ->

                req.pathline |> runtime.output

                let outgoing =

                    if req.method = "OPTIONS" then
                        [| |] |> bin__StandardResponse "text/html"
                    else
                        req
                        |> runtime.echo
                        |> oPipelineNone (fun _ -> 
                            fileService 
                                runtime.host.fsDir
                                runtime.host.vueDeployDir
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
        
        if conn.state <> ConnState.Keep then
            drop (Some runtime.queue) conn
    }

let snd runtime (bin:byte[]) conn = 
    try
        bin |> conn.ns.Write
    with
    | ex -> drop (Some runtime.keeps) conn

let sndJson runtime json conn = 
    let raw = 
        json
        |> json__strFinal
        |> Text.Encoding.UTF8.GetBytes
    let encoded = raw |> wsEncode OpCode.Text

    raw |> outputHex runtime.output "WS Outgoging Raw:"
    encoded |> outputHex runtime.output "WS Outgoging Encoded:"

    snd runtime encoded conn

let cycleAccept runtime = fun () ->
    "Accept" |> runtime.output

    let client = runtime.listener.AcceptTcpClient()
    let id = Interlocked.Increment runtime.connId

    let conn = checkoutConn id client
    runtime.queue[conn.id] <- conn

    "Client accepted [" + id.ToString() + "], Queue = " + runtime.queue.count.ToString()
    |> runtime.output

    //s.Close()

let cycleRcv runtime = fun () ->
    runtime.queue.Values
    |> Array.filter(fun conn -> 
        try
            conn.ns.DataAvailable
        with
        | ex -> 
            drop (Some runtime.queue) conn
            false)
    |> Array.iter(fun conn -> rcv runtime conn |> Async.Start)

let cycleWs runtime = fun () ->

    runtime.keeps.Values
    |> Array.filter(fun conn -> 
        try
            conn.ns.DataAvailable
        with
        | ex -> 
            drop (Some runtime.queue) conn
            false)
    |> Array.Parallel.iter(fun conn -> 
    
        use cw = new CodeWrapper("UtilWebServer.Net.cycleWs/Array.Parallel")
        let ip,incoming = tryReadToEnd conn
        
        if incoming.Length > 0 then
            incoming |> outputHex runtime.output "WS Incoming Raw:"
            let bits = bytes__bits incoming
            bits |> bit__txt |> runtime.output

            match wsDecode incoming with
            | Some (opcode,decoded) -> 

                decoded |> outputHex runtime.output "WS Incoming Decoded:"

                match opcode with
                | OpCode.Ping ->
                    let msg = decoded |> wsEncode OpCode.Pong
                    snd runtime msg conn
                | OpCode.Text -> 

                    let msg =  
                        decoded
                        |> Text.Encoding.UTF8.GetString
                        |> str__root

                    let repo = 
                        use cw = new CodeWrapper("UtilWebServer.Net.cycleWs/wsHandler")
                        msg |> runtime.wsHandler 

                    match repo with
                    | Some rep -> sndJson runtime rep conn
                    | None -> ()

                | OpCode.Close -> drop (Some runtime.keeps) conn
                | _ -> ()

            | None -> runtime.output "Decode failed")

