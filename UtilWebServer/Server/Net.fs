module UtilWebServer.Server.Net

open System
open System.Net
open System.Net.Sockets
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
    
    try 
        if conn.client.Client |> isNull |> not then
            ip <- conn.client.Client.RemoteEndPoint.ToString()
            while conn.client.Available > 0 do
                let bs = Array.zeroCreate conn.client.Available
                let length = conn.client.Available
                let count = conn.ns.Read(bs, 0, length)
                if count > 0 then
                    bb.append bs
    with
    | :? System.NullReferenceException as ex ->
        ()
    | _ -> ()

    let bin = bb.bytes()
    ip,bin


let rcv (listener:Listener) conn = 

    async{

        try

            //"Conn [" + conn.id.ToString() + "] Receving ..." |> runtime.output

            let ip,incoming = tryReadToEnd conn

            "[" + conn.id.ToString() + "] Incomining " + incoming.Length.ToString() + " bytes"
            |> listener.output

            //incoming
            //|> hex
            //|> runtime.output

            match incomingProcess (ip,incoming) with
            | HttpRequestWithWS.Echo (reqo,(headers,body)) ->

                match reqo with
                | Some req ->

                    req.pathline |> listener.output

                    let outgoing =
                    
                        if req.method = "OPTIONS" then
                            [| |] |> bin__StandardResponse "text/html"
                        else
                            let mutable o = None
                            if o.IsNone then
                                o <- listener.echo req
                            if o.IsNone then
                                o <- listener.fileService req
                            if o.IsNone then
                                match listener.h404o with
                                | Some h ->
                                    o <- h() |> Some
                                | None -> 
                                    ()
                            match o with
                            | Some bin -> bin
                            | None -> 
                                [| |] |> bin__StandardResponse "text/html"

                    try
                        conn.ns.Write(outgoing,0,outgoing.Length)
                    with
                    | ex -> ()

                | None -> ()

            | HttpRequestWithWS.WebSocketUpgrade upgrade -> 

                upgrade |> outputHex listener.output "Upgrade"

                conn.ns.Write(upgrade,0,upgrade.Length)

                conn.state <- ConnState.Keep
                listener.keeps[conn.id] <- conn

                "Rcv -> Keep"
                |> listener.output

            | _ -> ()
        
            if conn.state <> ConnState.Keep then
                drop (Some listener.queue) conn
        with
        | ex -> drop (Some listener.queue) conn
    }

let snd listener (bin:byte[]) conn = 
    try
        bin |> conn.ns.Write
    with
    | ex -> drop (Some listener.keeps) conn

let sndJson (listener:Listener) json conn = 
    let raw = 
        json
        |> json__strFinal
        |> Text.Encoding.UTF8.GetBytes
    let encoded = raw |> wsEncode OpCode.Text

    raw |> outputHex listener.output "WS Outgoging Raw:"
    encoded |> outputHex listener.output "WS Outgoging Encoded:"

    snd listener encoded conn

let cycleAccept (listener:Listener) = fun () ->
    "Accept" |> listener.output

    let client = listener.socket.AcceptTcpClient()
    let id = Interlocked.Increment listener.connId

    let conn = checkoutConn id client
    listener.queue[conn.id] <- conn

    "Client accepted [" + id.ToString() + "], Queue = " + listener.queue.count.ToString()
    |> listener.output

    //s.Close()

let cycleRcv listener = fun () ->
    listener.queue.Values
    |> Array.filter(fun conn -> 
        try
            conn.ns.DataAvailable
        with
        | ex -> 
            drop (Some listener.queue) conn
            false)
    |> Array.iter(fun conn -> rcv listener conn |> Async.Start)

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

