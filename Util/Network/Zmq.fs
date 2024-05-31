module Util.Zmq

open LanguagePrimitives

open System
open System.IO
open System.Text
open System.Threading
open System.Net.WebSockets
open System.Collections.Generic
open System.Collections.Concurrent

open NetMQ
open NetMQ.Sockets

open Util.Cat
open Util.Bin
open Util.Text
open Util.Perf
open Util.Http
open Util.HttpServer
open Util.Concurrent

let mutable isEnable = true

type Error =
| OK
| Unprocessed
| Ignore
| Internal
| InvalidMsg
| InvalidSrc
| ApiNotExists
| InvalidAPIKey
| InvalidParameters

type LogLevel =
| All = 0
| Debug = 1
| Production = 2
| None = 3

type Packet = {
client:int64
bin:byte[] }

type WsPacket = {
binOrTxt:bool
client:int64
bin:byte[] }

NetMQConfig.ThreadPoolSize <- 
    let n = Environment.ProcessorCount / 4
    match n with
    | 0 | 1 -> 2
    | _ -> n

let create__pakcet client bin = 
    {   client = client
        bin = bin }

let create__wspacket binOrTxt client bin =
    {   binOrTxt = binOrTxt
        client = client
        bin = bin }

let createZmqSocket helloMsg identity =
    let socket = new RouterSocket()
    socket.Options.RouterMandatory <- true
    socket.Options.Identity <- identity
    socket.Options.HelloMessage <- helloMsg
    socket

// 一个网络基础设施实例，绑定一个进程的特定端口
type ZmqNet = {
s:RouterSocket
mutable mid:int64 ref
inbounds:ConcurrentQueue<NetMQMessage>
outbounds:ConcurrentQueue<NetMQMessage>
results:ConcurrentDictionary<int64*byte[],byte[]>
mutable timeout:float }

let createZmqNet (socket: RouterSocket) = {
    s = socket
    mid = ref 0L
    inbounds = new ConcurrentQueue<NetMQMessage>()
    outbounds = new ConcurrentQueue<NetMQMessage>()
    results = new ConcurrentDictionary<int64*byte[], byte[]>(idbinComparer)
    timeout = 15.0 }

let getZmqAsyncRes znet (mid,nid) timeout (cancellationo: (byte array->bool) option) =

    let o = ref [| |]

    let start = DateTime.UtcNow
    let mutable proceed = true
    while proceed do        
        Thread.Sleep 10
        znet.results.TryGetValue((mid,nid),o) |> ignore
        if o.Value.Length > 0 || DateTime.UtcNow.Subtract(start).TotalSeconds > timeout ||
           (cancellationo.IsSome && cancellationo.Value nid) then
            (mid,nid) |> znet.results.Remove |> ignore
            proceed <- false

    o.Value

let getZmqAsyncResMulti 
        znet 
        (reskeys: (int64*byte array) array) 
        timeout 
        (cancellationo: (byte array->bool) option) = 

    let bins = new List<byte[]>()
    let pendings = new List<int64*byte array>(reskeys)

    let start = DateTime.UtcNow
    let mutable count = pendings.Count
    while count > 0 do
        Thread.Sleep 10
        let mutable i = 0
        while i < count do
            let id = pendings[i]
            let o = ref [| |]
            if znet.results.TryGetValue(id,o) then
                let bin = o.Value
                if bin.Length > 0 || 
                   (cancellationo.IsSome && id |> snd |> cancellationo.Value) then
                   if bin.Length > 0 then
                       bins.Add bin
                   pendings.RemoveAt i
                   znet.results.Remove id |> ignore                
                   i <- count
            i <- i + 1
        if DateTime.UtcNow.Subtract(start).TotalSeconds > timeout then
            count <- 0
            pendings.ToArray() 
            |> Array.map(fun p -> znet.results.TryRemove p)
            |> ignore
        else
            count <- pendings.Count
    bins

// Single-Producer-Multiple-Consumer
type SPMCQueue<'T> = 
    { batchsize:int32
      w:List<'T>
      r:ConcurrentQueue<'T array> }

    static member New() =
        {   batchsize = 16
            w = new List<'T>()
            r = new ConcurrentQueue<'T array>() }

    member this.Empty() = this.r.IsEmpty

    member this.Add item =
        this.w.Add item
        if this.w.Count = this.batchsize then
            this.w.ToArray() |> this.r.Enqueue
            this.w.Clear()

    member this.Pump() =
        if this.w.Count > 0 then
            this.w.ToArray() |> this.r.Enqueue
            this.w.Clear()

    member this.Batch() =
        match this.r.TryDequeue() with
        | true, arr -> arr
        | false,_ -> [||]

type WsMsgType =
| Text = 0
| Binary = 1

type WsHeader = {
totalLength:int
offset:int
masks:byte[]
multiFrm:bool}

type ZmqWeb = {
logLevel:LogLevel
echonum:int
port:int
s:RouterSocket
lastClient: int64 ref
mutable mid:int64 ref
mutable countInbound:int64 ref
mutable countOutbound:int64 ref
frameReceiver: NetMQMessage

inboundHttp:ConcurrentQueue<Packet>
inboundWs:ConcurrentQueue<WsPacket>
outboundHttp:ConcurrentQueue<Packet>
outboundWs:ConcurrentQueue<Packet>

clients:ConcurrentDictionary<int64,ConcurrentQueue<byte[]>>
disconnector: List<int64 -> unit> }

let wsnull = {
    totalLength = 0
    offset = 0
    masks = [||]
    multiFrm = false }

let wsMsgHeader (bin:byte[]) (header:WsHeader ref) =
    try
        if bin[1] &&& byte 0b10000000 <> byte 0 then // must be true, "All messages from the client to the server have this bit set"
            
            let mutable offset = 2
            let mutable msglen = (bin[1] &&& byte 0b01111111) |> int32
            match msglen with
            | 126 -> 
                let a = [| bin[3]; bin[2] |]
                msglen <- int32 <| BitConverter.ToUInt16(a, 0)
                offset <- 4
            | 127 -> 
                let a = [|bin[9]; bin[8]; bin[7]; bin[6]; bin[5]; bin[4]; bin[3]; bin[2]|]
                msglen <- int32(BitConverter.ToUInt64(a, 0))
                offset <- 10
            | _ -> ()
    
            if msglen = 0 then
                header.Value <- wsnull
            else
                let masks = [|bin[offset]; bin[offset + 1]; bin[offset + 2]; bin[offset + 3]|]
                header.Value <-
                    {
                        totalLength = msglen
                        offset = offset+4
                        masks = masks
                        multiFrm = msglen > bin.Length-offset-4
                    }   
        else header.Value <- wsnull
    with ex -> header.Value <- wsnull

let wsMsgDecode (masks:byte[]) offset (bin:byte[]) =
    if bin.Length = 0 || masks.Length = 0 then
        [||]
    else
        let len = bin.Length - offset
        let decoded = Array.zeroCreate len
        let mutable i = 0
        while i < len do
            decoded[i] <- byte (bin[i + offset] ^^^ masks[i % 4])
            i <- i + 1
        decoded

let wsDecode (bin:byte[]) =
    try
        let mask = bin[1] &&& byte 0b10000000 <> byte 0 // must be true, "All messages from the client to the server have this bit set"
        let mutable offset = 2
        let mutable msglen = (bin[1] &&& byte 0b01111111) |> int32
        match msglen with
        | 126 -> 
            // bytes are reversed because websocket will print them in Big-Endian, whereas
            // BitConverter will want them arranged in little-endian on windows
            let a = [| bin[3]; bin[2] |]
            msglen <- int32 <| BitConverter.ToUInt16(a, 0)
            offset <- 4
        | 127 -> 
            // To test the below code, we need to manually buffer larger messages — since the NIC's autobuffering
            // may be too latency-friendly for this code to run (that is, we may have only some of the bytes in this
            // websocket frame available through client.Available).
            let a = [|bin[9]; bin[8]; bin[7]; bin[6]; bin[5]; bin[4]; bin[3]; bin[2]|]
            msglen <- int32(BitConverter.ToUInt64(a, 0))
            offset <- 10
        | _ -> ()
    
        if msglen = 0 then
            Some [||]
        else
            if mask then
                let decoded = Array.zeroCreate msglen
                let masks = [| bin[offset]; bin[offset + 1]; bin[offset + 2]; bin[offset + 3] |]
                offset <- offset + 4
                let mutable i = 0
                while (i < msglen) && (i < bin.Length - offset) do
                    decoded[i] <- byte (bin[offset + i] ^^^ masks[i % 4])
                    i <- i + 1    
                Some decoded
            else
                None
    with ex -> None

let wsEncode (bin: byte[], opcode) =
    let mutable frame = Array.zeroCreate 10    
    let mutable indexStartRawData = -1
    let mutable length = bin.Length
    
    frame[0] <- byte (128 + opcode)
    if length <= 125 then
        frame[1] <- byte length
        indexStartRawData <- 2
    elif length >= 126 && length <= 65535 then
        frame[1] <- byte 126
        frame[2] <- byte ((length >>> 8) &&& 255)
        frame[3] <- byte (length &&& 255)
        indexStartRawData <- 4
    else    
        frame[1] <- byte 127
        frame[2] <- byte ((length >>> 56) &&& 255)
        frame[3] <- byte ((length >>> 48) &&& 255)
        frame[4] <- byte ((length >>> 40) &&& 255)
        frame[5] <- byte ((length >>> 32) &&& 255)
        frame[6] <- byte ((length >>> 24) &&& 255)
        frame[7] <- byte ((length >>> 16) &&& 255)
        frame[8] <- byte ((length >>> 8) &&& 255)
        frame[9] <- byte (length &&& 255)    
        indexStartRawData <- 10    
    
    let response = Array.zeroCreate (indexStartRawData + length)
    let mutable i = 0
    let mutable reponseIdx = 0
    //Add the frame bytes to the reponse
    while i < indexStartRawData do 
        response[reponseIdx] <- frame[i]
        i <- i + 1
        reponseIdx <- reponseIdx + 1    
    
    //Add the data bytes to the response
    i <- 0
    while i < length do 
        response[reponseIdx] <- bin[i]
        i <- i + 1
        reponseIdx <- reponseIdx + 1
    response

let str__wsEncode (s:String) =
    if s = "" then
        [||]
    else
        (Encoding.UTF8.GetBytes s, 1) |> wsEncode

let bin__wsEncode bin = (bin, 2) |> wsEncode

let frame__clientID (frame:NetMQMessage) = 
    let bs = 
        [|  frame[0].ToByteArray true
            Array.zeroCreate 3 |]
        |> Array.concat
    BitConverter.ToInt64(bs,0)

let logLevelQualificate zweb (l:LogLevel) = EnumToValue zweb.logLevel <= EnumToValue l

let create__ZWeb echonum port logLevel localonly (disconnector: (int64 -> unit)[]) = 
    {   logLevel = logLevel
        echonum = echonum
        port = port
        s =             
            let s = new RouterSocket()
            s.Options.RouterRawSocket <- true        
            s.Options.RouterMandatory <- true
            s.Options.ReceiveHighWatermark <- 2000
            s.Options.SendHighWatermark <- 2000
            if localonly then
                "tcp://127.0.0.1:" + port.ToString() |> s.Bind
            else
                "tcp://*:" + port.ToString() |> s.Bind
            s
        lastClient = ref 0L
        mid = ref 0L
        countInbound = ref 0L
        countOutbound = ref 0L
        frameReceiver = new NetMQMessage()

        inboundHttp = ConcurrentQueue<Packet>()
        inboundWs = new ConcurrentQueue<WsPacket>()
        outboundHttp = new ConcurrentQueue<Packet>()
        outboundWs = new ConcurrentQueue<Packet>()

        clients = new ConcurrentDictionary<int64,ConcurrentQueue<byte[]>>()
        disconnector = 
            let res = new List<int64 -> unit>()
            disconnector |> res.AddRange
            res }

let repping = 
    "{\"Error\":\"OK\"}"
    |> Util.HttpServer.str__StandardResponse "application/json"

let tryReceivePacket zweb = 
    zweb.frameReceiver.Clear()
    let received = zweb.s.TryReceiveMultipartMessage(ref zweb.frameReceiver)
    if received && zweb.frameReceiver.FrameCount > 1 then
        {   client = 
                zweb.frameReceiver[0].ToByteArray true 
                |> padRight 8
                |> BitConverter.ToInt64
            bin = zweb.frameReceiver[1].ToByteArray true } |> Some
    else
        None
    
let pushHttpPacket zweb item = 
    zweb.outboundHttp.Enqueue item
    Interlocked.Increment zweb.countOutbound |> ignore

let pushWsPacket zweb item = 
    zweb.outboundWs.Enqueue item
    Interlocked.Increment zweb.countOutbound |> ignore

let trySend zweb (packet:Packet, httpOrWs) = 

    let msg = new NetMQMessage(2)
    let dst = Array.sub (BitConverter.GetBytes packet.client) 0 5
    dst |> msg.Append
    packet.bin |> msg.Append    

    if httpOrWs then
        let msgClose = new NetMQMessage(2)
        msgClose.Append dst
        msgClose.AppendEmptyFrame()
        zweb.s.TrySendMultipartMessage msg |> ignore
        zweb.s.TrySendMultipartMessage msgClose
    else
        zweb.lastClient.Value <- packet.client
        if zweb.s.TrySendMultipartMessage msg = false then
            pushWsPacket zweb packet
            false
        else
            true

let clientRemover zweb id =
    if zweb.clients.ContainsKey id then
        if zweb.clients.TryRemove id |> fst then
            ""
            |> Encoding.UTF8.GetBytes
            |> create__pakcet id
            |> pushWsPacket zweb

let wsReceive zweb client =
    
    let mutable mergingMsg = false
    let msgs = new List<byte[]>()

    let mutable mergingFrm = false
    let bins = new List<byte[]>()    
    let mutable receivedlen = 0
    
    let header = ref wsnull
    let mutable isBino = None

    let mutable idlestart = DateTime.UtcNow.Ticks

    let mutable lastUnhandled = [||]

    let reset__frm () =
        bins.Clear()
        receivedlen <- 0
        header.Value <- wsnull
        mergingFrm <- false

    let reset__msg () =        
        msgs.Clear()
        mergingMsg <- false

    let inbound_msg () =
        if mergingMsg then
            msgs.ToArray() 
            |> Array.concat
            |> create__wspacket isBino.Value client
            |> zweb.inboundWs.Enqueue
            reset__msg()

    let merge_frms bins masks isbin =
        bins
        |> Seq.toArray 
        |> Array.concat
        |> wsMsgDecode masks 0
        |> msgs.Add
        reset__frm() 
    
    while zweb.clients.ContainsKey client do

        let rc, next = zweb.clients[client].TryDequeue()
        if not rc then
            Thread.Sleep 1000
            if mergingMsg then
                let sp = (DateTime.UtcNow.Ticks - idlestart) |> TimeSpan.FromTicks
                if sp.TotalSeconds > 2.0 then
                    inbound_msg()
        else

            let bin = 
                if lastUnhandled.Length > 0 then
                    let bs = Array.append lastUnhandled next
                    lastUnhandled <- [||]
                    bs
                else
                    next

            if mergingMsg then
                idlestart <- DateTime.UtcNow.Ticks
            if mergingFrm then                                
                if receivedlen + bin.Length = header.Value.totalLength then
                    bins.Add bin
                    merge_frms bins header.Value.masks isBino.Value
                elif receivedlen + bin.Length > header.Value.totalLength then
                    let lastlen = header.Value.totalLength - receivedlen
                    Array.sub bin 0 lastlen |> bins.Add
                    lastUnhandled <- Array.sub bin lastlen (bin.Length-lastlen)
                    merge_frms bins header.Value.masks isBino.Value
                else
                    bins.Add bin
                    receivedlen <- receivedlen + bin.Length
            else
                let opcode = int(bin[0] &&& (byte 0b00001111))
                match opcode with
                | 0 | 1 | 2 -> 
                    if opcode = 1 || opcode = 2 then
                        inbound_msg()

                    wsMsgHeader bin header
                    if header.Value.multiFrm then     
                        receivedlen <- bin.Length - header.Value.offset
                        Array.sub bin header.Value.offset receivedlen |> bins.Add
                        isBino <- Some(opcode=2)
                        mergingFrm <- true
                        mergingMsg <- true
                    else                        
                        let data = wsMsgDecode header.Value.masks header.Value.offset bin
                        if mergingMsg && opcode = 0 then
                            msgs.Add data
                        elif opcode = 1 || opcode = 2 then
                            create__wspacket (opcode = 2) client data
                            |> zweb.inboundWs.Enqueue
                        else
                            reset__frm()
                            reset__msg()                            
                | 8 -> 
                    clientRemover zweb client
                    zweb.disconnector.ToArray() |> Array.iter(fun h -> h client)
                | _ -> ()  

let fileService root req = 

    if req.pathline = "/" || req.pathline = "/nm" then
        Path.Combine(root, "index.html")
        |> IO.File.ReadAllText
        |> str__StandardResponse "text/html"
    elif req.path.Length > 0 then
        let file = Array.append [|root|] req.path |> Path.Combine
        if File.Exists file then
            let filename = req.path[req.path.Length - 1]
            match (Path.GetExtension filename).ToLower() with
            | ".jpg" | ".png" | ".gif" | ".ico" as ext -> 
                file
                |> IO.File.ReadAllBytes
                |> bin__StandardResponse ("image/" + (ext.Substring 1))
            | ".css" | ".html" | ".javascript" | ".txt" | ".xml" as ext ->
                file
                |> IO.File.ReadAllText
                |> str__StandardResponse ("text/" + (ext.Substring 1))
            | ".js" ->
                file
                |> IO.File.ReadAllText
                |> str__StandardResponse "text/javascript; charset=utf-8"
            | ".mp4" ->
                file
                |> IO.File.ReadAllBytes
                |> bin__StandardResponse "video/mp4"
            | _ -> [||]
        else [||]
    else [||]

let reqhandler apiService fileServiceo output req =

    if req.path.Length > 0 && req.path[0] = "api" then
        apiService output req
    else
        match fileServiceo with
        | Some fileService -> req |> fileService
        | None -> [||]

let httpHandler
    (reqhandler: (string -> unit) -> HttpRequest -> byte array)
    output 
    zweb =

    match zweb.inboundHttp.TryDequeue() with
    | true, p ->     
        async {
            //try
                let reqo,(headers,body) = bs__httpRequest p.bin
                match reqo with
                | Some req ->
                    let repbin = req |> reqhandler output
                    if repbin.Length > 0 then
                        repbin 
                        |> create__pakcet p.client
                        |> pushHttpPacket zweb
                | None -> ()
            //with ex -> ex.Message |> output
        } |> Async.Start
    | _ -> Thread.Sleep 10

let startTcpService (output:string->unit) zweb httpHandler wsHandler =

    // Receiving/Sending thread
    async {
        let mutable q = new ConcurrentQueue<byte[]>()
        while true do
            try
                match tryReceivePacket zweb with
                | Some p ->
                    match checkHttpUpgrade p.bin with
                    | _,false -> 
                        zweb.inboundHttp.Enqueue p
                    | upgrade,_ ->
                        if upgrade.Length > 0 then
                            create__pakcet p.client upgrade
                            |> pushWsPacket zweb
                        else
                            let mutable added = true
                            zweb.clients.AddOrUpdate(p.client, q, fun k q ->
                                q.Enqueue p.bin
                                added <- false
                                q) |> ignore
                            if added then
                                q.Enqueue p.bin
                                q <- new ConcurrentQueue<byte[]>()
                                async { wsReceive zweb p.client } |> Async.Start                            
                    Interlocked.Increment zweb.countInbound |> ignore
                | None -> ()
            
                match zweb.outboundHttp.TryDequeue() with
                | true, ph -> trySend zweb (ph,true) |> ignore
                | false, _ -> ()

                let mutable proceed = true
                while proceed do
                    match zweb.outboundWs.TryDequeue() with
                    | true, ph -> proceed <- trySend zweb (ph,false)
                    | false,_ -> proceed <- false
            with
            | :? HostUnreachableException as ex ->                    
                if zweb.lastClient.Value > 0L then
                    clientRemover zweb zweb.lastClient.Value
                    zweb.disconnector.ToArray()
                    |> Array.iter(fun h -> h zweb.lastClient.Value)
            | _ -> ()
    }
    |> Async.Ignore
    |> Async.Start

    // http queue
    //[|1 .. zweb.echonum|]
    //|> Array.iter(fun i -> 
    //    asyncCycler (fun _ -> httpHandler output zweb)
    //    Thread.Sleep 10
    //    "Http echo " + i.ToString() + " started" |> output)
    asyncCycler (fun _ -> httpHandler output zweb)

    // ws queue
    asyncCycler (fun _ -> 
        if zweb.inboundWs.IsEmpty then
            Thread.Sleep 100
        else
            let rc, wsp = zweb.inboundWs.TryDequeue()
            async {
                match wsHandler zweb wsp with
                | Some (bs:byte array) -> 
                    if bs.Length > 0 then
                        bs 
                        |> create__pakcet wsp.client
                        |> pushWsPacket zweb
                | None -> ()
            } |> Async.Start)

let lauchWebServer output httpHandler wsHandler zweb =    
                
    startTcpService output zweb httpHandler wsHandler

    if logLevelQualificate zweb LogLevel.Production then
        "TCP service started at " + zweb.port.ToString()
        |> output

type ApiCtx<'Runtime,'Session, 'Error> = { 
runtime: 'Runtime    
mutable since: DateTime
mutable service: string
mutable api: string
mutable ip: string
mutable procedureo: (ApiCtx<'Runtime, 'Session, 'Error> -> 'Error) option
mutable postdata: string
mutable cached: string
mutable fields: Dictionary<string, string>
mutable sessiono: 'Session option
w:TextBlockWriter  }


let empty__ApiCtx runtime = 

    {   runtime = runtime
        since = DateTime.UtcNow
        service = ""
        api = ""
        ip = ""
        procedureo = None
        postdata = ""
        cached = ""
        fields = new Dictionary<string,string>()
        sessiono = None
        w = empty__TextBlockWriter() }


let req__ApiCtx runtime req = 

    let service, api =
        let mutable pathline = req.pathline

        if pathline.StartsWith "/" then
            pathline <- pathline.Substring(1, pathline.Length - 1)

        let path = pathline.Split "/"
        if path.Length >= 3 then
            let i = path[2].IndexOf '?'
            if i > 0 then
                path[1], path[2].Substring(0, i)
            else
                path[1], path[2]
        else "", ""
    
    {   runtime = runtime
        since = DateTime.UtcNow
        service = service
        api = api
        ip = ""
        procedureo = None
        postdata = req.body
        cached = ""
        fields = req.query
        sessiono = None
        w = empty__TextBlockWriter() }

let bindRes x p = 
        x.procedureo <- Some p
        Suc x

let bindOK x p = 
        x.procedureo <- Some(fun x -> 
            "{\"Error\":\"OK\"," |> x.w.newline
            p x
            "}" |> x.w.newline
            Error.OK)
        Suc x

let ok (w:TextBlockWriter) = 
    "{\"Error\":\"OK\"}" |> w.newline
    None

let httpEcho folder runtime branch output req =

    let x = 
        req 
        |> req__ApiCtx runtime

    if x.service.Length * x.api.Length > 0 then
        match 
            x
            |> Suc
            |> bind(fun h -> branch x) with
        | Suc x ->
            let e = 
                match x.procedureo with
                | Some p ->
                    use cw = new CodeWrapper("Api." + x.api)
                    try
                        p x
                    with ex -> 
                        x.w.clear()
                        [|  "{\"Error\":\"Failed\",\"Response\":\""
                            ex.Message
                            "\"}" |]
                        |> x.w.multiLine
                        Error.Internal
                | None -> 
                    Error.OK

            if e <> Error.OK then
                if x.w.count() = 0 then
                    "{\"Error\":\"" + e.ToString() + "\"}" |> x.w.newline
        | Fail(e, x) -> "{\"Error\":\"" + e.ToString() + "\"}" |> x.w.newline

        x.w.text()
        |> str__StandardResponse "application/json"
    else
        fileService folder req

let wsReqRep dst (dataType:WebSocketMessageType, utf8Bytes:byte[]) =
    // let dst = "ws://127.0.0.1:" + testport.ToString()

    try
        let buffer = ArraySegment<byte>(Array.zeroCreate 1024)
        let mutable client = new ClientWebSocket()
        client.ConnectAsync(Uri(dst), CancellationToken.None).Wait()

        let msg = utf8Bytes |> ArraySegment<byte>

        client.SendAsync(msg, dataType, true, CancellationToken.None).Wait()
        let result = client.ReceiveAsync(buffer, CancellationToken.None).GetAwaiter().GetResult()
        let str = Encoding.UTF8.GetString(buffer.Array, 0, result.Count)
        
        Array.Clear(buffer.Array, 0, result.Count)
        str
    with ex -> 
        // client <- new ClientWebSocket()
        // client.ConnectAsync(new Uri(dst), CancellationToken.None).Wait()
        Console.WriteLine $"Util.ZwsBoost.wsReqRep ws connect failed"
        ""        

let wsReqRep_txt dst (msg:string) =
    wsReqRep dst (WebSocketMessageType.Text, msg |> Encoding.UTF8.GetBytes)

let wsReqRep_bin dst (binData:byte[]) =
    wsReqRep dst (WebSocketMessageType.Binary, binData)




//*********************WebSocket Client*********************

type WsClientState = 
| Connected
| Connecting
| Closing
| NotAvailable

type WsClient = {
mutable state: WsClientState
ws:ClientWebSocket
outbound:ConcurrentQueue<WsMsgType*byte[]>
url:String
timeout:int }

let create__WsClient timeout url = 
    {   state = WsClientState.NotAvailable
        ws = new ClientWebSocket()
        outbound = new ConcurrentQueue<WsMsgType*byte[]>()
        url = url
        timeout = timeout }

let WsConnect client =
    client.state <- WsClientState.Connecting
    async {        
        client.ws.ConnectAsync(Uri(client.url), CancellationToken.None).Wait()
        client.state <- WsClientState.Connected
    } |> Async.Start

let WsReconnect client =
    lock client (fun _ ->
    if client.state = WsClientState.Connected then
        client.state <- WsClientState.NotAvailable
        WsConnect client)

let WsClose client =
    client.state <- WsClientState.Closing
    async {
        client.ws.CloseAsync(WebSocketCloseStatus.NormalClosure, "", CancellationToken.None).Wait()
        client.state <- WsClientState.NotAvailable
    } |> Async.Start

let WsPush client = client.outbound.Enqueue

let launchWsSend (client:WsClient) =
    async {
        while true do
            match client.state with
            | WsClientState.Connected ->
                match client.outbound.TryDequeue() with
                | true, (t,bin) -> 
                    if client.ws.State = WebSocketState.Open then
                        //let cs = new CancellationTokenSource(client.timeout)
                        let bytesToSend = new ArraySegment<byte>(bin)
                        let msgtype = 
                            match t with
                            | WsMsgType.Text -> WebSocketMessageType.Text
                            | _ -> WebSocketMessageType.Binary
                        try                            
                            client.ws.SendAsync(bytesToSend, msgtype, true, CancellationToken.None).Wait()
                        with
                            | :? InvalidOperationException
                            | :? ObjectDisposedException -> WsReconnect client
                            | ex -> ex.Message |> Console.WriteLine
                | _ -> Thread.Sleep 100
            | _ -> Thread.Sleep 100            
    } |> Async.Start

let launchWsReceive client wsHandler =
    
    let bytes_WsData (result:WebSocketReceiveResult) (bytesReceived:ArraySegment<byte>) =

        let receivedbin = Array.sub bytesReceived.Array 0 result.Count
        match result.MessageType with
        | WebSocketMessageType.Text ->
            //let str = receivedbin |> Encoding.UTF8.GetString
            Array.Clear(bytesReceived.Array, 0, result.Count)
            //str |> Encoding.UTF8.GetBytes |> Some
            receivedbin |> Some
        | WebSocketMessageType.Binary ->
            Array.Clear(bytesReceived.Array, 0, result.Count)
            receivedbin |> Some
        | WebSocketMessageType.Close -> 
            WsClose client
            None
        | _ -> None

    let mergefrms (frms:List<byte[]>) =
        if frms.Count > 1 then
            frms |> Seq.toArray |> Array.concat |> Some
        else None

    async {
        let bytesReceived = new ArraySegment<byte>(Array.zeroCreate 8192)
        let frames = new List<byte[]>()
        while true do
            match client.state with
            | WsClientState.Connected ->
                try
                    match client.ws.State with
                    | WebSocketState.Open ->
                        let result = client.ws.ReceiveAsync(bytesReceived, CancellationToken.None).GetAwaiter().GetResult()
                        if result.EndOfMessage then
                            match bytes_WsData result bytesReceived with
                            | Some wsdata -> 
                                if frames.Count > 0 then
                                    frames.Add wsdata
                                    match mergefrms frames with
                                    | Some d ->
                                        async { 
                                            wsHandler (result.MessageType = WebSocketMessageType.Binary) d 
                                            } |> Async.Start
                                    | None -> ()
                                else
                                    async { 
                                        wsHandler (result.MessageType = WebSocketMessageType.Binary) wsdata 
                                        } |> Async.Start
                            | None -> ()
                            frames.Clear()
                        else
                            match bytes_WsData result bytesReceived with
                            | Some wsdata -> frames.Add wsdata
                            | None -> frames.Clear()
                    | WebSocketState.CloseReceived
                    | WebSocketState.Closed
                    | WebSocketState.Aborted -> WsReconnect client
                    | _ -> ()
                with 
                    | :? InvalidOperationException
                    | :? ObjectDisposedException -> WsReconnect client
                    | ex -> ex.Message |> Console.WriteLine
            | _ -> Thread.Sleep 100 
    } |> Async.Start

let launchWsClient output client wshandler =
    
    WsConnect client
    launchWsReceive client wshandler
    launchWsSend client

    "Websocket client launched" |> output

let expiry timeout = DateTime.UtcNow.AddMinutes(-timeout)

let pushWs__conn zweb connid = create__pakcet connid >> pushWsPacket zweb
let binPushWs__conn zweb connid = bin__wsEncode >> (pushWs__conn zweb connid)
let strPushWs__conn zweb connid = str__wsEncode >> (pushWs__conn zweb connid)
    
let pushWsToAll zweb (clients:int64 array) encoded = 
    clients
    |> Array.iter(fun client -> 
        pushWsPacket zweb {client = client; bin = encoded})
let strPushWsToAll zweb clients = str__wsEncode >> (pushWsToAll zweb clients)
let binPushWsToAll zweb clients = bin__wsEncode >> (pushWsToAll zweb clients)