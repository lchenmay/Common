module Util.HighPerfNet

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Net
open System.Net.Sockets
open System.Threading

open Util.Perf
open Util.Bin



type NetState =
| Init
| Accepted
| Received
| Sending
| Available

type Conn =
    {
        id: int;
        mutable reuse_count:int64;
        mutable occupied: bool;
        mutable state: NetState;

        mutable socket: Socket option;
        receiver: SocketAsyncEventArgs;
        sender: SocketAsyncEventArgs;

        bb: BytesBuilder;
        mutable response_bin: byte[];
        mutable response_send_index: int;

        mutable acceptedat: DateTime;
        mutable receivedat: DateTime;
        mutable sendstartedat: DateTime;
        mutable sendendedat: DateTime }

let empty_conn(i) =
    let res = 
        {
            id = i;
            reuse_count = 0L;
            occupied = false;
            state = NetState.Init;

            socket = None;
            receiver = new SocketAsyncEventArgs();
            sender = new SocketAsyncEventArgs();

            bb = new BytesBuilder();
            response_bin = [||];
            response_send_index = 0;

            acceptedat = DateTime.MinValue;
            receivedat = DateTime.MinValue;
            sendstartedat = DateTime.MinValue;
            sendendedat = DateTime.MinValue }

    res

let mutable buffer_bytes = 0
let mutable max_occupied = 0
let mutable max_inbound_bytes = 0
let mutable max_outbound_bytes = 0
let mutable total_count = 0L

type Server1(buffer_size,max_conn,port,loggero,on_incoming) =
            
    let conns = new ConcurrentQueue<Conn>()
    let pool = new ConcurrentStack<Conn>()
    let inuse = new ConcurrentBag<Conn>()
    let mutable top = 0

    let log(loc,s) = 
        match loggero with
        | Some logger -> logger(loc,s)
        | None -> ()

    let error_handler(loc,conn:Conn)(exn:exn) = 
        // System.Net.Sockets.SocketException (0x80004005): 远程主机强迫关闭了一个现有的连接。
        let text = exn.ToString()
        let msg = conn.id.ToString() + text

        let mutable ignore = false
        if(text.Contains("(10053)")) then//你的主机中的软件中止了一个已建立的连接
            ignore <- true
        if(text.Contains("(0x80004005)") || text.Contains("(10054)")) then//远程主机强迫关闭了一个现有的连接
            ignore <- true
            
        if(ignore = false) then
            log(loc,msg)

    let listen_socket =
        let local = new IPEndPoint(IPAddress.Any, port)
        let socket = new Socket(local.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
        socket.ReceiveBufferSize <- buffer_size
        socket.SendBufferSize <- buffer_size
        if (local.AddressFamily = AddressFamily.InterNetworkV6) then
            socket.SetSocketOption(SocketOptionLevel.IPv6, SocketOptionName.IPv6Only, false)
            socket.Bind(new IPEndPoint(IPAddress.IPv6Any, local.Port))
        else
            socket.Bind(local)
        socket

    let empty = empty_conn(0)

    let mutex = new Mutex()

    member this.desc() = 
        ("".PadRight(top,'.')).PadLeft(max_conn,'X')

    member private this.push(conn) =
        log("PUSH/POP " + total_count.ToString(),"".PadRight(top,'.') + "".PadRight(max_conn - top,'X'))
        inuse.TryTake(ref conn) |> ignore
        pool.Push(conn)
        conn.occupied <- false
        conn.acceptedat <- DateTime.MinValue
        top <- top + 1

    member private this.pop() =
        log("PUSH/POP " + total_count.ToString(),"".PadRight(top,'.') + "".PadRight(max_conn - top,'X'))

        let mutable result = ref empty
        if(pool.TryPop(result))then
            top <- top - 1
            inuse.Add(result.Value)
            result.Value.occupied <- true

            if(max_occupied < max_conn - pool.Count)then
                max_occupied <- max_conn - pool.Count

            result.Value.reuse_count <- System.Threading.Interlocked.Increment(&result.Value.reuse_count)
            total_count <- System.Threading.Interlocked.Increment(&total_count)

            result.Value.response_send_index <- 0

            Some(result.Value)
        else
            None
        
    member this.closeClientSocket(socket:Socket,ctx:SocketAsyncEventArgs,caller) =
        use cw = new CodeWrapper("IOCP.closeClientSocket")
        log("closeClientSocket","")

        let conn = ctx.UserToken :?> Conn

        try
            socket.Shutdown(SocketShutdown.Both)
            socket.Close()
        with exn -> 
            exn |> error_handler("IOCP.close_client_socket",conn)

        this.push(conn)
        conn.state <- NetState.Available
        conn.socket <- None

    member this.onAcceptCompleted(sender:Object)(ctx:SocketAsyncEventArgs) =
        use cw = new CodeWrapper("IOCP.onAcceptCompleted")
        log("onAcceptCompleted","")

        let socket = ctx.AcceptSocket
        if(socket = null)then
            ()
        else if(socket.Connected)then
            let conno = this.pop()
            if(conno.IsNone)then
                try
                    socket.Shutdown(SocketShutdown.Both)
                    socket.Close()
                with exn ->
                    ()
            else

                let conn:Conn = conno.Value
                conn.socket <- Some(socket)
                conn.receiver.UserToken <- conn
                conn.sender.UserToken <- conn
                conn.state <- NetState.Accepted

                conn.bb.clear()

                conn.acceptedat <- DateTime.UtcNow

                try
                    if(socket.ReceiveAsync(conn.receiver) = false)then
                        this.onReceiveCompleted("SYN")(conn.receiver)
                         
                with exn -> 
                    exn |> error_handler("IOCP.on_accept_completed/Receive",conn)

            this.startAccept(ref ctx)

    member this.onReceiveCompleted(sender:Object)(ctx:SocketAsyncEventArgs) =
        use cw = new CodeWrapper("IOCP.onReceiveCompleted")
        log("onReceiveCompleted","")

        let conn = ctx.UserToken :?> Conn
        if(conn.socket.IsNone)then
            this.push(conn)
            conn.state <- NetState.Available
        else
            let socket = conn.socket.Value
            try
                conn.state <- NetState.Received

                //远程主机已关闭连接
                if(ctx.BytesTransferred = 0)then
                    this.push(conn)
                    conn.state <- NetState.Available
                    conn.socket <- None
                else if(ctx.SocketError <> SocketError.Success) then
                    this.closeClientSocket(socket,ctx,"onReceiveCompleted")
                else
                    let count = ctx.BytesTransferred
                    let bs = Array.zeroCreate(count)
                    Array.Copy(ctx.Buffer, bs, count)
                    conn.bb.append(bs)

                    //判断所有需接收的数据是否已经完成
                    if(socket.Available = 0)then
                        conn.receivedat <- DateTime.UtcNow
                        //Perf.log_cw("IOCP/accepted->received",conn.receivedat.Subtract(conn.acceptedat).Ticks, 0L)
                            
                        on_incoming(conn)
                        this.respose(conn.sender)
                                
                    else if(socket.ReceiveAsync(conn.receiver) = false) then
                        //为接收下一段数据，投递接收请求，这个函数有可能同步完成，这时返回false，并且不会引发SocketAsyncEventArgs.Completed事件
                        this.onReceiveCompleted("SYN")(conn.receiver)

            with exn ->
                exn |> error_handler("closeClientSocketCP.onReceiveCompleted/Line307",conn)
                this.closeClientSocket(socket,ctx,"onReceiveCompleted")

    member this.onSendCompleted(sender:Object)(ctx:SocketAsyncEventArgs) =
        use cw = new CodeWrapper("IOCP.onSendCompleted")
        log("onSendCompleted","")

        let conn = ctx.UserToken :?> Conn

        conn.sendendedat <- DateTime.UtcNow
        //Perf.log_cw("IOCP/sending",conn.sendendedat.Subtract(conn.sendstartedat).Ticks, 0L)
        if(conn.socket.IsSome) then
            try
                let socket = conn.socket.Value
                this.closeClientSocket(socket,ctx,"onSendCompleted")
            with exn ->
                exn |> error_handler("IOCP.onSendCompleted",conn)

    member this.respose(ctx:SocketAsyncEventArgs) =
        use cw = new CodeWrapper("IOCP.respose")
        log("respose","")

        let conn = ctx.UserToken :?> Conn
        try
            let socket = conn.socket.Value

            conn.sendstartedat <- DateTime.UtcNow
            conn.state <- NetState.Sending
            let mutable count = 0
            let mutable keep = true
            while (keep) do
                if(socket.Connected)then
                    //待发送数据长度
                    let mutable length = conn.response_bin.Length - conn.response_send_index
                    if (length <= 0) then
                        keep <- false
                    else
                        if (length > ctx.Buffer.Length)then
                            length <- ctx.Buffer.Length
                        else
                            Array.Clear(ctx.Buffer,0,ctx.Buffer.Length)

                        Array.Copy(conn.response_bin, conn.response_send_index, ctx.Buffer, 0, length)

                        let send_length = socket.Send(ctx.Buffer)
                        conn.response_send_index <- conn.response_send_index + send_length

                        count <- count + 1
                else
                    keep <- false

        with exn ->
            exn |> error_handler("IOCP.respose",conn)

        this.onSendCompleted("")(conn.sender)

    member this.startAccept(ctxo:Ref<SocketAsyncEventArgs>) =
        log("startAccept","")

        let ctx =
            if(ctxo.Value=null)then
                ctxo.Value <- new SocketAsyncEventArgs()
                let ctx = ctxo.Value
                ctx.Completed.AddHandler(new EventHandler<SocketAsyncEventArgs>(this.onAcceptCompleted))
                ctx
            else
                let ctx = ctxo.Value
                ctx.AcceptSocket <- null
                ctx

        if(listen_socket.AcceptAsync(ctx)=false)then
            this.onAcceptCompleted("SYN")(ctx)

    member this.occupied() =
        let now = DateTime.UtcNow
        let timeout_warnings = new List<Conn>()
        let occupied =
            lock(conns)(fun() ->
                conns
                |> Seq.filter(fun conn -> conn.occupied)
                |> Seq.toArray)
        occupied

    member this.start() =

        [| 0..max_conn-1 |]
        |> Array.iter(fun i->
            let conn = empty_conn(i)
            conn.receiver.UserToken <- conn
            conn.receiver.Completed.AddHandler(fun s e -> this.onReceiveCompleted(s)(e))
            conn.receiver.SetBuffer(Array.zeroCreate(buffer_size),0,buffer_size)
            conn.sender.UserToken <- conn
            conn.sender.Completed.AddHandler(fun s e -> this.onSendCompleted(s)(e))
            conn.sender.SetBuffer(Array.zeroCreate(buffer_size),0,buffer_size)
            this.push(conn)
            conns.Add(conn))

        listen_socket.Listen(max_conn)
        this.startAccept(ref null)

        mutex.WaitOne() |> ignore

type Ctx = {
    id: int64;
    since: DateTime;
    client: TcpClient;
    stream: System.IO.Stream }

let createCtx(id,client) = {
    id = id;
    since = DateTime.UtcNow;
    client = client;
    stream = client.GetStream() :> System.IO.Stream }

let port = 25072

type Server(loggero,on_incoming) =

    let actual(ctx) = async{

        use cw = new CodeWrapper("Server.actual")

        try
            let client = ctx.client
            let stream = client.GetStream() :> System.IO.Stream

            let buffer = Array.zeroCreate Tcp.bufferLength
            let bin = 

                use cw = new CodeWrapper("Server.read")

                let bb = new BytesBuilder()

                let mutable keep = true
                while(keep) do

                    let count = stream.Read(buffer, 0, buffer.Length)

                    bb.append(buffer,count)
                    if(count < buffer.Length) then
                        keep <- false

                bb.bytes()

            let responseHeader,responseBody =
                bin
                |> on_incoming

            use cw = new CodeWrapper("Server.write")
            stream.Write(responseHeader, 0, responseHeader.Length)
            stream.Write(responseBody, 0, responseBody.Length)
            stream.Flush()
            stream.Close()

            client.Close()

            client.Dispose()

            match loggero with
            | Some(logger) -> logger("Server.actual","Echo done.")
            | None -> ()

        with
        | _ -> ()
    }

    let count = ref 0L

    let queue = new System.Collections.Concurrent.ConcurrentQueue<Ctx>()

    member this.start() = 

        match loggero with
        | Some(logger) -> logger("Server.actual","Launching IOCP/HTTP server: " + port.ToString())
        | None -> ()

        let listener = new TcpListener(IPAddress.Any, port)
        listener.Start()

        async{
            let mutable loggedat = DateTime.UtcNow
            while(true) do
                try
                    let client = listener.AcceptTcpClient()
                    System.Threading.Interlocked.Increment(count) |> ignore
                        
                    queue.Enqueue(createCtx(count.Value,client))

                    if(DateTime.UtcNow.Subtract(loggedat).TotalSeconds > 1.0) then

                        //let desc(k) =
                        //    if(Util.Perf.h_cw.ContainsKey(k)) then
                        //        let v = Util.Perf.cws.[k]
                        //        let elapse = TimeSpan.FromTicks(v.sum).TotalMilliseconds
                        //        let mil = TimeSpan.FromTicks(v.sum).TotalMilliseconds / float(v.count)
                        //        System.Console.WriteLine(k + ": count = " + v.count.ToString() + ", avg = " + mil.ToString("0.00"))


                        //System.Console.WriteLine()
                        //desc("Server.actual")
                        //desc("Server.read")
                        //desc("Server.write")
                        //desc("WebApiEngine.incoming")
                        //desc("WebApiEngine.request")
                        //desc("WebApiEngine.request/regex")
                        //desc("WebApiEngine.auth")
                        //desc("WebApiEngine.dispatcher")
                        //desc("WebApiEngine.exe")
                        //desc("WebApiEngine.response")

                        loggedat <- DateTime.UtcNow

                with
                | ex -> 
                    ()
        }
        |> Async.Start

        [| 0..8-1 |]
        |> Array.map(fun i ->
            async{
                while(true) do
                    let suc,ctx = queue.TryDequeue()
                    if(suc) then
                        do! actual ctx
            })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
