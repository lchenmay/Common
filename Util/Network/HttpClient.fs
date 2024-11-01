module Util.HttpClient

open System
open System.Collections.Generic
open System.Net
open System.Web
open System.Net.Security
open System.Net.Sockets
open System.Security.Cryptography.X509Certificates
open System.Security.Authentication
open System.Text
open System.Text.RegularExpressions

open Util.Bin
open Util.Perf
open Util.Text
open Util.Http
open Util.Console
open Util.TcpServer
open System.Threading


open System.IO
open System.IO.Compression
open Collection

let CRLF = Util.Text.crlf
    
let private stream__bytes(stream:Stream) =
    let bb = new BytesBuilder()
    let buffer = Array.zeroCreate(1024)
    let mutable keep = true
    while(keep) do
        let c = stream.Read(buffer,0,buffer.Length)
        if(c=0)then
            keep<-false
        else if(c=buffer.Length) then
            buffer |> Array.copy |> bb.append
        else
            buffer |> Array.take(c) |> bb.append
    bb.bytes()

let private gzip__bytes(src:byte[]) =
    let ms = new MemoryStream(src)
    use stream = new GZipStream(ms, CompressionMode.Decompress)
    try
        let bs = stream__bytes(stream)
        stream.Close()
        bs
    with
    | ex -> // System.IO.InvalidDataException: GZip 脚注中的 CRC 与从解压缩的数据中计算出的 CRC 不匹配。
        [||]


type HttpParameters = 
    {
        mutable scheme: string;
        mutable host: string;
        mutable port: int;
        mutable secure: bool;
        mutable netloc: string;
        mutable method: string;

        mutable accept: string;
        mutable content_type: string;
        mutable accept_encoding: string;
        mutable user_agent: string;
        mutable connection: string }

        member this.forward remain url =

            let mutable residual =
                HttpUtility.UrlDecode url
            if residual.Contains "://" then
                this.scheme <- residual |> regex_match re_url_scheme
                residual <- residual.Substring(this.scheme.Length + 3)

            match this.scheme with
            | "https"
            | "wss" ->
                this.port <- 443
                this.secure <- true
            | _ ->
                this.secure <- false

            if url.StartsWith "/" then
                residual <- url
            else
                if remain = false then
                    this.host <- residual |> regex_match re_url_host
                    residual <- residual.Substring this.host.Length

                let s_port = residual |> regex_match re_url_port
                if s_port.Length > 0 then
                    this.port <- s_port |> Util.Text.parse_int32
                    residual <- residual.Substring (s_port.Length + 1)

            this.netloc <- residual.Trim()
            if this.netloc.Length = 0 then
                this.netloc <- "/"

            if this.netloc.StartsWith "/" = false then
                this.netloc <- "/" + this.netloc

        member this.url() = 
            [|  this.scheme
                "://"
                this.host
                ":"
                this.port.ToString()
                this.netloc |]
            |> String.Concat

let default_httpparams() = 
    {
        scheme = "http";
        host = "";
        port = 80;
        secure = false;
        netloc = "/";
        method = "GET";

        accept = "*/*";//image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash, application/vnd.ms-excel, application/vnd.ms-powerpoint, application/msword, */*"
        content_type =  "application/json;charset=UTF-8";
        accept_encoding = "gzip, deflate";
        user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36";
        connection = "Keep-Alive" }


type HttpClientRes = 
    {
        since: DateTime;
        mutable duration_dns: TimeSpan;
        mutable duration_conn: TimeSpan;
        mutable duration_request: TimeSpan;
        mutable duration_response: TimeSpan;
        mutable rq_url: string;
        mutable rq_body: string;
        mutable bytes: byte[];
        mutable html: string;
        mutable msg: string;
        mutable returnCode: string;
        mutable returnHeaders: string[];
        mutable elapse: TimeSpan;
        mutable success: bool }

let empty_HttpClientRes() = 
    {
        since = DateTime.UtcNow;
        duration_dns = new TimeSpan();
        duration_conn = new TimeSpan();
        duration_request = new TimeSpan();
        duration_response = new TimeSpan();
        rq_url = "";
        rq_body = "";
        bytes = [||];
        html = "";
        msg = "";
        returnCode = "";
        returnHeaders = [| |];
        elapse = new TimeSpan();
        success = false }


let HttpPostAsync (url: string) (param: string) (contenttype: string) =
    async {
        use client = new System.Net.Http.HttpClient()
        let content = new System.Net.Http.StringContent(param, Encoding.UTF8, contenttype)

        try
            let! response = client.PostAsync(url, content) |> Async.AwaitTask
            response.EnsureSuccessStatusCode() |> ignore

            let! responseStr = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            return true, responseStr
        with
        | ex -> return false, ex.Message
    }

let HttpPost (url: string) (param: string) (contenttype: string) =
    let asyncOperation = HttpPostAsync url param contenttype
    asyncOperation |> Async.RunSynchronously

//let HttpPost(url:string,param:string,contenttype:string)=
//    let request = HttpWebRequest.CreateHttp url
        
//    request.Method<- "POST"
//    if contenttype.Length>0 then
//        request.ContentType <- contenttype
//    else
//        request.ContentType <- "application/x-www-form-urlencoded"

//    request.Timeout <- 30 * 1000
//    request.AllowAutoRedirect <-false
//    let mutable requestStream:StreamWriter = null
//    let mutable response:WebResponse = null
//    let mutable responseStr = null
//    try
//        requestStream <- new StreamWriter(request.GetRequestStream())
//        requestStream.Write(param);
//        requestStream.Close();

//        response <- request.GetResponse()
//        if (response <> null) then
            
//            let reader = new StreamReader(response.GetResponseStream(), Encoding.UTF8)
//            responseStr <- reader.ReadToEnd()
//            reader.Close();
//            true,responseStr
//        else    
//            false,""
//    with
//    | ex ->
//        false,ex.Message

let mutable maxCycle = 0

// ServicePointManager.SecurityProtocol <- SecurityProtocolType.Ssl3 ||| SecurityProtocolType.Tls ||| SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11
ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls ||| SecurityProtocolType.Tls12 ||| SecurityProtocolType.Tls11

type HttpClient = 
    {

        httpparams: HttpParameters;
        insertions: Dictionary<string, string>;

        debug: bool;

        mutable req_body_text: string;
        mutable req_body_bytes: byte[];
        mutable req_text: string;
        mutable req_bytes: byte[];

        mutable resBytes: List<byte>;
        mutable resBin: byte[];
        mutable resHex: string;

        mutable resBinHeader: byte[];
        mutable resHexHeader: string;

        mutable resBinBody: byte[];
        mutable resHexBody: string;

        mutable bodyStarting: int;

        mutable version: string;
        mutable returncode: string;
        mutable returnHeaders: string;
        mutable charset: string;
        mutable transfer_encoding: string;
        mutable content_encoding: string;
        mutable content_length: string;

        mutable loggero: (string -> unit) option;

        cookies: Dictionary<string, Cookie> 
    }

        member this.addcookie(incoming:string) =
            let text = incoming.Trim()
            let main = text |> Util.Text.regex_match(re_http_cookiemain)
            let index = main.IndexOf("=")
            if(index > 0) then
                let k = main.Substring(0, index)
                let v = main.Substring(index + 1)
                if(this.cookies.ContainsKey(k)) then
                    this.cookies.Remove(k) |> ignore
                this.cookies.Add(k, { k = k; v = v })
            ()

        member this.setResponse() = 
            this.resBin <- this.resBytes.ToArray()
            if(this.debug) then
                this.resHex <- this.resBin |> Util.Bin.hex
                if(this.resBin.Length > 0 && this.bodyStarting > 0) then
                    try
                        let a,b = Array.splitAt this.bodyStarting this.resBin
                        this.resBinHeader <- a
                        this.resHexHeader <- this.resBinHeader |> Util.Bin.hex
                        this.resBinBody <- b
                        this.resHexBody <- this.resBinBody |> Util.Bin.hex
                    with
                    | e -> 
                        ()

        member this.clearCookies() = this.cookies.Clear()

        member this.go(customize)(rawurl:string,postdata,contenttype:string) =
            use cw = new CodeWrapper("Util.HttpClient.go")

            let res = empty_HttpClientRes()

            let mutable duration_dns = new TimeSpan()
            let mutable duration_conn = new TimeSpan()

            let mutable dtBeginWrite = DateTime.UtcNow
            let mutable dtEndWrite = DateTime.UtcNow
            let mutable dtEndRead = DateTime.UtcNow

            //TODO EscapeDataString 导致不能登录。
            let url = rawurl |> System.Uri.UnescapeDataString |> System.Uri.EscapeDataString

            //req_body_text <- Util.Text.httpPostdataQuote(postdata)
            this.req_body_text <- postdata

            let logline(s) = 
                if(this.loggero.IsSome) then 
                    s |> this.loggero.Value
                    "" |> this.loggero.Value

            let parse_url() =
                let mutable residual =
                    HttpUtility.UrlDecode(url)
                if(residual.Contains("://")) then
                    this.httpparams.scheme <- residual |> regex_match(re_url_scheme)
                    residual <- residual.Substring(this.httpparams.scheme.Length + 3)

                match this.httpparams.scheme with
                | "https"
                | "wss" ->
                    this.httpparams.port <- 443
                    this.httpparams.secure <- true
                | _ ->
                    this.httpparams.secure <- false

                if(url.StartsWith("/")) then
                    residual <- url
                else
                    this.httpparams.host <- residual |> regex_match(re_url_host)
                    residual <- residual.Substring(this.httpparams.host.Length)

                    let s_port = residual |> regex_match(re_url_port)
                    if(s_port.Length > 0) then
                        this.httpparams.port <- s_port |> Util.Text.parse_int32
                        residual <- residual.Substring(s_port.Length + 1)

                this.httpparams.netloc <- residual.Trim()
                if(this.httpparams.netloc.Length = 0) then
                    this.httpparams.netloc <- "/"

                if(this.req_body_text.Length > 0) then
                    this.httpparams.method <- "POST"
                else
                    this.httpparams.method <- "GET"

            let build_request_text() =
                if(postdata.Length > 0)then
                    if contenttype.Length>0 then
                        this.httpparams.content_type<- contenttype
                    else
                        this.httpparams.content_type <- "application/x-www-form-urlencoded"
                this.req_body_text <- customize(this.httpparams,this.req_body_text,duration_dns,duration_conn)

                let sb = new List<string>()

                sb.Add(this.httpparams.method + " " + this.httpparams.netloc + " HTTP/" + this.version + CRLF) 
                sb.Add("Accept: " + this.httpparams.accept + CRLF) |> ignore
                sb.Add("Accept-Encoding: " + this.httpparams.accept_encoding + CRLF) 
                sb.Add("Host: " + this.httpparams.host + CRLF) |> ignore
                sb.Add("Content-Type: " + this.httpparams.content_type + CRLF) 
                sb.Add("Content-Length: " + this.req_body_bytes.Length.ToString() + CRLF) 
                if(this.cookies.Count > 0) then
                    let sbb = new List<string>()
                    this.cookies |> Seq.iter(fun item -> item.Value.text() |> sbb.Add)
                    sb.Add("Cookie: " + (sbb |> String.Concat) + CRLF) |> ignore
                sb.Add("User-Agent: " + this.httpparams.user_agent + CRLF) 
                sb.Add("Connection: " + this.httpparams.connection + CRLF) 

                if(this.insertions.Count > 0) then
                    this.insertions |> Seq.iter(fun item ->
                        sb.Add(item.Key + ": " + item.Value + CRLF) )

                if(this.req_body_text.Length>0)then
                    sb.Add(CRLF + this.req_body_text) 
                sb.Add(CRLF) 

                sb |> String.Concat

            let get_tcpclient_stream() =
                
                let ip =
                    let mutable ip = ""

                    let dt_dns = DateTime.UtcNow

                    let host = this.httpparams.host
                    let ips = Dns.GetHostAddresses(host)
                    ips
                    |> Array.iter(fun a ->
                        let s = a.ToString()
                        if(not (s.Contains ":")) then
                            ip <- s)

                    duration_dns <- DateTime.UtcNow.Subtract(dt_dns)

                    ip

                let dt_conn = DateTime.UtcNow

                let tc = new TcpClient(ip, this.httpparams.port)

                let stream =
                    if(this.httpparams.secure)then
                        let callback(obj:Object)(certificate:X509Certificate)(chain:X509Chain)(errors:SslPolicyErrors) = true
                        let sslstream = new SslStream(tc.GetStream(), false, new RemoteCertificateValidationCallback(callback), null)
                        sslstream.AuthenticateAsClient(this.httpparams.host)
                        sslstream :> System.IO.Stream
                    else
                        let s = tc.GetStream()
                        s :> System.IO.Stream

                (ip + ":" + this.httpparams.port.ToString()) |> logline

                duration_conn <- DateTime.UtcNow.Subtract(dt_conn)

                tc,stream

            try
                
                parse_url()

                let tc,stream = get_tcpclient_stream()
                let buffer = Array.zeroCreate Tcp.bufferLength

                this.req_body_bytes <- System.Text.Encoding.UTF8.GetBytes(this.req_body_text)

                this.req_text <- build_request_text()
                this.req_bytes <- System.Text.Encoding.UTF8.GetBytes(this.req_text)

                this.req_bytes |> Util.Bin.hex |> logline

                dtBeginWrite <- DateTime.UtcNow

                stream.Write(this.req_bytes,0,this.req_bytes.Length)
                stream.Flush()

                dtEndWrite <- DateTime.UtcNow

                this.resBytes.Clear()

                this.bodyStarting <-
                    let blanklineo = readstream_until_pattern(stream,buffer)(this.resBytes,BLANKLINE,0)
                    this.resBin <- this.resBytes.ToArray()

                    let header_index = 
                        match blanklineo with
                        | Some(v) ->
                            this.returnHeaders <- System.Text.Encoding.UTF8.GetString(this.resBin,0,v)
                            v
                        | None ->
                            this.returnHeaders <- System.Text.Encoding.UTF8.GetString(this.resBin)
                            this.resBin.Length

                    this.returncode <- this.returnHeaders |> Util.Text.regex_match(re_http_returncode)
                    this.charset <- ""
                    this.transfer_encoding <- ""
                    this.content_encoding <- ""
                    this.content_length <- ""
                    this.returnHeaders.Split(crlf.ToCharArray()) |> Array.iter(fun line ->
                        let setter = line |> Util.Text.regex_match(re_http_setter)
                        if(setter.Length > 0) then
                            let v = line.Substring(setter.Length + 2)
                            match setter.ToLower() with
                            | "set-cookie" -> this.addcookie(v)
                            | "content-type" ->
                                let cs = Util.Text.regex_match(rs_http_charset)(v)
                                if(cs.Length > 0) then
                                    this.charset <- cs.Replace(";","").ToUpper()
                            | "transfer-encoding" -> this.transfer_encoding <- v.ToUpper()
                            | "content-encoding" -> this.content_encoding <- v.ToUpper()
                            | "content-length" -> this.content_length <- v.ToUpper()
                            | _ ->())

                    header_index + BLANKLINE.Length             
                        
                this.setResponse()

                let body =

                    let mutable raw =

                        let buffer = Array.zeroCreate Tcp.bufferLength

                        if(this.transfer_encoding = "CHUNKED") then

                            let mutable index = this.bodyStarting
                            let mutable keep = true
                            let blocks = new List<int*int>()
                            let mutable cycle = 0                                
                            while(keep) do
                                cycle <- cycle + 1
                                if(maxCycle < cycle) then
                                    maxCycle <- cycle

                                let mutable crlf_index,length = -1,0
                                     
                                let o = Util.Bin.readstream_until_pattern(stream,buffer)(this.resBytes,Util.Bin.CRLF,index)
                                this.setResponse()

                                if(cycle > 1000) then
                                    ()

                                if(o.IsSome) then
                                    crlf_index <- o.Value
                                    let mutable str = ""
                                    try 
                                        str <- Encoding.UTF8.GetString(this.resBin,index,crlf_index - index)
                                        length <- Int32.Parse(str,System.Globalization.NumberStyles.HexNumber)
                                    with
                                    | ex ->
                                        let msg = ex.ToString()
                                        let hexStaring = Util.Bin.hex(Array.sub this.resBin index (crlf_index - index))
                                        ()

                                if(crlf_index >= 0 && length > 0 )then
                                    blocks.Add(crlf_index+2,length)
                                    let mutable count_sofar = this.resBytes.Count - crlf_index - Util.Bin.CRLF.Length
                                    while(count_sofar < length)do
                                        let count = stream.Read(buffer,0,buffer.Length)
                                        if(count = 0)then
                                            length<-0
                                            keep<-false
                                        else
                                            count_sofar <- count_sofar + count
                                            this.resBytes.AddRange(buffer |> Array.take(count))
                                            this.setResponse()
                                    index <- crlf_index + Util.Bin.CRLF.Length + length + Util.Bin.CRLF.Length
                                else
                                    keep <- false

                            let body = new ResizeArray<byte>()
                            this.setResponse()
                            blocks.ToArray()
                            |> Array.iter(fun item ->
                                let starting,length = item
                                if(starting+length<this.resBytes.Count)then
                                    Array.sub(this.resBin)(starting)(length)
                                else
                                    Array.sub(this.resBin)(starting)(this.resBytes.Count-starting)
                                |> body.AddRange)

                            body.ToArray()


                        else if(this.content_length.Length > 0) then

                            let length = System.Convert.ToInt32(this.content_length)

                            let mutable keep = true
                            if(this.resBytes.Count >= this.bodyStarting + length) then
                                keep <- false

                            let mutable cycle = 0                                
                            while(keep) do
                                cycle <- cycle + 1
                                if(maxCycle < cycle) then
                                    maxCycle <- cycle

                                if(cycle > 1000) then
                                    ()

                                let count = stream.Read(buffer,0,buffer.Length)
                                if(count = 0)then
                                    keep <-false
                                else
                                    buffer |> Array.take(count)|> this.resBytes.AddRange
                                if(this.resBytes.Count >= this.bodyStarting + length) then
                                    keep <- false

                            if(this.resBytes.Count >= this.bodyStarting + length) then
                                Array.sub(this.resBytes.ToArray())(this.bodyStarting)(length)
                            else if(this.resBytes.Count > this.bodyStarting) then
                                Array.sub(this.resBytes.ToArray())(this.bodyStarting)(this.resBytes.Count - this.bodyStarting)
                            else
                                [| |]

                        else
                            this.setResponse()
                            let count = this.resBin.Length - this.bodyStarting
                            if(this.resBin.Length > 0 && count > 0)then
                                Array.sub this.resBin this.bodyStarting count
                            else
                                [| |]

                    dtEndRead <- DateTime.UtcNow

                    this.resHex |> logline
                    ("BodyStarting = " + this.bodyStarting.ToString()) |> logline
                    ("Raw = "+raw.Length.ToString()) |> logline

                    if(this.content_length.Length>0)then
                        let length = this.content_length |> Util.Text.parse_int32
                        if(length<raw.Length)then
                            raw <- raw |> Array.take(length)
                    match this.content_encoding with
                    | "GZIP" ->
                        raw |> gzip__bytes
                    //| "DEFLATE" -> Tools.Compression.CompressionUtilities.GZipCompress(msgbs)
                    | _ ->
                        raw

                let encoding =
                    if(this.charset = "") then
                        System.Text.Encoding.UTF8
                    else
                        System.Text.Encoding.GetEncoding(this.charset)
                let final = encoding.GetString(body)

                final |> logline

                res.bytes <- body
                res.msg <- ""
                res.html <- final
                res.success <- true

                res.duration_request <- dtEndWrite.Subtract(dtBeginWrite)
                res.duration_response <- dtEndRead.Subtract(dtEndWrite)

            with
            | ex -> 
                res.bytes <- [||]
                res.html <- "" 
                res.msg <- ex.ToString()
                res.success <- false

            res.duration_dns <- duration_dns
            res.duration_conn <- duration_conn
            res.rq_url <- url
            res.rq_body <- postdata
            res.elapse <- DateTime.UtcNow.Subtract(res.since)

            res.returnCode <- this.returncode
            res.returnHeaders <-
                this.returnHeaders
                |> regex_matches(str__regex(@".*?" + regexCRLF))
                |> Array.map(fun line -> line.Substring(0,line.Length - 2))

            res

        member this.post(url:string, postdata:string) = this.go(fun (p,body,dns,conn) -> body)(url,postdata,"")
        member this.post(url: string, postdata: byte[], contenttype: string) =
            let ex, result = HttpPost url (Encoding.UTF8.GetString(postdata)) contenttype
            result

        //member this.post(url:string,postdata:byte[],contenttype) =
        //    let mutable result =null
        //    try
        //        let req = HttpWebRequest.Create(url)
        //        req.Method <- "POST";
        //        req.ContentType <-contenttype
        //        req.ContentLength <- (int64)postdata.Length;
        //        if(this.insertions.Count > 0) then
        //            this.insertions |> Seq.iter(fun item ->
        //                req.Headers.Add( item.Key, item.Value))
        //        let reqStream = req.GetRequestStream()
        //        reqStream.Write(postdata, 0, postdata.Length);

        //        let res =  new StreamReader(req.GetResponse().GetResponseStream(), Encoding.UTF8)
        //        result <- res.ReadToEnd()
        //        reqStream.Close()
        //        req.GetResponse().Close()
        //        result
        //    with
        //    | ex -> 
        //        result
        member this.post(url:string, postdata:string,contenttype:string) = this.go(fun (p,body,dns,conn) -> body)(url,postdata,contenttype)

        member this.get(url) = 
            lock this (fun () ->
                let mutable res = this.go(fun (p,body,dns,conn) -> body)(url,"","")

                match res.returnCode with
                | "301" ->
                
                    let mutable loc = ""
                    res.returnHeaders
                    |> Array.iter(fun line ->
                        if(line.StartsWith("Location: ")) then
                            loc <- line.Substring("Location: ".Length))

                    if(loc.Length > 0) then
                        res <- this.go(fun (p,body,dns,conn) -> body)(loc,"","")

                | _ -> ()

                res)

let empty__HttpClient() = 
    {

        httpparams = default_httpparams();
        insertions = new Dictionary<string, string>();

        debug = false;

        req_body_text = "";
        req_body_bytes = [||];
        req_text = "";
        req_bytes = [||];

        resBytes = new List<byte>();
        resBin = [||];
        resHex = "";

        resBinHeader = [||];
        resHexHeader = "";

        resBinBody = [||];
        resHexBody = "";

        bodyStarting = 0;

        version ="1.1";
        returncode = "";
        returnHeaders = "";
        charset = "";
        transfer_encoding = "";
        content_encoding = "";
        content_length = "";

        loggero = None

        cookies = new Dictionary<string, Cookie>() }



let post(hc:HttpClient,url:string)(postdata:string) = hc.post(url,postdata)

let httpGeta (headers:Dictionary<string,string> option) (url:string) = 
    async{
        try
            use client = new System.Net.Http.HttpClient()
            client.Timeout <- new TimeSpan(0, 0, 20)
            let cts = new System.Threading.CancellationToken()
            if headers.IsSome && headers.Value.Count > 0 then
                for kv in headers.Value do
                    client.DefaultRequestHeaders.Add(kv.Key,kv.Value)
            let! response = client.GetAsync(url,cts) |> Async.AwaitTask
            // response.EnsureSuccessStatusCode () |> ignore
            let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            return (url,response.StatusCode.ToString()), content
        with
        | :? System.Threading.Tasks.TaskCanceledException as ex ->
            return (url,ex.ToString()),""
        | e -> 
            return (url,e.ToString()),""
    }

let httpGet (headers:Dictionary<string,string> option) (url:string) = url |> httpGeta headers |> Async.RunSynchronously
let httpGetSleep (interval:int) (headers:Dictionary<string,string> option) (url:string) =
    let res = httpGet headers url
    System.Threading.Thread.Sleep interval
    res

let httpPosta(url:string,postdata) = 

    let httpcontent = new System.Net.Http.StringContent(postdata,Encoding.UTF8,"application/json")
    async{
        try
            use client = new System.Net.Http.HttpClient()
            client.Timeout <- new TimeSpan(0, 0, 20)
            // printfn "--> Post: timeout= %A url=%A postdata=%A " client.Timeout url postdata
            let! response = client.PostAsync(url,httpcontent) |> Async.AwaitTask
            // printfn "<-- Post Reps: statusCode=%A ReasonPhrase=%A" response.StatusCode response.ReasonPhrase
            // response.EnsureSuccessStatusCode () |> ignore
            let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            return response.StatusCode.ToString(), content
        with
        | _ as ex -> 
            let current_time = DateTime.UtcNow.ToString("hh:mm:ss")
            let msg = $"[ {current_time} ]" + " Connection Refused. \r" 
            verror <| sprintf "%s" msg
            return msg,""
    }

let httpPost(url:string,postdata) = (url,postdata) |> httpPosta |> Async.RunSynchronously

let httpPostWithHeadera(url:string,postdata,header:Dictionary<string, string>) = 

    let httpcontent = new System.Net.Http.StringContent(postdata,Encoding.UTF8,"application/json")
    async{
        try
            use client = new System.Net.Http.HttpClient()
            if (header <> null && header.Count > 0) then
                for KeyValue(k, v) in header do
                    client.DefaultRequestHeaders.TryAddWithoutValidation(k, v) |> ignore
            let! response = client.PostAsync(url,httpcontent) |> Async.AwaitTask
            let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            return response.StatusCode.ToString(), content
        with
        | e -> 
            let msg = url + " " + e.ToString()
            let headerinfo =
                header.Keys
                |> Seq.toArray
                |> Array.map (
                    fun i -> i + ":" + header.[i]
                )
                |> String.concat "|"
            return (msg + "|" + headerinfo),""
    }
