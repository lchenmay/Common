module Util.HttpServer

open System
open System.Collections.Generic
open System.Web
open System.Net
open System.Net.Security
open System.Net.Sockets
open System.Security.Cryptography.X509Certificates
open System.Security.Authentication
open System.Text
open System.Text.RegularExpressions

open Util.Text
open Util.Bin
open Util.Http
open Util.TcpServer


let crlf = Util.Text.crlf
let crlfcrlf = Util.Text.crlfcrlf
let crlfcrlfBin = Encoding.ASCII.GetBytes crlfcrlf

let reg_req_topline = new Regex(@"\S+", Util.Text.regex_options)
let reg_req_header = new Regex(@"[\w-]+:\s+", Util.Text.regex_options)
let reg_req_path = new Regex(@"[^/]+", Util.Text.regex_options)
let reg_req_query = new Regex(@"[^\x26]+", Util.Text.regex_options)


let mutable logging = None
let log line = if logging.IsSome then logging.Value("HTTP>" + line)

let headers_body (bin:byte[]) =

    match
        [| crlfcrlfBin.Length .. bin.Length - 1 |]
        |> Array.tryFind(fun i -> 
            ([| 0 .. crlfcrlfBin.Length - 1|]
            |> Array.filter(fun j -> crlfcrlfBin[j] = bin[i + j])
            |> Array.length) = crlfcrlfBin.Length) with
    | Some i -> 
        let head = 
            Array.sub bin 0 i
            |> Encoding.UTF8.GetString 
            |> HttpUtility.UrlDecode    
        let body = 
            Array.sub bin (i + crlfcrlfBin.Length) (bin.Length - i - crlfcrlfBin.Length)
        head,body
    | None -> 
        let head = 
            bin
            |> Encoding.UTF8.GetString 
            |> HttpUtility.UrlDecode    
        head,[||]

let topline_dict(headers:string) =
    let mutable topline = None
    let dict = new Dictionary<string, string>()
    headers.Split(crlf.ToCharArray())
    |> Seq.iter(fun line ->
        if line.Length > 0 then
            if topline.IsNone then
                topline <- Some line
            else
                let k = line |> Util.Text.regex_match(reg_req_header)
                let index = k.IndexOf ":"
                if index > 0 then
                    let key = k.Substring(0, index)
                    if key.Length > 0 && dict.ContainsKey key = false then
                        dict.Add(key, (line.Substring (key.Length + 1)).Trim())
    )
    topline, dict

let method_path topline =
    let a = topline |> Util.Text.regex_matches reg_req_topline
    if a.Length = 3 then
        let path = a[1] |> HttpUtility.UrlDecode
        let subpaths =
            let arr = path.TrimStart('/').Split('?')
            let strs = arr[0].Split('/')
            match arr.Length with
            | 0 -> [||]
            | 1 -> strs
            | _ -> [|0 .. strs.Length - 1|] |> Array.map(fun i -> 
                if i = strs.Length - 1 then
                    strs[i] + "?" + arr[1]
                else
                    strs[i])            
        a[0],path,subpaths
    else
        "","",[||]

let query(path:string[]) =
    let dict = new Dictionary<string, string>()
    if path.Length > 0 then
        let lastline = path.[path.Length - 1]
        let index  = lastline.IndexOf "?"
        if index >= 0 then
            path.[path.Length - 1] <- lastline.Substring(0, index)
            lastline.Substring(index + 1)
            |> Util.Text.regex_matches reg_req_query
            |> Array.iter(fun s ->
                let k, v =
                    let index = s.IndexOf "="
                    if index > 0 then
                        s.Substring(0, index), s.Substring(index + 1)
                    else
                        s, ""
                if dict.ContainsKey k = false then
                    dict.Add(k, v))
    dict

let bsx__httprequest(ip,port,acceptedat,receivedat,sendstartedat,sendendedat) bs =
    let headers, body = headers_body bs
    let topline, dict = topline_dict headers
    match topline with
    | Some tl ->
        let method,pathline,path = method_path tl
        let query = query path

        log topline.Value
        let file =
            if pathline.IndexOf "?" >= 0 then
                pathline.Substring(0, (pathline.IndexOf "?"))
            else pathline
        Some {
                bin = bs;
                str = 
                    bs 
                    |> Encoding.UTF8.GetString 
                    |> HttpUtility.UrlDecode
                domainname = dict.["Host"];
                requestLine = topline.Value;
                method = method;
                pathline = pathline;
                path = path;
                headers = dict;
                query = query;
                body = body;
                acceptedat = acceptedat;
                receivedat = receivedat;
                sendstartedat = sendstartedat;
                sendendedat = sendendedat;
                ip = ip;
                port = port
                file = file},(headers,body)

    | None ->
        log "Invalid HTTP request"
        None,(headers,body)

let bs__httpRequest (ip,bs) = 
    let now = DateTime.UtcNow
    bsx__httprequest(ip,0,now,now,now,now) bs

let header(hres, bodybytes:byte[]) =
    let sb = new List<string>()
    sb.Add("HTTP/1.1 200 OK" + crlf) |> ignore

    if hres.headers.ContainsKey "Content-Type" = false then
        hres.headers.Add("Content-Type", "text/html;charset=UTF-8")
    hres.headers.Add("Content-Length", bodybytes.Length.ToString())

    if hres.gzip then
        hres.headers.Add("Content-Encoding", "gzip")
        hres.headers.Add("vary", "Accept-Encoding")

    hres.headers.Add("Access-Control-Allow-Methods", "*")
    hres.headers.Add("Access-Control-Max-Age", "3600")
    hres.headers.Add("Access-Control-Allow-Credentials", "true")

    hres.headers |> Seq.iter(fun kvp -> sb.Add(kvp.Key + ": " + kvp.Value + crlf))
    sb.Add crlf
    sb 
    |> String.Concat
    |> Encoding.UTF8.GetBytes

let httpresponse__bs(rq:HttpRequest,rs:HttpResponse) =

    let mutable bodybytes = 
        rs.body 
        |> Encoding.UTF8.GetBytes

    rs.gzip <- 
        match 
            rq.headers.Keys
            |> Seq.tryFind(fun k -> k.ToLower() = "accept-encoding") with
        | Some k -> rq.headers.[k].ToLower().Contains "gzip"
        | None -> false

    if rs.gzip then
        bodybytes <-
            bodybytes
            |> Util.Bin.bs__gzip

    let headerbytes = header(rs, bodybytes)

    let bb = new BytesBuilder()
    headerbytes |> bb.append
    bodybytes |> bb.append
    bb.bytes()

let run handler extloggero (bs,ip,port,acceptedat,receivedat,sendstartedat,sendendedat) =
    let hro,(a,b) = 
        bs
        |> bsx__httprequest(ip,port,acceptedat,receivedat,sendstartedat,sendendedat)
    match hro with
    | Some httprequest ->
        let httpresponse = handler extloggero httprequest
        httpresponse__bs(httprequest,httpresponse)
    | None -> 
        Util.Concurrent.logsome extloggero "L133:Util/HttpServer.fs/run()/HttpRequest parse error"
        [||]

let standardResponseHeader = 
    [|  "HTTP/1.1 200 OK"
        "Access-Control-Allow-Origin: *" 
        "Access-Control-Allow-Methods: GET, POST, OPTIONS"
        "Access-Control-Allow-Headers: *"
        "Access-Control-Allow-Credentials: true" |]
    |> String.concat crlf
    |> System.Text.Encoding.UTF8.GetBytes

let bin__StandardResponse mime body = 
    [|  standardResponseHeader 
        crlf + "Content-Type: " + mime + crlfcrlf |> System.Text.Encoding.UTF8.GetBytes        
        body |]
    |> Array.concat

let str__StandardResponse mime (str:string) = 
    str 
    |> Encoding.UTF8.GetBytes 
    |> bin__StandardResponse mime


let incomingProcess (ip,bin:byte[]) = 
    
    let txt = bin |> Encoding.ASCII.GetString

    if  txt.StartsWith "GET" 
        || txt.StartsWith "POST" 
        || txt.StartsWith "OPTIONS" then

        let key = (regex_match rxSecWebSocketKey txt).Trim()

        if key.Length > 0 then

            let headers = 
                let headers = new Dictionary<string,string>()
                headers["Connection"] <- "Upgrade"
                headers["Upgrade"] <- "websocket"
                headers["Sec-WebSocket-Accept"] <- 
                    key + webSocketUUID
                    |> Encoding.UTF8.GetBytes
                    |> Crypto.bin__sha1bin
                    |> Convert.ToBase64String
                //headers["Sec-WebSocket-Protocol"] <- "binary"
                headers__txt headers

            [|  "HTTP/1.1 101 Switching Protocols"
                headers
                crlf    |]
            |> String.concat crlf
            |> Encoding.ASCII.GetBytes
            |> HttpRequestWithWS.WebSocketUpgrade
        else
            bs__httpRequest (ip,bin)
            |> HttpRequestWithWS.Echo
    else
        HttpRequestWithWS.InvalidRequest
