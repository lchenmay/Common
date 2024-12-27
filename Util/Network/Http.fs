module Util.Http 

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Net
open System.Net.Sockets
open System.Text
open System.Text.RegularExpressions

open Util.Text
open Util.Bin
open Util.Tcp


open Collection

let re_url_scheme = new Regex(@".*?(?=://)", Util.Text.regex_options)
let re_url_host = new Regex(@"[^:/]+", Util.Text.regex_options)
let re_url_port = new Regex(@"(?<=:)\d+", Util.Text.regex_options)
let re_http_returncode = new Regex(@"(?<=HTTP/\S+\s+)\d+", Util.Text.regex_options)
let re_http_setter = new Regex(@".+?(?=:\s)", Util.Text.regex_options)
let re_http_cookiemain = new Regex(@".+?(?=;)", Util.Text.regex_options)

let rs_http_charset = new Regex(@"(?<=charset=)\S+", Util.Text.regex_options)

let url__domainame (url:string) = 

    let mutable domainame = 
        let i = url.IndexOf "://"
        if i > 0 then
            url.Substring (i + 3)            
        else
            url

    domainame <-
        let i = domainame.IndexOf "/"
        if i > 0 then
            domainame.Substring (0,i)            
        else
            domainame

    domainame <-
        let i = domainame.IndexOf ":"
        if i > 0 then
            domainame.Substring (0,i)            
        else
            domainame

    domainame <-
        domainame.ToCharArray()
        |> Array.rev
        |> String

    domainame <-
        domainame
        |> regex_match (str__regex "\w+\.\w+")

    domainame <-
        domainame.ToCharArray()
        |> Array.rev
        |> String

    domainame.Trim().ToLower()


//Set-Cookie: session-id-time=-; path=/; domain=.www.amazon.cn; expires=Tue, 26-Dec-2006 11:41:37 GMT
type Cookie =
    { k:string; v:string }
    member this.text() =
        this.k + "=" + this.v + "; "

// expires=Tue, 26-Dec-2006 11:41:37 GMT
let create_cookie_expiry(expiry:DateTime) =
    [|
        expiry.DayOfWeek.ToString().Substring(0,3);
        ",";
        expiry.Day.ToString("00");
        "-";
        expiry.Month |> Util.Text.month__xxx;
        "-";
        expiry.Year.ToString("00");
        " ";
        expiry.Hour.ToString("00");
        ":"
        expiry.Minute.ToString("00");
        ":"
        expiry.Second.ToString("00");
        " GMT" |]
    |> String.concat("")

let regex_ip = new Regex(@"[\w\.\x3a]+", Util.Text.regex_options)

let headers__txt (headers:Dictionary<string,string>) = 
    headers.Keys
    |> Seq.toArray
    |> Array.map(fun k -> k + ": " + headers[k])
    |> String.concat crlf

type HttpRequest = {   
bin:byte[]
str:string
domainname:string
requestLine:string
method:string
mutable pathline:string
path:string[]
headers:Dictionary<string, string>
query:Dictionary<string, string>
body:byte[]
acceptedat:DateTime
receivedat:DateTime
sendstartedat:DateTime
sendendedat:DateTime
ip:string
port:int
file: string }

let remote_ip req = 
    let init_proxy_ip = 
        let k = "X-Forwarded-For"
        if req.headers.ContainsKey k then
            let v = req.headers[k]
            v |> Util.Text.regex_match regex_ip
        else
            ""
    if init_proxy_ip.Length > 0 then
        init_proxy_ip
    else 
        req.ip

let empty__HttpRequest() = 
    {   
        bin = [||];
        str = "";
        domainname = "";
        requestLine = "";
        method = "";
        pathline = "";
        path = [||];
        headers = new Dictionary<string, string>();
        query = new Dictionary<string, string>();
        body = [| |];
        acceptedat = DateTime.UtcNow;
        receivedat = DateTime.UtcNow;
        sendstartedat = DateTime.UtcNow;
        sendendedat = DateTime.UtcNow;
        ip = "";
        port = 0;
        file = ""}
            

type HttpResponse =
    { 
        mutable code:string; 
        headers:Dictionary<string, string>; 
        mutable body:string;
        mutable gzip:bool }

let empty__HttpResponse() =
    { 
        code = "200"; 
        headers = new Dictionary<string, string>(); 
        body = "";
        gzip = true }


type HttpRequestWithWS = 
| Echo of (HttpRequest option) * (string * byte[])
| WebSocketUpgrade of byte[]
| InvalidRequest

let chunkedreading(stream:Stream,buffer,bin:ResizeArray<byte>,body_starting:int) =
    let mutable index = body_starting
    let mutable keep = true
    let blocks = new List<int*int>()
    while(keep) do
        let mutable crlf_index,length = 
            let starting = index
            let crlf_index = Util.Bin.readstream_until_pattern(stream,buffer)(bin,CRLF,starting)

            if(crlf_index.IsSome)then
                let mutable str =
                    Encoding.UTF8.GetString(
                        bin.ToArray(),
                        starting,
                        crlf_index.Value - starting)

                let mutable length = 0
                try 
                    length <- Int32.Parse(str,System.Globalization.NumberStyles.HexNumber)
                with
                | ex ->
                    let msg = ex.ToString()
                    let bs = bin.ToArray()
                    let hex = Util.Bin.hex(bin.ToArray())
                    let hexStaring = Util.Bin.hex(Array.sub bs starting (crlf_index.Value - starting))
                    ()

                crlf_index,length
            else
                crlf_index,0


        if(crlf_index.IsNone)then
            keep <- false
        else if(length=0)then
            keep <- false
        else
            blocks.Add(crlf_index.Value+2,length)
            let mutable count_sofar = bin.Count - crlf_index.Value - CRLF.Length
            while(count_sofar < length)do
                let count = stream.Read(buffer,0,buffer.Length)
                if(count=0)then
                    length<-0
                    keep<-false
                else
                    count_sofar <- count_sofar + count
                    bin.AddRange(buffer |> Array.take(count))
            index <- crlf_index.Value + CRLF.Length + length + CRLF.Length

    let body = new ResizeArray<byte>()
    let bytes = bin.ToArray()
    blocks.ToArray()
    |> Array.iter(fun item ->
        let starting,length = item
        if(starting+length<bin.Count)then
            Array.sub(bytes)(starting)(length)
        else
            Array.sub(bytes)(starting)(bin.Count-starting)
        |> body.AddRange)

    body.ToArray()

let getQueryString(url:string) (prefix:string) =
    if url.IndexOf prefix > 0 then
        let pos = url.IndexOf(prefix) + prefix.Length
        let r = url.Substring pos
        if r.IndexOf("&") > 0 then
            r.Substring(0,r.IndexOf "&")
        else
            r
    else ""

let rec clearEndSlash (url:string) =
    if url.EndsWith "/" && url.Length-1 >= 0 then
        url.Substring(0,url.Length-1)
        |> clearEndSlash
    else
        url

let rxSecWebSocketKey = new Regex(@"(?<=Sec-WebSocket-Key: )\S+", regex_options)

let binSecWebSocketKey = 
    [|  "HTTP/1.1 101 Switching Protocols" + crlf
        "Connection: Upgrade" + crlf
        "Upgrade: websocket" + crlf
        "Sec-WebSocket-Accept: " |]
    |> System.String.Concat
    |> Encoding.UTF8.GetBytes

let httpUpgradeWebSocket str = 
    let key = regex_match rxSecWebSocketKey str
    if key.Length > 0 then
        let s = 
            key.Trim() + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
            |> Encoding.UTF8.GetBytes
            |> System.Security.Cryptography.SHA1.Create().ComputeHash
            |> Convert.ToBase64String

        [|  binSecWebSocketKey
            s + crlf + crlf |> Encoding.UTF8.GetBytes |]
        |> Array.concat
    else
        [||]

let webSocketUUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let checkHttpUpgrade (bin:byte[]) = 
    if bin.Length > 10 then
        let starting = Encoding.ASCII.GetString(bin, 0, 10)
        if starting.StartsWith "GET" || starting.StartsWith "POST" || starting.StartsWith("OPTIONS") then
            let upgrade =
                let key = 
                    bin 
                    |> Encoding.UTF8.GetString
                    |> regex_match rxSecWebSocketKey

                if key.Length > 0 then
                    let s = 
                        key.Trim() + webSocketUUID
                        |> Encoding.UTF8.GetBytes
                        |> System.Security.Cryptography.SHA1.Create().ComputeHash
                        |> Convert.ToBase64String

                    [|  binSecWebSocketKey
                        s + crlf + crlf |> Encoding.UTF8.GetBytes |]
                    |> Array.concat
                else
                    [||]
            upgrade, upgrade.Length > 0
        else
            [||], true
    else
        [||], true

