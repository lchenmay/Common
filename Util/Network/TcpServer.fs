module Util.TcpServer

open System
open System.Net
open System.Net.Security
open System.Net.Sockets
open System.Security.Cryptography.X509Certificates
open System.Security.Authentication

open Util.Bin
open Util.Tcp



let mutable logging = None
let log(line) = if(logging.IsSome) then logging.Value("TCP>" + line)

let prepareserver(port:int, certfile) =
    let cert =
        if(System.IO.File.Exists(certfile)) then
            Some(X509Certificate.CreateFromCertFile(certfile))
        else
            None

    new TcpListener(IPAddress.Any, port), cert

let validate(cert)(client:TcpClient) =
    log(client.Client.RemoteEndPoint.ToString())
    let s = client.GetStream()
    let stream = new SslStream(s, false)
    try
        stream.AuthenticateAsServer(cert, false, SslProtocols.Tls12, true)
        Some(stream)
    with
    | ex ->
        log(ex.ToString())
        stream.Close()
        client.Close()
        None

let incoming(listener:TcpListener, cert:X509Certificate option) =
    let client = listener.AcceptTcpClient()
    log("Accepted: " + client.Client.RemoteEndPoint.ToString())
    if(cert.IsSome) then
        let res = client |> validate(cert.Value)
        if(res.IsSome) then
            log("Cert OK: " + client.Client.RemoteEndPoint.ToString())
            Some(client, res.Value:>System.IO.Stream)
        else
            log("Cert Failed: " + client.Client.RemoteEndPoint.ToString())
            None
    else
        Some(client, client.GetStream():>System.IO.Stream)

let read(stream:System.IO.Stream,buffer) =

    let bb = new BytesBuilder()

    let mutable keep = true
    while(keep) do

        let count = stream.Read(buffer, 0, buffer.Length)

        bb.append(buffer |> Seq.toArray,count)
        if(count < buffer.Length) then
            keep <- false

    bb.bytes()

let process_cycle(listener, cert, handler) =
    match incoming(listener, cert) with
    | Some(client, s) ->
        let buffer = Array.zeroCreate Tcp.bufferLength
        let rqbytes = read(s,buffer)
        log("Read:" + rqbytes.Length.ToString())
        let rsbytes = handler(rqbytes, client.Client.RemoteEndPoint.ToString())

        s.Write(rsbytes, 0, rsbytes.Length)
        s.Flush()
        s.Close()

        log("Write:" + rsbytes.Length.ToString())

        client.Close()
    | None -> ()

let run(port, certfile, handler) =

    let listener, cert = prepareserver(port, certfile)
    listener.Start()
    log("TCP server started at [" + port.ToString() + "]")
    [0..8] |> Seq.iter(fun i -> log(""))

    while(true) do
        try
            process_cycle(listener, cert, handler)
        with
        | ex -> Console.WriteLine(ex.ToString())