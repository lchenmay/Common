module UtilWebServer.PageLog

open System
open System.Text
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Cat
open Util.Runtime
open Util.Text
open Util.Json
open Util.Db
open Util.DbTx
open Util.Orm
open Util.Http
open Util.HttpServer
open Util.Zmq

(*

66.249.74.14
GET /m/54864678 HTTP/1.1
Host: fleaacademy.com
User-Agent: Mozilla/5.0 (Linux; Android 6.0.1; Nexus 5X Build/MMB29P) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.6778.139 Mobile Safari/537.36 (compatible; Googlebot/2.1; http://www.google.com/bot.html)
Accept: text/html,application/xhtml xml,application/signed-exchange;v=b3,application/xml;q=0.9,*/*;q=0.8
Accept-Encoding: gzip, br
Amp-Cache-Transform: google;v="1..8"
Cdn-Loop: cloudflare; loops=1
Cf-Connecting-Ip: 66.249.74.14
Cf-Ipcountry: US
Cf-Ray: 8fc74b19ee1e3ab6-DFW
Cf-Visitor: {"scheme":"https"}
Cf-Warp-Tag-Id: c108d7b5-dbba-4b10-990e-346e99ee3a21
Connection: keep-alive
From: googlebot(at)googlebot.com
X-Forwarded-For: 66.249.74.14
X-Forwarded-Proto: https

*)

let req__fromo (req:string) = 
    req.Split crlf
    |> Array.tryFind(fun i -> i.StartsWith "From: ")
    
let req__json (ip,timestamp,req:string) = 
    let lines = req.Split crlf
    
    let pathline = 
        match
            lines 
            |> Array.tryFind(fun i -> i.StartsWith "GET ") with
        | Some s -> s
        | None -> ""

    let src = 
        match
            lines 
            |> Array.tryFind(fun i -> i.StartsWith "From: ") with
        | Some s -> s
        | None -> ""

    [|  ("time",(Util.Text.timestamp__userfriendlyPast timestamp) |> Json.Str)
        ("ip",ip |> Json.Str)
        ("pathline",pathline |> Json.Str)
        ("from",src |> Json.Str) |]
    |> Json.Braket


