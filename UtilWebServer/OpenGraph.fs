module UtilWebServer.OpenGraph

open System
open System.Text
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading

open Util.Runtime
open Util.Text
open Util.Json
open Util.Db
open Util.DbTx
open Util.Orm
open Util.Http
open Util.HttpClient
open Util.Zmq

open UtilWebServer.DbLogger

let r1 = string__regex "<meta .*?>"

let parse html = 

    let mutable title = ""
    let mutable desc = ""
    let mutable image = ""
    let mutable url = ""

    let items = 
        html
        |> find ("<head","</head>")
        |> regex_matches r1
        |> Array.map(fun line -> 
            let content = find("content=\"","\"") line
            let property = find("property=\"","\"") line
            property,content)

    items
    |> Array.iter(fun (property,content) -> 
        if title.Length = 0 then
            if property = "og:title" then
                title <- content

        if desc.Length = 0 then
            if property = "og:description" then
                desc <- content

        if image.Length = 0 then
            if property = "og:image" then
                image <- content

        if url.Length = 0 then
            if property = "og:url" then
                url <- content)

    if image.StartsWith "http" = false then

        let hc = default_httpparams()
        hc.forward false url
        hc.forward true image

        image <- hc.url()

    title,desc,image
   
