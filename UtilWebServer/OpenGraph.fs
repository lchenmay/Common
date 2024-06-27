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

let parse host html = 

    let mutable title = ""
    let mutable desc = ""
    let mutable image = ""
    let mutable url = ""

    html
    |> find ("<head","</head>")
    |> regex_matches r1
    |> Array.iter(fun line -> 
        if title.Length = 0 then
            if line.Contains """ property="og:title" """ then
                title <- line |> find("content=\"","\"")

        if desc.Length = 0 then
            if line.Contains """ name="description" """ then
                desc <- line |> find("content=\"","\"")
            elif line.Contains """ property="og:description" """ then
                desc <- line |> find("content=\"","\"")

        if image.Length = 0 then
            if line.Contains """ property="og:image" """ then
                image <- line |> find("content=\"","\"")

        if image.Length = 0 then
            if line.Contains """ property="og:url" """ then
                image <- line |> find("content=\"","\""))

    if image.StartsWith "http" = false then
        image <- "https://" + host + "/" + image

    title,desc,image
   
