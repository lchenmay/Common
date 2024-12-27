module UtilWebServer.Server.File

open System
open System.IO

open Util.ADT
open Util.Collection
open Util.CollectionModDict
open Util.Http
open Util.HttpServer
open Util.Text

let mutable fileErLoggero: (string * exn -> unit) option = None

let cacheFile = createModDictStr<DateTime * (byte[])> 8

let checkoutFile mime f = 
    try
        if cacheFile.ContainsKey f then
            if DateTime.UtcNow.Subtract(fst cacheFile[f]).TotalMinutes > 1.0 then
                cacheFile.Remove f

        if cacheFile.ContainsKey f = false then
            cacheFile[f] <- DateTime.UtcNow,File.ReadAllBytes f

        cacheFile[f]
        |> snd
        |> bin__StandardResponse mime
        |> Some
    with
    | ex -> 
        handlero fileErLoggero (f,ex)
        None

let fileService fsDir vueDeployDir req = 

    let mutable file = 
        [|  [| fsDir |]
            req.path |]
        |> Array.concat
        |> Path.Combine

    if File.Exists file = false then
        file <- 
            [|  [| vueDeployDir |]
                req.path |]
            |> Array.concat
            |> Path.Combine

    if File.Exists file then
        let filename = req.path[req.path.Length - 1]

        let fileext = 
            req.path[req.path.Length - 1]
            |> Path.GetExtension

        let mime = 
            match fileext.ToLower() with
            | ".jpg" as ext -> 
                "image/jpeg"
            | ".svg" | ".svgz" as ext -> 
                "image/svg+xml"
            | ".jpeg" | ".png" | ".gif" | ".ico" | ".webp" as ext -> 
                "image/" + (ext.Substring 1)
            | ".css" | ".html" | ".javascript" | ".txt" | ".xml" as ext ->
                "text/" + (ext.Substring 1)
            | ".js" ->
                "text/javascript; charset=utf-8"
            | ".mp4" ->
                "video/mp4"
            | _ -> ""

        file
        |> checkoutFile mime

    else None

let r1 = str__regex "(?<=-----------------------------)\d+"

let fileUploadReceive (req:HttpRequest) = 

(*

-----------------------------13390593892577175696221916023
Content-Disposition: form-data; name="file"; filename="aaa.txt"
Content-Type: text/plain

abc
-----------------------------13390593892577175696221916023--

*)

    //let token = req.body |> regex_match r1
    //let pattern = "-----------------------------" + token
    //let i1 = req.body.IndexOf pattern
    //let i2 = req.body.IndexOf crlfcrlf
    //let i3 = req.body.LastIndexOf pattern

    let body = req.body

    System.IO.File.WriteAllBytes("a.jpg",body)

    [| |]
