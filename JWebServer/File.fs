module JWebServer.File

open System
open System.IO

open Util.ADT
open Util.Collection
open Util.CollectionModDict
open Util.Http
open Util.HttpServer

let mutable fileErLoggero: (string * exn -> unit) option = None

let cacheFile = create_mdIntString<DateTime * (byte[])> 8

let checkoutFile mime f = 
    try
        if cacheFile.ContainsKey f then
            if DateTime.UtcNow.Subtract(fst cacheFile[f]).TotalMinutes > 1.0 then
                cacheFile.remove f

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

let fileService root defaultHtml req = 

    if req.pathline = "/" then
        Path.Combine(root, defaultHtml)
        |> checkoutFile "text/html"
    elif req.path.Length > 0 then

        let file = 
            [|  [| root |]
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
    else None
