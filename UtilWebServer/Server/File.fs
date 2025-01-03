﻿module UtilWebServer.Server.File

open System
open System.IO
open System.Threading

open Util.Cat
open Util.ADT
open Util.Db
open Util.DbTx
open Util.Collection
open Util.CollectionModDict
open Util.Orm
open Util.Http
open Util.HttpServer
open Util.Text

open UtilWebServer.Common
open UtilWebServer.Db

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

let ext__mime (ext:string) = 
    match ext.ToLower() with
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
    | ".mp3" ->
        "audio/mpeg"
    | ".mp4" ->
        "video/mp4"
    | _ -> ""



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

        let mime = ext__mime fileext

        file
        |> checkoutFile mime

    else None

let r1 = str__regex "(?<=-----------------------------)\d+"


let buildfilename fsDir id (suffix:string) =
    let f = 
        if suffix.Length > 0 then
            id.ToString() + "." + suffix
        else
            id.ToString()
    Path.Combine(fsDir,"managed",f)

let file__bin fsDir fid fsuffix = 
    let filename = buildfilename fsDir fid fsuffix
    if File.Exists filename then
        try
            File.ReadAllBytes filename
        with
        | ex -> [| |]
    else
        [| |]


let echoUploadFile
    fsDir conn (metadata:MetadataTypes<'p>) dbLoggero
    setter postCreateo (x:ReqRep) =
    let req = x.req
    if req.path.Length = 1 then
        if req.path[0] = "upload" then
            
            let id = Interlocked.Increment metadata.id

            let owner = 0L
            let caption = 
                if req.headers.ContainsKey "Filename" then
                    req.headers["Filename"].Trim()
                else
                    ""
            let suffix = 
                let index = caption.LastIndexOf "."
                if index > 0 then
                    let s = caption.Substring (index + 1)
                    if s.Length <= 4 then
                        s.ToLower()
                    else
                        ""
                else
                    ""
            let desc = 
                if req.headers.ContainsKey "Desc" then
                    req.headers["Desc"].Trim()
                else
                    ""

            let suc =
                try
                    let filename = buildfilename fsDir id suffix
                    System.IO.File.WriteAllBytes(filename,req.body)
                    if System.IO.File.Exists filename then

                        let p = metadata.empty__p()
                        setter p (owner,caption,suffix,desc,req.body.Length)
    
                        let pretx = None |> opctx__pretx

                        let rcd = 
                            p
                            |> id__CreateTx id pretx metadata

                        if pretx |> loggedPipeline dbLoggero "BizLogics.Db" conn then
                            handlero postCreateo rcd
                            true
                        else
                            false
                    else
                        false
                with
                | ex -> false

            Suc x
        else 
            Fail((),x)
    else
        Fail((),x)

let echoDownloadFile 
    fsDir (metadata:MetadataTypes<'p>)
    rcd__suffix
    (x:ReqRep) =
    let req = x.req
    if req.path.Length = 2 then
        if req.path[0] = "file" then
            match 
                req.path[1]
                |> regex_match (str__regex "\d+")
                |> parse_int64
                |> metadata.id__rcdo with
            | Some file ->
                let suffix = rcd__suffix file
                try 
                    if suffix = "mp3" then
                        ()
                    let filename = buildfilename fsDir  file.ID suffix
                    x.rep <-

                        let mime = ext__mime ("." + suffix)
                        if File.Exists filename then
                            File.ReadAllBytes filename
                        else
                            [| |]
                        |> bin__StandardResponse mime
                        |> Some
                with
                | ex -> ()
            | None -> ()

            Suc x
        else 
            Fail((),x)
    else
        Fail((),x)

let w = 100
let h = 100

let checkFileThumbnail 
    conn fsDir metadata dbLoggero
    rcd__items setterSize setterThumbnail
    file = 

    let (thumbnail:byte[]),suffix,size = rcd__items file

    let allowThumbnail = 
        if thumbnail.Length = 0 then
            match suffix with
            | "jpg" | "jpeg" | "png" -> true
            | _ -> false
        else
            false

    if size = 0L || allowThumbnail then
        let bin = file__bin fsDir file.ID suffix
        bin.Length 
        |> int64 
        |> setterSize file 
        if allowThumbnail then
            try
                bin 
                |> Util.SixLaborsImageSharp.generateThumbnail (w,h)
                |> setterThumbnail file
            with
            | ex -> ()

        UtilWebServer.Db.update "BizLogics.Db.checkFileThumbnail" 
            conn metadata dbLoggero (file.ID,file.p)
        |> ignore

    file
