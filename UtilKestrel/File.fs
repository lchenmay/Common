module UtilKestrel.File

open System
open System.IO
open System.Threading
open System.Collections.Generic

open Util.Cat
open Util.ADT
open Util.Bin
open Util.Db
open Util.DbTx
open Util.Collection
open Util.CollectionModDict
open Util.Orm
open Util.Http
open Util.HttpServer
open Util.Text
open Util.Json

open UtilKestrel.Types
open UtilKestrel.Ctx
open UtilKestrel.Db
open UtilKestrel.Api

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



let fileService fsDir req__vueDeployDir req = 

    let mutable file = 
        [|  [| fsDir |]
            req.path |]
        |> Array.concat
        |> Path.Combine

    if File.Exists file = false then
        file <- 
            [|  [| req__vueDeployDir req |]
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

        UtilKestrel.Db.update "BizLogics.Db.checkFileThumbnail" 
            conn metadata dbLoggero (file.ID,file.p)
        |> ignore

    file
