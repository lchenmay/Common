﻿module UtilWebServer.SSR

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

open UtilWebServer.DbLogger
open UtilWebServer.Common

let r1 = str__regex @"(?<=src=\x22/js/index\.)[\w-]+(?=\.js\x22)"
let r2 = str__regex @"(?<=href=\x22/as/index\.)[\w-]+(?=\.css\x22)"

type SsrPage = {
title: string
desc: string
image: string
url: string
noscript: string }

let vueIndexFile__hashes file = 

    let html = 
        file
        |> Util.FileSys.try_read_string
        |> snd

    let hash1 = html |> regex_match r1
    let hash2 = html |> regex_match r2

    hash1,hash2

let render 
    (hash1,hash2) plugin
    ssrPage = 
        let imports = 
            """
  <script type="importmap">
    {
      "imports": {
        "@antv/x6": "https://cdn.jsdelivr.net/npm/@antv/x6@2.18.1/+esm"
      }
    }
  </script>
        """
        
        $"""
<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no">
  <meta property="og:title" content="{ssrPage.title}" />
  <meta property="og:description" content="{ssrPage.desc}" />
  <meta property="og:type" content="website" />
  <meta property="og:url" content="{ssrPage.url}" />
  <meta property="og:image" content="{ssrPage.image}" />
  <title>{ssrPage.title}</title>
  <meta name="description" content="{ssrPage.desc}" />
  <link rel="icon" type="image/svg+xml" href="/favicon.ico" />
  <script src="/panel.js"></script>
  {imports}
  <script type="module" crossorigin src="/js/index.{hash1}.js"></script>
  <link rel="stylesheet" crossorigin href="/as/index.{hash2}.css" />
  {plugin}
</head>

<body class="overflow-x-hidden">
  <noscript>{ssrPage.noscript}</noscript>
  <div id="app"></div>
</body>

</html>
        """ 
        |> Encoding.UTF8.GetBytes


let hpattern (pattern:string) h x = 
    let req = x.req
    if req.pathline.StartsWith pattern then
        x.rep <-
            h req
            |> Some
        Suc x
    else
        Fail((),x)

let hapi echoApiHandler branch x = 
    let req = x.req
    if req.path.Length = 3 then
        if req.path[0] = "api" then
            x.rep <-
                echoApiHandler branch req
                |> Some
            Suc x
        else
            Fail((),x)
    else
        Fail((),x)


let hHomepage render x = 
    let req = x.req
    match req.pathline with
    | ""
    | "/" ->
        x.rep <-
            render()
            |> bin__StandardResponse "text/html"
            |> Some
        Suc x
    | _ -> Fail((),x)

let homepage ssr vueDeployDir plugin =
    hHomepage (fun _ -> ssr |> render (vueIndexFile__hashes(vueDeployDir + "/index.html")) plugin)

let hSitemap x__items (x:ReqRep) =
    if x.req.pathline = "/sitemap.xml" then
        x.rep <-
            
            let w = empty__TextBlockWriter()

            [|  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"  |]
            |> w.multiLine

            x__items x
            |> Array.map(fun (loc) -> 
            
                [|  "<url>"
                    "<loc>" + loc + "</loc>"
                    //"<lastmod>2025-01-01</lastmod>"
                    //"<changefreq>daily</changefreq>"
                    //"<priority>0.8</priority>" 
                    "</url>"  |]
                |> String.Concat)
            |> w.multiLine

            "</urlset>" |> w.newline

            w.text()
            |> System.Text.Encoding.UTF8.GetBytes
            |> bin__StandardResponse "text/xml"
            |> Some
        Suc x
    else
        Fail((),x)
