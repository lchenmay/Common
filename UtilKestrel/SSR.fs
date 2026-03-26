module UtilKestrel.SSR

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

open UtilKestrel.Types
open UtilKestrel.Ctx
open UtilKestrel.Common
open UtilKestrel.Server

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


let hHomepage (langs:string[]) (pages:string[]) render (x:EchoCtx<'Runtime,'Session,'Error>) = 
    // pathline = /?session=E3500820E03FC50ED65E89C60132DBDADE30D94557BDB37C7A8BA6F675B95A35&id=1003
    let path = x.Struct.httpx.Request.Path.ToString()

    let mutable hit = 
        path = ""
        || path = "/"
        || path.StartsWith "/?"

    if not hit then
        hit <- 
            (pages
            |> Array.tryFind(fun i -> 
                path.StartsWith i)).IsSome

    if not hit then
        hit <- 
            (langs
            |> Array.tryFind(fun i -> "/" + i + "/" |> path.StartsWith)).IsSome

    if hit then
        x.Struct.contentType <- "text/html"
        x.Struct.rep <- 
            render()
            |> bin__StandardResponse "text/html"
        Suc x
    else
        Fail((),x)

let homepage langs pages ssr vueDeployDir plugin =
    hHomepage langs pages (fun _ -> 
        ssr 
        |> render (vueIndexFile__hashes(vueDeployDir + "/index.html")) plugin)


let hSsrSinglePage paramName plugin vueDeployDir 
    tryFinder v__SsrPage
    (x:EchoCtx<'Runtime,'Session,'Error>) = 
    let path = x.PathSegments
    if path.Length = 2 then
        if path[0] = paramName then
            let id = path[1] |> parse_int64
            match tryFinder id with
            | Some v -> 
                x.Struct.rep <-
                    v__SsrPage v
                    |> render (vueIndexFile__hashes(vueDeployDir + "/index.html")) plugin
                    |> bin__StandardResponse "text/html"
                Suc x
            | None -> Fail((),x)
        else
            Fail((),x)
    else
        Fail((),x)

let hSEO x__items (adsTxt:string) (x:EchoCtx<'Runtime,'Session,'Error>) =
    if x.Struct.httpx.Request.Path.ToString() = "/sitemap.xml" then
        x.Struct.rep <-
            
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
        Suc x
    else if x.Struct.httpx.Request.Path.ToString() = "/robots.txt" then
        x.Struct.rep <-
            [|  "User-agent: *"
                "Disallow:" |]
            |> String.concat crlf
            |> System.Text.Encoding.UTF8.GetBytes
            |> bin__StandardResponse "text/xml"
        Suc x
    else if x.Struct.httpx.Request.Path.ToString() = "/Ads.txt" then
        x.Struct.rep <-
            adsTxt
            |> System.Text.Encoding.UTF8.GetBytes
            |> bin__StandardResponse "text/xml"
        Suc x
    else if x.Struct.httpx.Request.Path.ToString() = "/favicon.ico" then
        x.Struct.rep <-
            [|  "User-agent: *"
                "Disallow:" |]
            |> String.concat crlf
            |> System.Text.Encoding.UTF8.GetBytes
            |> bin__StandardResponse "text/xml"
        Suc x
    else
        Fail((),x)
