module UtilWebServer.SSR

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
open Util.HttpServer
open Util.Zmq

open UtilWebServer.DbLogger

let vueIndex file = 

    let html = 
        file
        |> Util.FileSys.try_read_string
        |> snd

    let head = 
        html
        |> find ("</title>","</head>")

    let body = 
        html
        |> find ("</head>","</html>")

    html,head,body

let render 
    (html,head,body)
    (title,desc,img,url,noscript) = 

    let a = 
        """
<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=no">
        """
        |> Encoding.UTF8.GetBytes
    
    let b = 
        [|  "<meta name=\"description\" content=\"" + desc + "\">"
            "<meta property=\"og:title\" content=\"" + title + "\">"
            "<meta property=\"og:description\" content=\"" + desc + "\">"
            "<meta property=\"og:type\" content=\"website\">"
            "<meta property=\"og:url\" content=\"" + url + "\">"
            "<meta property=\"og:image\" content=\"" + img + "\">"
            "<title>" + title + "</title>" |]
        |> String.Concat
        |> Encoding.UTF8.GetBytes

    let c = 
        [|  head
            "</head>"
            body
            "</html>" |]
        |> String.Concat
        |> Encoding.UTF8.GetBytes

    [|  a
        b
        c   |]
    |> Array.concat
