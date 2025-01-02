module UtilWebServer.Runtime

open System
open System.Text
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading
open System.Net
open System.Net.Sockets

open Util.Cat
open Util.Text
open Util.CollectionModDict
open Util.Db
open Util.DbQuery
open Util.DbTx
open Util.Orm

open UtilWebServer.Common
open UtilWebServer.Server.Common
open UtilWebServer.Server.Net
open UtilWebServer.Server.File

let createLisener output fileService port = 
    {
        port = port
        socket = new TcpListener(IPAddress.Any,port)
        echo = (fun _ -> None)
        h404o = None
        wsHandler = (fun _ -> None)
        fileService = fileService
        connId = ref 0L
        queue = createModDictInt64<Conn> 8
        keeps = createModDictInt64<Conn> 8 
        output = output }

let empty__Runtime<'User,'SessionData,'HostData,'RuntimeData>
    (host:Host<'HostData>)
    (data:'RuntimeData):RuntimeTemplate<'User,'SessionData,'RuntimeData,'HostData> =
    {
        host = host
        data = data 
        users = createModDictInt64 4
        sessions = createModDictStr 4
        output = output
        listener = 
            createLisener output 
                (fileService host.fsDir host.vueDeployDir) host.port }

