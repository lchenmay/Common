module UtilKestrel.Runtime

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

open UtilKestrel.Types
open UtilKestrel.Common
open UtilKestrel.Constants
open UtilKestrel.File

let empty__Runtime<'User,'SessionData,'HostData,'RuntimeData>
    projectCode
    (host:Host<'HostData>)
    (data:'RuntimeData):RuntimeTemplate<'User,'SessionData,'RuntimeData,'HostData> =
    {
        host = host
        data = data 
        langs = [| en |]
        users = createModDictInt64 4
        sessions = createModDictStr 4
        output = output
        //listener = 
        //    createLisener output 
        //        (fileService host.fsDir host.req__vueDeployDir) host.port
        projectCode = projectCode }

