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
open Util.OS
open Util.DbQuery
open Util.DbTx
open Util.Orm

open UtilKestrel.Types
open UtilKestrel.Common
open UtilKestrel.Constants
open UtilKestrel.File

let empty__Runtime<'User,'SessionData,'HostData,'RuntimeData>
    projectCode
    (hostdata: 'HostData)
    (data:'RuntimeData):RuntimeTemplate<'User,'SessionData,'RuntimeData,'HostData> =
    {
        since = DateTime.UtcNow
        debugger = Debugger.Unknown
        inited = false
        host = {
            data = hostdata
            port = 1723
            rdbms = Util.Db.Rdbms.PostgreSql
            conn = ""
            fsroot = ""
            url = ""
            cert = ""
            certpwd = ""

            deploy = {
                credential = (None,"root","")
                postgresPwd = ""
                gitName = ""
                gitEmail = "@.com" }

            updateDatabase = true

            disk = "C:/" }

        data = data 
        langs = [| en |]
        users = createModDictInt64 4
        sessions = createModDictStr 4
        output = sc.output
        projectCode = projectCode }

