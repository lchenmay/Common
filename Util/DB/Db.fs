
module Util.Db

open LanguagePrimitives

open System
open System.Collections.Generic
open System.IO
open System.Data
open System.Data.SqlClient
open System.Text

open Util.Perf
open Util.Cat
open Util.Collection
open Util.OS
open Util.Bin

let SqlParameter__bin (bb:BytesBuilder) (sp:SqlParameter) = 
    sp.ParameterName |> str__bin bb
    sp.DbType |> EnumToValue |> int32__bin bb
    match sp.DbType with
    | System.Data.DbType.Int32 -> sp.Value :?> int32 |> int32__bin bb
    | System.Data.DbType.Int64 -> sp.Value :?> int64 |> int64__bin bb
    | System.Data.DbType.String -> sp.Value |> string |> str__bin bb
    | System.Data.DbType.Binary -> sp.Value :?> byte[] |> bb.append
    | _ -> ()

let bin__SqlParameter bi =
    let sp = new SqlParameter()
    sp.ParameterName <- bin__str bi
    match bin__int32 bi |> EnumOfValue with
    | System.Data.DbType.Int32 -> sp.Value <- bin__int32 bi
    | System.Data.DbType.Int64 -> sp.Value <- bin__int64 bi
    | System.Data.DbType.String -> sp.Value <- bin__str bi
    | System.Data.DbType.Binary -> sp.Value <- bin__int32 bi
    | _ -> ()
    sp

let sps__bin (bb:BytesBuilder) (sps:SqlParameter[]) = 
    array__bin SqlParameter__bin bb sps

let bin__sps (bin:byte[],index:Ref<int>) =
    bin__array bin__SqlParameter (bin,index)

type Sql = { text:string; ps:SqlParameter[] }

let sql__bin (bb:BytesBuilder) (sql:Sql) = 
    sql.text |> str__bin bb
    sql.ps |> sps__bin bb

let bin__sql bi =
    let text = bin__str bi
    let sps = bin__sps bi
    { text = text; ps = sps }

let sqls__bin (bb:BytesBuilder) (sqls:Sql[]) = 
    array__bin sql__bin bb sqls

let bin__sqls (bin:byte[],index:Ref<int>) =
    bin__array bin__sql (bin,index)

let build(ps)(text) = { text = text; ps = ps}
let str__sql(s) = { text = s; ps = [||]}
let str__sqls(s) = [|str__sql(s)|]

let sql__string(sql) =
    let ps =
        sql.ps
        |> Array.map(fun p -> p.ParameterName+"="+p.Value.ToString())
        |> String.concat(",")
    sql.text+" {"+ps+"}"

let validate_connect conn =
    //match  sb.Append("IF NOT EXISTS(SELECT * FROM sysobjects WHERE [name]='" + table + "' AND xtype='U')" + Util.Text.crlf) |> ignore
    //""
    try
        let c = new SqlConnection(conn)
        c.Open()
        if(c.State = ConnectionState.Open) then
            c.Close()
        true
    with ex ->
        System.Console.WriteLine $"-------------"
        System.Console.WriteLine $"Database.validate_connect: {ex}"
        false


type BackupType =
| Full
| Diff
| Log

let backup 
    backupType 
    (instName, dbName, backupSetName, file) =

    match
        runInProcess false (fun p ->
            let cmd = 

                let mutable backuptype =
                    match backupType with
                    | BackupType.Diff ->
                        " -Incremental"
                    | BackupType.Log ->
                        " -Incremental -BackupAction Log -LogTruncationType Truncate"
                    | _ -> ""

                [|  "Backup-SqlDatabase -ServerInstance \""
                    instName
                    "\" -Database \""
                    dbName
                    "\" -BackupSetName \""
                    backupSetName
                    "\" -CompressionOption On -BackupFile \""
                    file
                    "\""
                    backuptype |]
                |> String.Concat

            if File.Exists file then
                File.Delete file
            "\r\n\r\nStarting database backup, Command: " |> Console.WriteLine
            cmd |> Console.WriteLine
            [|  "powershell"
                "Import-Module SqlServer"
                cmd
                "exit" |]
            |> Array.iter(outcmd p))
    with
    | "" -> "Command success"
    | _ as err -> err