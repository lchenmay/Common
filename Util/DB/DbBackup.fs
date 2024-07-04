module Util.DbBackup

open LanguagePrimitives

open System
open System.Data
open System.Collections.Generic
open System.IO
open System.Data
open System.Data.SqlClient
open System.Text

open Npgsql

open Util.Perf
open Util.Cat
open Util.Collection
open Util.OS
open Util.Bin
open Util.Db


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