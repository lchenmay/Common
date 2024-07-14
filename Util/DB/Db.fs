
module Util.Db

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

type Rdbms = 
| SqlServer
| PostgreSql

let mutable rdbms = Rdbms.SqlServer

type Conn = 
| SqlServer of SqlConnection
| PostgreSql of NpgsqlConnection

type DbLog = {
location: string
content: string
sql: string }

type SqlParam = 
| SqlServer of SqlParameter
| PostgreSql of NpgsqlParameter

let kvp__sqlparam (k,v:obj) = 
    match rdbms with
    | Rdbms.SqlServer -> 
        new SqlParameter(k,v)
        |> SqlParam.SqlServer
    | Rdbms.PostgreSql -> 
        new NpgsqlParameter(k,v)
        |> SqlParam.PostgreSql

let sp__data sp = 
    match sp with
    | SqlParam.SqlServer sp -> sp.ParameterName,sp.DbType,sp.Value
    | SqlParam.PostgreSql sp -> sp.ParameterName,sp.DbType,sp.Value

type Sql = { 
text:string
ps:SqlParam[] }

type SqlCmd = 
| SqlServer of SqlCommand
| PostgreSql of NpgsqlCommand

type SqlTx = 
| SqlServer of SqlTransaction
| PostgreSql of NpgsqlTransaction

let uncommited = "SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;"

let sql__sqlcmd conn sql = 

    match rdbms,conn with
    | Rdbms.SqlServer,Conn.SqlServer conn -> 
        let sc = new SqlCommand( uncommited + sql.text, conn)

        sql.ps 
        |> Array.iter(fun p -> 
            match p with
            | SqlParam.SqlServer pp -> 
                sc.Parameters.Add pp |> ignore
            | _ -> ())

        sc
        |> SqlCmd.SqlServer
    | Rdbms.PostgreSql,Conn.PostgreSql conn -> 
        let sc = new NpgsqlCommand(sql.text, conn)

        sql.ps 
        |> Array.iter(fun p -> 
            match p with
            | SqlParam.PostgreSql pp -> 
                sc.Parameters.Add pp |> ignore
            | _ -> ())

        sc
        |> SqlCmd.PostgreSql

let SqlParam__bin bb sp = 

    let name,dbtype,v = sp |> sp__data 
    
    name |> str__bin bb
    dbtype |> EnumToValue |> int32__bin bb
    match dbtype with
    | DbType.Int32 -> v:?> int32 |> int32__bin bb
    | DbType.Int64 -> v :?> int64 |> int64__bin bb
    | DbType.String -> v |> string |> str__bin bb
    | DbType.Binary -> v :?> byte[] |> bb.append
    | _ -> ()
        

let bin__SqlParam bi =
    match rdbms with
    | Rdbms.SqlServer -> 
        let sp = new SqlParameter()
        sp.ParameterName <- bin__str bi
        match bin__int32 bi |> EnumOfValue with
        | DbType.Int32 -> sp.Value <- bin__int32 bi
        | DbType.Int64 -> sp.Value <- bin__int64 bi
        | DbType.String -> sp.Value <- bin__str bi
        | DbType.Binary -> sp.Value <- bin__int32 bi
        | _ -> ()
        sp |> SqlParam.SqlServer
    | Rdbms.PostgreSql -> 
        let sp = new NpgsqlParameter()
        sp.ParameterName <- bin__str bi
        match bin__int32 bi |> EnumOfValue with
        | DbType.Int32 -> sp.Value <- bin__int32 bi
        | DbType.Int64 -> sp.Value <- bin__int64 bi
        | DbType.String -> sp.Value <- bin__str bi
        | DbType.Binary -> sp.Value <- bin__int32 bi
        | _ -> ()
        sp |> SqlParam.PostgreSql

let sps__bin bb sps = 
    array__bin SqlParam__bin bb sps

let bin__sps = bin__array bin__SqlParam

let sql__bin bb sql = 
    sql.text |> str__bin bb
    sql.ps |> sps__bin bb

let bin__sql bi =
    let text = bin__str bi
    let sps = bin__sps bi
    { text = text; ps = sps }

let build ps text = { text = text; ps = ps}
let str__sql s = { text = s; ps = [||]}
let str__sqls s = [| str__sql s |]

let sql__string sql =
    let ps =
        sql.ps
        |> Array.map(fun p -> 
            let name,dbtype,v = p |> sp__data 
            name + "=" + v.ToString())
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

let connOpen conn = 
    match conn with
    | Conn.SqlServer conn -> conn.Open()
    | Conn.PostgreSql conn -> conn.Open()

let connClose conn = 
    match conn with
    | Conn.SqlServer conn -> 
        if conn.State = ConnectionState.Open then
            conn.Close()
    | Conn.PostgreSql conn -> conn.Close()
