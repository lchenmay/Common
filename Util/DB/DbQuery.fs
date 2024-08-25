module Util.DbQuery

open LanguagePrimitives

open System
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

type Ctx = {
    mutable connStr:string
    mutable conno:Conn option
    sql:Sql;
    mutable fieldcount:int
    mutable lines:List<Object[]>
    mutable line:Object[] option }

type DbQueryError =
| Ex of exn
| Zero
| OverOne

let connect x =
    try
        x.conno <- 
            match rdbms with
            | Rdbms.SqlServer -> 
                new SqlConnection(x.connStr)
                |> Conn.SqlServer
                |> Some
            | Rdbms.PostgreSql -> 
                new NpgsqlConnection(x.connStr)
                |> Conn.PostgreSql
                |> Some
            | _ -> None
        Suc x
    with ex -> 
        Fail(ex,x)

let queryExe conn sql = 
    match conn with
    | Conn.SqlServer conn ->
        (new SqlCommand(sql, conn)).ExecuteNonQuery()
        |> ignore
    | Conn.PostgreSql conn ->
        ()

let execNoneQuery sql x =
    let conn = x.conno.Value
    try
        connOpen conn
        queryExe conn sql
        connClose conn
        Suc x
    with ex -> 
        connClose conn
        Fail(ex,x)

let noneQuery conn sql =
    use cw = new CodeWrapper("DB.noneQuery")

    {   connStr = conn
        conno = None
        sql = { text = sql; ps = [||] }
        fieldcount = 0
        lines = new List<Object[]>()
        line = None}
    |> Suc
    |> bind connect
    |> bind (execNoneQuery sql)

let line_handler handler x =
    use cw = new CodeWrapper("db.line_handler")

    let conn = x.conno.Value
    let mutable sql = ""
    try
        connOpen conn
        //(new SqlCommand(uncommited, x.sqlconno.Value)).ExecuteNonQuery()
        let sc = sql__sqlcmd conn x.sql
        sql <- x.sql.text

        match sc with
        | SqlCmd.SqlServer sc -> 
            use reader = sc.ExecuteReader()
            x.fieldcount <- reader.FieldCount

            let mutable keep = true
            while keep && reader.Read() do
                let array = Array.zeroCreate x.fieldcount
                reader.GetValues array |> ignore
                if handler(array,x) = false then
                    keep <- false

            reader.Close()

        | SqlCmd.PostgreSql sc -> 

            use reader = sc.ExecuteReader()

            let mutable keep = true
            while keep && reader.Read() do
                let array = Array.zeroCreate reader.FieldCount
                let id = reader.GetInt64(0)
                reader.GetValues array |> ignore
                if handler(array,x) = false then
                    keep <- false

            reader.Close()

        connClose conn

        Suc x

    with ex ->
        connClose conn
        System.Console.WriteLine $"-------------"
        System.Console.WriteLine $"Database.line_handler: sql: {sql}"
        System.Console.WriteLine $"Database.line_handler: {ex}"
        Fail(ex,x)

let read = 
    line_handler(fun(array,x) -> 
        x.lines.Add array
        true)

let multiline_handle(conn,handler) sql =
    use cw = new CodeWrapper("DB.multiline_handle")

    let r0 = Suc{connStr=conn;conno=None;sql=sql;fieldcount=0;lines=new ResizeArray<Object[]>();line=None}
    let r1 = bind connect r0
    let r2 = bind (line_handler handler) r1

    r2

let multiline_query conn sql =
    use cw = new CodeWrapper("DB.multiline_query")

    Suc{connStr=conn;conno=None;sql=sql;fieldcount=0;lines=new ResizeArray<Object[]>();line=None}
    |> bind connect
    |> bind read

let singleline_query conn sql =

    let r0 = Suc{connStr=conn;conno=None;sql=sql;fieldcount=0;lines=new ResizeArray<Object[]>();line=None}
    let r1 = bind connect r0
    let r2 = bind read r1

    match r2 with
    | Suc x ->
        if x.lines.Count = 1 then
            x.line <- Some x.lines[0]
            Suc x
        else if x.lines.Count = 0 then
            Fail(Zero,x)
        else
            Fail(OverOne,x)
    | Fail(exn,x) -> Fail(Ex exn,x)

let singlevalue_query conn sql =
    match singleline_query conn sql with
    | Suc x ->
        let vs = x.line.Value
        if vs.Length = 1 then
            let v = vs[0]
            if v.GetType() <> typeof<System.DBNull> then
                Some(x.line.Value[0])
            else
                None
        else
            None
    | _ -> None

let loadall (conn,db__rcd,logging) query_sql =
    
    match multiline_query conn query_sql with
    | Suc x -> 
        x.lines.ToArray()
        |> Array.map db__rcd
        |> Some
    | Fail(exn,x) ->
        logging(exn,x.sql)
        None


