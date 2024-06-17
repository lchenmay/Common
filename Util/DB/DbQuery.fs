




module Util.DbQuery

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

open Util.Db

type Ctx = {
    mutable conn:string;
    mutable sqlconno:SqlConnection option;
    sql:Sql;
    mutable fieldcount:int;
    mutable lines:List<Object[]>;
    mutable line:Object[] option }

type DbQueryError =
| Ex of exn
| Zero
| OverOne

let connect x =
    try
        x.sqlconno <- Some(new SqlConnection(x.conn))
        Suc x
    with ex ->
        System.Console.WriteLine $"-------------"
        System.Console.WriteLine $"Database.connect: {ex}"
        Fail(ex,x)

let execNoneQuery(sql:string)(x:Ctx) =
    try
        x.sqlconno.Value.Open()
        (new SqlCommand(sql, x.sqlconno.Value)).ExecuteNonQuery() |> ignore
        x.sqlconno.Value.Close()
        Suc(x)
    with ex ->
        x.sqlconno.Value.Close()
        System.Console.WriteLine $"-------------"
        System.Console.WriteLine $"ex: {ex},sql:{sql}"
        Fail(ex,x)

let noneQuery(conn)(sql:string) =
    use cw = new CodeWrapper("DB.noneQuery")

    Suc{conn=conn;sqlconno=None;sql={ text = sql; ps = [||] };fieldcount=0;lines=new ResizeArray<Object[]>();line=None}
    |> bind(connect)
    |> bind(execNoneQuery(sql))

let line_handler handler (x:Ctx) =
    use cw = new Util.Perf.CodeWrapper("db.line_handler")
    let mutable sql = ""
    let uncommited = "SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;"
    try
            
        x.sqlconno.Value.Open()
        //(new SqlCommand(uncommited, x.sqlconno.Value)).ExecuteNonQuery()
        let sqlcmd = new SqlCommand( uncommited + x.sql.text, x.sqlconno.Value)
        sql <- x.sql.text
            
        x.sql.ps |> Array.iter(fun p -> sqlcmd.Parameters.Add(p) |> ignore)

        let sdr = sqlcmd.ExecuteReader()
        x.fieldcount <- sdr.FieldCount

        let mutable keep = true
        while (keep && sdr.Read()) do
            let array = Array.zeroCreate(x.fieldcount)
            sdr.GetValues(array) |> ignore
            if(handler(array,x)=false)then
                keep <- false

        sdr.Close()

        if(x.sqlconno.Value.State = ConnectionState.Open) then
            x.sqlconno.Value.Close()

        Suc(x)

    with ex ->
        //this is important close connection to realse connection to pool
        if(x.sqlconno.Value.State = ConnectionState.Open) then
            x.sqlconno.Value.Close()
        System.Console.WriteLine $"-------------"
        System.Console.WriteLine $"Database.line_handler: sql: {sql}"
        System.Console.WriteLine $"Database.line_handler: {ex}"
        Fail(ex,x)

let read(x:Ctx) = 
    x
    |> line_handler(fun(array,x) -> 
        x.lines.Add array
        true)

let multiline_handle(conn,handler) sql =
    use cw = new CodeWrapper("DB.multiline_handle")

    let r0 = Suc{conn=conn;sqlconno=None;sql=sql;fieldcount=0;lines=new ResizeArray<Object[]>();line=None}
    let r1 = bind(connect)(r0)
    let r2 = bind(line_handler(handler))(r1)

    r2

let multiline_query conn sql =
    use cw = new CodeWrapper("DB.multiline_query")

    Suc{conn=conn;sqlconno=None;sql=sql;fieldcount=0;lines=new ResizeArray<Object[]>();line=None}
    |> bind connect
    |> bind read

let singleline_query conn sql =

    let r0 = Suc{conn=conn;sqlconno=None;sql=sql;fieldcount=0;lines=new ResizeArray<Object[]>();line=None}
    let r1 = bind connect r0
    let r2 = bind read r1

    match r2 with
    | Suc x ->
        if x.lines.Count = 1 then
            x.line <- Some(x.lines.[0])
            Suc x
        else if x.lines.Count = 0 then
            Fail(Zero,x)
        else
            Fail(OverOne,x)
    | Fail(exn,x) -> Fail(Ex exn,x)

let singlevalue_query conn sql =
    match singleline_query conn sql with
    | Suc x ->
        let v = x.line.Value.[0]
        if v.GetType() <> typeof<System.DBNull> then
            Some(x.line.Value.[0])
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


