
module Util.DbTx

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
    sqls:List<Sql>;
    mutable count: int[] }

type DbTxError = { 
    exno:exn option; 
    sqlo:Sql option }

let exnSqlo__fail x exn sqlo = 
    let dte = {
        exno = Some exn
        sqlo = sqlo}

    (dte,x)
    |> Fail

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
        exnSqlo__fail x ex None


let commitc (suc,fail) x =
    let mutable current = None

    use cw = new CodeWrapper("Database.commitc")
    let conn = x.conno.Value

    try
        try
            connOpen conn

            let tx =
                match conn with
                | Conn.SqlServer conn -> 
                    conn.BeginTransaction() 
                    |> SqlTx.SqlServer
                | Conn.PostgreSql conn -> 
                    conn.BeginTransaction() 
                    |> SqlTx.PostgreSql

            let lines = 
                x.sqls.ToArray()
                |> Array.map(sql__sqlcmd conn)

            let affected = 
                lines
                |> Array.map(fun line ->
                    current <- Some line
                    match line,tx with
                    | SqlCmd.SqlServer sc,SqlTx.SqlServer tx ->
                        sc.Transaction <- tx
                        sc.ExecuteNonQuery()
                    | SqlCmd.PostgreSql sc,SqlTx.PostgreSql tx ->
                        sc.Transaction <- tx
                        sc.ExecuteNonQuery())

            use cw = new CodeWrapper("Database.commitc/Commit")

            match tx with
            | SqlTx.SqlServer tx ->
                tx.Commit()
            | SqlTx.PostgreSql tx ->
                tx.Commit()

            connClose conn

            x.count <- affected

        finally
            connClose conn

        suc x

    with ex ->
        let sqls = 
            x.sqls.ToArray()
            |> Array.map(fun i -> i.text)
            |> String.concat ";\n"

        exnSqlo__fail x ex None


let tx connStr output (lines:Sql[]) =
    use cw = new CodeWrapper("Db.tx")

    {   connStr = connStr
        conno = None
        sqls = new List<Sql>(lines)
        count= [||] }
    |> Suc
    |> bind connect
    |> bind (commitc (Suc,Fail))

let txOne conn output line = tx conn output [| str__sql line |]

let txOneSql conn output sql = tx conn output [| sql |]

// transactions with continuation
let txc(connStr,suc,fail) lines =
    use cw = new CodeWrapper("Db.txc")

    {   connStr = connStr
        conno = None
        sqls = lines
        count = [||] }
    |> Suc
    |> bind connect
    |> bind(commitc (suc,fail))

type PreTx<'context> = 
    {
        ctx:'context;
        mutable dt: DateTime;
        sucs:List<Ctx->unit>;
        fails:List<DbTxError*Ctx->unit>;
        sqls:List<Sql> }

    member this.clear() = 
        this.sucs.Clear()
        this.fails.Clear()
        this.sqls.Clear()

    member this.append(another) =
        this.sucs.AddRange(another.sucs)
        this.fails.AddRange(another.fails)
        this.sqls.AddRange(another.sqls)

let opctx__pretx(ctx:'context) = 
    {
        ctx = ctx;
        dt = DateTime.UtcNow;
        sucs = new List<Ctx -> unit>();
        fails = new List<DbTxError * Ctx -> unit>();
        sqls = new List<Sql>() }

let pipeline conn pretx = 
    pretx.sqls
    |> txc(
        conn,
        (fun ctx -> 
            pretx.sucs.ToArray()
            |> Array.iter(fun handler -> handler ctx)
            Suc ctx),
        (fun (dte,ctx) -> 
            pretx.fails.ToArray()
            |> Array.rev
            |> Array.iter(fun handler -> handler (dte,ctx))
            Fail(dte,ctx)))
