
module Util.DbTx

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

let mutable tableRLOG = ""

type Ctx = {
    mutable conn:string;
    mutable sqlconno:SqlConnection option;
    sqls:ResizeArray<Sql>;
    mutable count: int[] }

type DbTxError = { 
    exno:exn option; 
    sqlo:Sql option }

let connect x =
    try
        x.sqlconno <- Some(new SqlConnection(x.conn))
        Suc x
    with ex ->
        Fail({exno=Some(ex);sqlo=None},x)

let commitc (suc,fail)(x:Ctx) =
    let mutable current = None
    let mutable sqltrans = None

    use cw = new CodeWrapper("Database.commitc")
    let sqlTexts = List<string>()

    try
        try
            x.sqlconno.Value.Open()

            if tableRLOG.Length > 0 then
                let text = "INSERT INTO [" + tableRLOG + "] ([SQLS]) VALUES (@SQLS)"
                let bin = 
                    let bb = new BytesBuilder()
                    x.sqls.ToArray() |> sqls__bin bb
                    bb.bytes()
                let sp = new SqlParameter("SQLS",bin)
                let sql = { text = text; ps = [| sp |]}
                x.sqls.Add(sql) |> ignore

            sqltrans <- Some(x.sqlconno.Value.BeginTransaction())

            let affected = 
                x.sqls.ToArray()
                |> Array.map(fun line ->

                use cw = new CodeWrapper("Database.commitc/line")

                sqlTexts.Add line.text
                let sqlcmd = new SqlCommand(line.text, x.sqlconno.Value)
                line.ps |> Seq.iter(fun p -> sqlcmd.Parameters.Add(p) |> ignore)

                current <- Some(line)

                sqlcmd.Connection <- x.sqlconno.Value
                sqlcmd.Transaction <- sqltrans.Value
                sqlcmd.ExecuteNonQuery())

            use cw = new CodeWrapper("Database.commitc/Commit")

            sqltrans.Value.Commit()

            if(x.sqlconno.Value.State = ConnectionState.Open) then
                x.sqlconno.Value.Close()

            x.count <- affected
            suc(x)

        finally
            if(x.sqlconno.Value.State = ConnectionState.Open) then
                x.sqlconno.Value.Close()

    with ex ->
        let sqls = sqlTexts |> Seq.toArray |> String.concat ";\n"
        ({
            exno = Some ex
            sqlo = current },x)
        |> fail

let private _suc(x) = Suc(x)
let private _fail(dte,x) = Fail(dte,x)

let tx conn output (lines:Sql[]) =
    use cw = new CodeWrapper("Db.tx")

    {   conn=conn;
        sqlconno=None;
        sqls=new ResizeArray<Sql>(lines);
        count=[||] }
    |> Suc
    |> bind connect
    |> bind (commitc (_suc,_fail))

let txOne conn output line = tx conn output [|str__sql(line)|]

let txOneSql conn output sql = tx conn output [|sql|]

// transactions with continuation
let txc(conn,suc,fail) lines =
    use cw = new CodeWrapper("Db.txc")

    {   conn = conn;
        sqlconno = None;
        sqls = lines;
        count = [||] }
    |> Suc
    |> bind connect
    |> bind(commitc (suc,fail))

type PreTx<'context> = 
    {
        ctx:'context;
        mutable dt: DateTime;
        sucs:ResizeArray<Ctx->unit>;
        fails:ResizeArray<DbTxError*Ctx->unit>;
        sqls:ResizeArray<Sql> }

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
        sucs = new ResizeArray<Ctx->unit>();
        fails = new ResizeArray<DbTxError*Ctx->unit>();
        sqls = new ResizeArray<Sql>() }

let pipeline conn pretx = 
    pretx.sqls
    |> txc(
        conn,
        (fun ctx -> 
            pretx.sucs 
            |> Seq.iter(fun handler -> handler(ctx))
            Suc(ctx)),
        (fun (dte,ctx) -> 
            pretx.fails 
            |> Seq.rev
            |> Seq.iter(fun handler -> handler(dte,ctx))
            Fail(dte,ctx)))
