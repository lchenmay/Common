module UtilKestrel.Common

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Collections.Concurrent
open System.Text
open System.IO
open System.Diagnostics

open Util.Cat
open Util.CollectionModDict
open Util.Console
open Util.Json
open Util.Http

let since = DateTime.UtcNow

type StatefullConsole = {
max: int ref
mutable autosaveo: string option
history: System.Collections.Generic.List<DateTime * string>
output: string -> unit }

Console.InputEncoding <- Encoding.Unicode
Console.OutputEncoding <- Encoding.Unicode

let sc = 
  
  let maxr = ref 10000
  let history = new System.Collections.Generic.List<DateTime * string>()

  { max = maxr
    autosaveo = None
    history = history
    output = (fun s ->
      s |> prompt since |> Console.WriteLine

      lock history (fun _ -> 
        (DateTime.UtcNow,s) |> history.Add
        if history.Count > maxr.Value then
          history.RemoveAt 0)) }

let loadAll() = sc.history.ToArray()

(fun _ -> 
  match sc.autosaveo with
  | Some f -> 
    loadAll()
    |> Array.map(fun (a,b) -> a.ToLongTimeString() + ": " + b)
    |> Util.FileSys.try_write_lines f 
    |> ignore
  | None -> ())
|> Util.Concurrent.asyncCyclerInterval 1000
